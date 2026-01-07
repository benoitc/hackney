cmake_minimum_required(VERSION 3.14 FATAL_ERROR)

project(HackneyQuicNIF C CXX)

# CMake policies
if(POLICY CMP0028)
  cmake_policy(SET CMP0028 NEW)
endif()
if(POLICY CMP0054)
  cmake_policy(SET CMP0054 NEW)
endif()
if(POLICY CMP0075)
  cmake_policy(SET CMP0075 NEW)
endif()
if(POLICY CMP0077)
  cmake_policy(SET CMP0077 NEW)
endif()

# CMake module path
list(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/CMake")

# Architecture detection
message(STATUS "Building for: ${CMAKE_SYSTEM_NAME} ${CMAKE_SYSTEM_PROCESSOR}")

# Standard package manager paths by platform
if(APPLE)
    if(EXISTS "/opt/homebrew")
        list(APPEND CMAKE_PREFIX_PATH "/opt/homebrew")
        list(APPEND CMAKE_INCLUDE_PATH "/opt/homebrew/include")
        list(APPEND CMAKE_LIBRARY_PATH "/opt/homebrew/lib")
    endif()
    if(EXISTS "/opt/local")
        list(APPEND CMAKE_PREFIX_PATH "/opt/local")
        list(APPEND CMAKE_INCLUDE_PATH "/opt/local/include")
        list(APPEND CMAKE_LIBRARY_PATH "/opt/local/lib")
    endif()
    if(EXISTS "/usr/local/include")
        list(APPEND CMAKE_PREFIX_PATH "/usr/local")
        list(APPEND CMAKE_INCLUDE_PATH "/usr/local/include")
        list(APPEND CMAKE_LIBRARY_PATH "/usr/local/lib")
    endif()
elseif(CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")
    if(EXISTS "/usr/local")
        list(APPEND CMAKE_PREFIX_PATH "/usr/local")
        list(APPEND CMAKE_INCLUDE_PATH "/usr/local/include")
        list(APPEND CMAKE_LIBRARY_PATH "/usr/local/lib")
    endif()
elseif(CMAKE_SYSTEM_NAME STREQUAL "NetBSD")
    if(EXISTS "/usr/pkg")
        list(APPEND CMAKE_PREFIX_PATH "/usr/pkg")
        list(APPEND CMAKE_INCLUDE_PATH "/usr/pkg/include")
        list(APPEND CMAKE_LIBRARY_PATH "/usr/pkg/lib")
    endif()
elseif(CMAKE_SYSTEM_NAME STREQUAL "OpenBSD")
    if(EXISTS "/usr/local")
        list(APPEND CMAKE_PREFIX_PATH "/usr/local")
        list(APPEND CMAKE_INCLUDE_PATH "/usr/local/include")
        list(APPEND CMAKE_LIBRARY_PATH "/usr/local/lib")
    endif()
endif()

# Output directory
set(priv_dir "${PROJECT_SOURCE_DIR}/../priv")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${priv_dir})
if(MSVC)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY_RELEASE "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}")
endif()

# C/C++ standards (BoringSSL needs C++)
set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Enable position independent code for all targets (required for shared library NIF)
# Must be set before adding any subdirectories
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# Threading
set(CMAKE_THREAD_PREFER_PTHREAD ON)
set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)

# Find zlib (required by lsquic)
find_package(ZLIB REQUIRED)
message(STATUS "Found zlib: ${ZLIB_LIBRARIES}")

#--------------------------------------------------------------------------------
# Find Erlang
#--------------------------------------------------------------------------------
include(FindErlang)

#--------------------------------------------------------------------------------
# Build BoringSSL
#--------------------------------------------------------------------------------
set(BORINGSSL_DIR "${PROJECT_SOURCE_DIR}/boringssl")
message(STATUS "Building BoringSSL from: ${BORINGSSL_DIR}")

# BoringSSL build options
set(BUILD_SHARED_LIBS OFF CACHE BOOL "" FORCE)
set(OPENSSL_NO_ASM OFF CACHE BOOL "" FORCE)
set(BUILD_TESTING OFF CACHE BOOL "" FORCE)

# GCC compatibility: define __has_feature as 0 for non-Clang compilers
# BoringSSL's generated assembly uses __has_feature(hwaddress_sanitizer) which is Clang-only
# CMake doesn't support function-style macros on the command line, so we use a header file
# that is force-included via -include flag
if(CMAKE_C_COMPILER_ID STREQUAL "GNU")
    set(GCC_COMPAT_HEADER "${PROJECT_SOURCE_DIR}/gcc_compat.h")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -include ${GCC_COMPAT_HEADER}")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -include ${GCC_COMPAT_HEADER}")
    set(CMAKE_ASM_FLAGS "${CMAKE_ASM_FLAGS} -include ${GCC_COMPAT_HEADER}")
endif()

# Add BoringSSL subdirectory
add_subdirectory(${BORINGSSL_DIR} ${CMAKE_BINARY_DIR}/boringssl EXCLUDE_FROM_ALL)

# BoringSSL include directory
set(BORINGSSL_INCLUDE "${BORINGSSL_DIR}/include")
message(STATUS "BoringSSL include: ${BORINGSSL_INCLUDE}")

#--------------------------------------------------------------------------------
# Build lsquic
#--------------------------------------------------------------------------------
set(LSQUIC_DIR "${PROJECT_SOURCE_DIR}/lsquic")
message(STATUS "Building lsquic from: ${LSQUIC_DIR}")

# lsquic build options - tell it where BoringSSL is
set(BORINGSSL_INCLUDE ${BORINGSSL_INCLUDE} CACHE PATH "" FORCE)
set(BORINGSSL_LIB_ssl ssl CACHE STRING "" FORCE)
set(BORINGSSL_LIB_crypto crypto CACHE STRING "" FORCE)
set(ZLIB_LIB ${ZLIB_LIBRARIES} CACHE STRING "" FORCE)

# Disable tests and examples
set(LSQUIC_BIN OFF CACHE BOOL "" FORCE)
set(LSQUIC_TESTS OFF CACHE BOOL "" FORCE)
set(LSQUIC_SHARED_LIB OFF CACHE BOOL "" FORCE)

# Add lsquic subdirectory
add_subdirectory(${LSQUIC_DIR} ${CMAKE_BINARY_DIR}/lsquic EXCLUDE_FROM_ALL)

#--------------------------------------------------------------------------------
# NIF target
#--------------------------------------------------------------------------------
set(NIF_TARGET hackney_quic)

add_library(${NIF_TARGET} MODULE
    ${CMAKE_CURRENT_SOURCE_DIR}/hackney_quic_nif.c
    ${CMAKE_CURRENT_SOURCE_DIR}/quic_conn.c
)

# Include directories
target_include_directories(${NIF_TARGET}
    PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${ERLANG_ERTS_INCLUDE_PATH}
    ${BORINGSSL_INCLUDE}
    ${LSQUIC_DIR}/include
)

# Compiler flags
if(MSVC)
    target_compile_options(${NIF_TARGET} PRIVATE /W4)
else()
    target_compile_options(${NIF_TARGET} PRIVATE
        -Wall -Wextra
        -Wno-unused-parameter
        -fPIC
        -fvisibility=hidden
    )
    if(NOT CMAKE_BUILD_TYPE STREQUAL "Debug")
        target_compile_options(${NIF_TARGET} PRIVATE -O2)
    endif()
endif()

# Linker flags
if(APPLE)
    set_target_properties(${NIF_TARGET} PROPERTIES
        LINK_FLAGS "-undefined dynamic_lookup"
    )
elseif(CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")
    set_target_properties(${NIF_TARGET} PROPERTIES
        LINK_FLAGS "-L/usr/local/lib"
    )
endif()

# Link libraries
target_link_libraries(${NIF_TARGET}
    PRIVATE
    lsquic
    ssl
    crypto
    ${ZLIB_LIBRARIES}
    Threads::Threads
)

if(MSVC)
    target_link_libraries(${NIF_TARGET} PRIVATE ws2_32)
endif()

# Output name
set_target_properties(${NIF_TARGET} PROPERTIES
    PREFIX ""
    OUTPUT_NAME "${NIF_TARGET}"
)
