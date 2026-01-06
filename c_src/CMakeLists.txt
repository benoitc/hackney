cmake_minimum_required(VERSION 3.14 FATAL_ERROR)

project(HackneyQuicNIF C)

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

# CMake module path
list(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/CMake")

# Architecture detection
message(STATUS "Building for: ${CMAKE_SYSTEM_NAME} ${CMAKE_SYSTEM_PROCESSOR}")

# Standard package manager paths by platform
if(APPLE)
    # macOS - Homebrew (arm64 and x86_64) and MacPorts
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
    # FreeBSD - ports and pkg
    if(EXISTS "/usr/local")
        list(APPEND CMAKE_PREFIX_PATH "/usr/local")
        list(APPEND CMAKE_INCLUDE_PATH "/usr/local/include")
        list(APPEND CMAKE_LIBRARY_PATH "/usr/local/lib")
    endif()
elseif(CMAKE_SYSTEM_NAME STREQUAL "NetBSD")
    # NetBSD - pkgsrc
    if(EXISTS "/usr/pkg")
        list(APPEND CMAKE_PREFIX_PATH "/usr/pkg")
        list(APPEND CMAKE_INCLUDE_PATH "/usr/pkg/include")
        list(APPEND CMAKE_LIBRARY_PATH "/usr/pkg/lib")
    endif()
elseif(CMAKE_SYSTEM_NAME STREQUAL "OpenBSD")
    # OpenBSD - ports
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

# C standard
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# Threading
set(CMAKE_THREAD_PREFER_PTHREAD ON)
set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)

#--------------------------------------------------------------------------------
# Find Erlang
#--------------------------------------------------------------------------------
include(FindErlang)
include_directories(${ERLANG_ERTS_INCLUDE_PATH})

#--------------------------------------------------------------------------------
# Find OpenSSL (required for QUIC-TLS)
#--------------------------------------------------------------------------------
find_package(OpenSSL 1.1.1 REQUIRED)
if(NOT OPENSSL_FOUND)
    message(FATAL_ERROR "OpenSSL >= 1.1.1 is required for QUIC-TLS support")
endif()
message(STATUS "Found OpenSSL: ${OPENSSL_VERSION}")
include_directories(${OPENSSL_INCLUDE_DIR})

#--------------------------------------------------------------------------------
# Build ngtcp2 (QUIC library)
#--------------------------------------------------------------------------------
set(NGTCP2_SOURCE_DIR "${PROJECT_SOURCE_DIR}/../libs/ngtcp2")
if(EXISTS "${NGTCP2_SOURCE_DIR}/CMakeLists.txt")
    message(STATUS "Building ngtcp2 from: ${NGTCP2_SOURCE_DIR}")

    # ngtcp2 build options
    set(ENABLE_SHARED_LIB OFF CACHE BOOL "" FORCE)
    set(ENABLE_STATIC_LIB ON CACHE BOOL "" FORCE)
    set(ENABLE_OPENSSL ON CACHE BOOL "" FORCE)
    set(ENABLE_GNUTLS OFF CACHE BOOL "" FORCE)
    set(ENABLE_BORINGSSL OFF CACHE BOOL "" FORCE)
    set(ENABLE_PICOTLS OFF CACHE BOOL "" FORCE)
    set(ENABLE_WOLFSSL OFF CACHE BOOL "" FORCE)
    set(BUILD_TESTING OFF CACHE BOOL "" FORCE)
    set(ENABLE_EXAMPLES OFF CACHE BOOL "" FORCE)

    add_subdirectory(${NGTCP2_SOURCE_DIR} ${CMAKE_BINARY_DIR}/ngtcp2 EXCLUDE_FROM_ALL)

    set(NGTCP2_INCLUDE_DIRS "${NGTCP2_SOURCE_DIR}/lib/includes" "${CMAKE_BINARY_DIR}/ngtcp2/lib/includes")
    set(NGTCP2_LIBRARIES ngtcp2_static ngtcp2_crypto_ossl_static)
else()
    message(FATAL_ERROR "ngtcp2 not found at ${NGTCP2_SOURCE_DIR}. Run: git subtree add --prefix=libs/ngtcp2 https://github.com/ngtcp2/ngtcp2.git v1.9.1 --squash")
endif()

#--------------------------------------------------------------------------------
# Build nghttp3 (HTTP/3 library)
#--------------------------------------------------------------------------------
set(NGHTTP3_SOURCE_DIR "${PROJECT_SOURCE_DIR}/../libs/nghttp3")
if(EXISTS "${NGHTTP3_SOURCE_DIR}/CMakeLists.txt")
    message(STATUS "Building nghttp3 from: ${NGHTTP3_SOURCE_DIR}")

    # nghttp3 build options
    set(ENABLE_SHARED_LIB OFF CACHE BOOL "" FORCE)
    set(ENABLE_STATIC_LIB ON CACHE BOOL "" FORCE)
    set(BUILD_TESTING OFF CACHE BOOL "" FORCE)
    set(ENABLE_EXAMPLES OFF CACHE BOOL "" FORCE)
    set(ENABLE_LIB_ONLY ON CACHE BOOL "" FORCE)

    add_subdirectory(${NGHTTP3_SOURCE_DIR} ${CMAKE_BINARY_DIR}/nghttp3 EXCLUDE_FROM_ALL)

    set(NGHTTP3_INCLUDE_DIRS "${NGHTTP3_SOURCE_DIR}/lib/includes" "${CMAKE_BINARY_DIR}/nghttp3/lib/includes")
    set(NGHTTP3_LIBRARIES nghttp3_static)
else()
    message(FATAL_ERROR "nghttp3 not found at ${NGHTTP3_SOURCE_DIR}. Run: git subtree add --prefix=libs/nghttp3 https://github.com/ngtcp2/nghttp3.git v1.6.0 --squash")
endif()

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
    ${OPENSSL_INCLUDE_DIR}
    ${NGTCP2_INCLUDE_DIRS}
    ${NGHTTP3_INCLUDE_DIRS}
)

# Compiler flags
if(MSVC)
    target_compile_options(${NIF_TARGET} PRIVATE /W4)
else()
    target_compile_options(${NIF_TARGET} PRIVATE
        -Wall -Wextra
        -Wno-unused-parameter
        -Wno-pedantic
        -fPIC
    )
    if(NOT CMAKE_BUILD_TYPE STREQUAL "Debug")
        target_compile_options(${NIF_TARGET} PRIVATE -O2)
    endif()
    # FreeBSD-specific flags
    if(CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")
        target_compile_options(${NIF_TARGET} PRIVATE -I/usr/local/include)
    endif()
endif()

# Linker flags
if(APPLE)
    set_target_properties(${NIF_TARGET} PROPERTIES
        LINK_FLAGS "-flat_namespace -undefined suppress"
    )
elseif(CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")
    set_target_properties(${NIF_TARGET} PROPERTIES
        LINK_FLAGS "-L/usr/local/lib"
    )
endif()

# Link libraries
target_link_libraries(${NIF_TARGET}
    PRIVATE
    ${NGTCP2_LIBRARIES}
    ${NGHTTP3_LIBRARIES}
    ${OPENSSL_LIBRARIES}
    Threads::Threads
)

if(MSVC)
    target_link_libraries(${NIF_TARGET} PRIVATE ws2_32)
endif()

# Output name (without 'lib' prefix on Unix)
set_target_properties(${NIF_TARGET} PROPERTIES
    PREFIX ""
    OUTPUT_NAME "${NIF_TARGET}"
)

# Install
install(TARGETS ${NIF_TARGET} LIBRARY DESTINATION ${priv_dir})
