# Find Erlang installation for NIF compilation
# Sets:
#  ERLANG_RUNTIME              - path to erl
#  ERLANG_COMPILE              - path to erlc
#  ERLANG_EI_PATH              - path to erl_interface
#  ERLANG_ERTS_PATH            - path to erts
#  ERLANG_EI_INCLUDE_PATH      - include path for erl_interface
#  ERLANG_EI_LIBRARY_PATH      - library path for erl_interface
#  ERLANG_ERTS_INCLUDE_PATH    - include path for erts (contains erl_nif.h)
#  ERLANG_ERTS_LIBRARY_PATH    - library path for erts

SET(ERLANG_BIN_PATH
    $ENV{ERLANG_HOME}/bin
    /opt/bin
    /sw/bin
    /usr/bin
    /usr/local/bin
    /opt/local/bin
    /opt/homebrew/bin
   )

FIND_PROGRAM(ERLANG_RUNTIME
  NAMES erl
  PATHS ${ERLANG_BIN_PATH}
  )

FIND_PROGRAM(ERLANG_COMPILE
  NAMES erlc
  PATHS ${ERLANG_BIN_PATH}
  )

EXECUTE_PROCESS(COMMAND
  erl -noshell -eval "io:format(\"~s\", [code:lib_dir()])" -s erlang halt
  OUTPUT_VARIABLE ERLANG_OTP_LIB_DIR)

EXECUTE_PROCESS(COMMAND
  erl -noshell -eval "io:format(\"~s\", [code:root_dir()])" -s erlang halt
  OUTPUT_VARIABLE ERLANG_OTP_ROOT_DIR)

MESSAGE(STATUS "Using OTP lib: ${ERLANG_OTP_LIB_DIR}")

EXECUTE_PROCESS(COMMAND
  erl -noshell -eval "io:format(\"~s\",[filename:basename(code:lib_dir('erl_interface'))])" -s erlang halt
  OUTPUT_VARIABLE ERLANG_EI_DIR)

EXECUTE_PROCESS(COMMAND
  erl -noshell -eval "io:format(\"~s\",[filename:basename(code:lib_dir('erts'))])" -s erlang halt
  OUTPUT_VARIABLE ERLANG_ERTS_DIR)

MESSAGE(STATUS "Using erl_interface version: ${ERLANG_EI_DIR}")
MESSAGE(STATUS "Using erts version: ${ERLANG_ERTS_DIR}")

SET(ERLANG_EI_PATH ${ERLANG_OTP_LIB_DIR}/${ERLANG_EI_DIR})
SET(ERLANG_EI_INCLUDE_PATH ${ERLANG_OTP_LIB_DIR}/${ERLANG_EI_DIR}/include)
SET(ERLANG_EI_LIBRARY_PATH ${ERLANG_OTP_LIB_DIR}/${ERLANG_EI_DIR}/lib)

SET(ERLANG_ERTS_PATH ${ERLANG_OTP_ROOT_DIR}/${ERLANG_ERTS_DIR})
SET(ERLANG_ERTS_INCLUDE_PATH ${ERLANG_OTP_ROOT_DIR}/${ERLANG_ERTS_DIR}/include)
SET(ERLANG_ERTS_LIBRARY_PATH ${ERLANG_OTP_ROOT_DIR}/${ERLANG_ERTS_DIR}/lib)

MARK_AS_ADVANCED(
  ERLANG_RUNTIME
  ERLANG_COMPILE
  ERLANG_EI_PATH
  ERLANG_EI_INCLUDE_PATH
  ERLANG_EI_LIBRARY_PATH
  )
