# - Find remoll library
# This module sets up remoll information
# It defines:
# REMOLL_FOUND               If the remoll is found
# REMOLL_INCLUDE_DIR         PATH to the include directory
# REMOLL_LIBRARY_DIR         PATH to the library directory

find_program(REMOLL_CONFIG NAMES remoll-config
             PATHS $ENV{REMOLL_INSTALL}/bin
                   ${REMOLL_INSTALL}/bin
                   /usr/local/bin /opt/local/bin)

if(REMOLL_CONFIG)
  set(REMOLL_FOUND TRUE)

  execute_process(COMMAND ${REMOLL_CONFIG} --prefix
                  OUTPUT_VARIABLE REMOLL_PREFIX
                  OUTPUT_STRIP_TRAILING_WHITESPACE)

  execute_process(COMMAND ${REMOLL_CONFIG} --bindir
                  OUTPUT_VARIABLE REMOLL_BINARY_DIR
                  OUTPUT_STRIP_TRAILING_WHITESPACE)

  execute_process(COMMAND ${REMOLL_CONFIG} --incdir
                  OUTPUT_VARIABLE REMOLL_INCLUDE_DIR
                  OUTPUT_STRIP_TRAILING_WHITESPACE)

  execute_process(COMMAND ${REMOLL_CONFIG} --libdir
                  OUTPUT_VARIABLE REMOLL_LIBRARY_DIR
                  OUTPUT_STRIP_TRAILING_WHITESPACE)

  execute_process(COMMAND ${REMOLL_CONFIG} --libs
                  OUTPUT_VARIABLE REMOLL_LIBRARIES
                  OUTPUT_STRIP_TRAILING_WHITESPACE)

  message(STATUS "Found remoll: ${REMOLL_PREFIX}")

else()
  set(REMOLL_FOUND FALSE)
  message(STATUS "NOT Found remoll: set REMOLL_INSTALL env var.")

endif()
