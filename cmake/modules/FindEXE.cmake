# - Find remoll library
# This module checks if remoll was compiled
# It defines:
# exe_FOUND               If the remoll is found

find_program(exe NAMES remoll
             PATHS ${PROJECT_BINARY_DIR}/remoll)
if(exe)
    set(exe_FOUND TRUE)
else()
  set(REMOLL_FOUND FALSE)
  message(STATUS "NOT Found remoll: set REMOLL_INSTALL env var.")

endif()
