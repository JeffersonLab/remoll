# - Find remoll library
# This module checks if remoll was compiled
# It defines:
# EXE_FOUND               If the remoll is found

find_program(EXE NAMES remoll
             PATHS ${PROJECT_BINARY_DIR}/remoll)
if(EXE)
  set(EXE_FOUND TRUE)
else()
  set(EXE_FOUND FALSE)
  message(STATUS "NOT Found remoll: set REMOLL_INSTALL env var.")

endif()
