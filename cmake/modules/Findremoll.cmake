# - Find remoll library
# This module checks if remoll was compiled
# It defines:
# remoll_FOUND               If the remoll is found

find_program(remoll NAMES remoll
             PATHS ${PROJECT_SOURCE_DIR}/build/remoll)
if(remoll)
    set(remoll_FOUND TRUE)
else()
  set(REMOLL_FOUND FALSE)
  message(STATUS "NOT Found remoll: set REMOLL_INSTALL env var.")

endif()
