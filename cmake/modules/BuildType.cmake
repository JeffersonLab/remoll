# Set a default build type if none was specified
set(DEFAULT_CMAKE_BUILD_TYPE "Release")
if (EXISTS "${CMAKE_SOURCE_DIR}/.git")
  set(DEFAULT_CMAKE_BUILD_TYPE "RelWithDebInfo")
endif()
if (EXISTS "${CMAKE_SOURCE_DIR}/.buildtype")
  file(STRINGS "${CMAKE_SOURCE_DIR}/.buildtype" DEFAULT_CMAKE_BUILD_TYPE)
endif()
if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to '${DEFAULT_CMAKE_BUILD_TYPE}' as none was specified.")
  message(STATUS "    Use -DCMAKE_BUILD_TYPE= to set to: Debug, Release, MinSizeRel, RelWithDebInfo.")
  set(CMAKE_BUILD_TYPE "${DEFAULT_CMAKE_BUILD_TYPE}" CACHE
    STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
    "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()
