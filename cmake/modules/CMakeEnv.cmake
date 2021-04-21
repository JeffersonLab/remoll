# CMake settings and utility functions for projects

include(CheckCXXCompilerFlag)

#----------------------------------------------------------------------------
# Set default build type to RelWithDebInfo
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING "Build type" FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
    "Debug" "Release" "RelWithDebInfo" "MinSizeRel")
endif()

#----------------------------------------------------------------------------
# Build options
option(WITH_DEBUG "Enable support for detailed debug messages" ON)

#----------------------------------------------------------------------------
# Project-specific build flags
if(${CMAKE_SYSTEM_NAME} MATCHES Darwin)
  set(CMAKE_SHARED_LINKER_FLAGS
    "${CMAKE_SHARED_LINKER_FLAGS} -Wl,-undefined,dynamic_lookup")
  list(REMOVE_DUPLICATES CMAKE_SHARED_LINKER_FLAGS)
endif()

#----------------------------------------------------------------------------
# Useful shorthands
string(TOLOWER ${PROJECT_NAME} PROJECT_NAME_LC)
string(TOUPPER ${PROJECT_NAME} PROJECT_NAME_UC)
if("${CMAKE_PROJECT_NAME}" STREQUAL "${PROJECT_NAME}")
  set(MAIN_PROJECT_NAME ${PROJECT_NAME})
  string(TOLOWER ${PROJECT_NAME} MAIN_PROJECT_NAME_LC)
endif()
if(NOT PROJECT_VERSION_PATCH)
  set(PROJECT_VERSION_PATCH 0)
endif()
if(NOT PROJECT_VERSION_MINOR)
  set(PROJECT_VERSION_MINOR 0)
endif()
if(NOT PROJECT_VERSION_MAJOR)
  set(PROJECT_VERSION_MAJOR 0)
endif()
math(EXPR ${PROJECT_NAME_UC}_VERCODE
  "${PROJECT_VERSION_MAJOR} * 65536 + ${PROJECT_VERSION_MINOR} * 256 + ${PROJECT_VERSION_PATCH}")

#============================================================================
# Remove duplicates from space-separated list of items
function(remove_duplicates _invar _outvar)
  separate_arguments(_invar)
  list(REMOVE_DUPLICATES _invar)
  string(REPLACE ";" " " _invar "${_invar}")
  set(${_outvar} "${_invar}" PARENT_SCOPE)
endfunction()

#----------------------------------------------------------------------------
# Get list of definitions for given target as list of -DXXX
# (needed for generating ROOT dictionaries)
function(get_target_definitions TARGET DEFINES)
  get_target_property(defs ${TARGET} COMPILE_DEFINITIONS)
  if(defs)
    foreach(d IN LISTS defs)
      list(APPEND deflist -D${d})
    endforeach(d)
    set(${DEFINES} ${deflist} PARENT_SCOPE)
  endif()
endfunction(get_target_definitions)

#----------------------------------------------------------------------------
# Set project's CXX level if no "-std=xxx" flag given in CXX_FLAGS
macro(set_cxx_std CXX_FLAGS)
  string(REGEX MATCH "(^| +)-std=([^ ]*)" std_flag "${CXX_FLAGS}")
  if(NOT CMAKE_MATCH_COUNT GREATER 1)
    if(DEFINED ROOT_VERSION AND NOT ${ROOT_VERSION} VERSION_LESS 6)
      set(CMAKE_CXX_STANDARD 11)
      set(CMAKE_CXX_STANDARD_REQUIRED TRUE)
      set(CMAKE_CXX_EXTENSIONS FALSE)
    endif()
  endif()
  unset(std_flag)
endmacro(set_cxx_std)

#----------------------------------------------------------------------------
# Set our CXX flags plus "_suggested_flags" without any -IXXX options
macro(set_compiler_flags _suggested_flags)
  check_cxx_compiler_flag(-pipe cxx-compiler-supports-pipe)
  check_cxx_compiler_flag(-fsigned-char cxx-compiler-supports-fsigned-char)
  check_cxx_compiler_flag(-fomit-frame-pointer cxx-compiler-supports-fomit-frame-pointer)
  check_cxx_compiler_flag(-mtune=generic cxx-compiler-supports-mtune-generic)
  if(APPLE)
    check_cxx_compiler_flag(-Qunused-arguments cxx-compiler-supports-Qunused-arguments)
  endif()
  if(cxx-compiler-supports-pipe)
    set(${PROJECT_NAME_UC}_CXX_FLAGS "${${PROJECT_NAME_UC}_CXX_FLAGS} -pipe")
  endif()
  if(cxx-compiler-supports-fsigned-char)
    set(${PROJECT_NAME_UC}_CXX_FLAGS "${${PROJECT_NAME_UC}_CXX_FLAGS} -fsigned-char")
  endif()
  if(cxx-compiler-supports-mtune-generic)
    set(${PROJECT_NAME_UC}_CXX_FLAGS "${${PROJECT_NAME_UC}_CXX_FLAGS} -mtune=generic")
  endif()
  if(cxx-compiler-supports-Qunused-arguments)
    set(${PROJECT_NAME_UC}_CXX_FLAGS "${${PROJECT_NAME_UC}_CXX_FLAGS} -Qunused-arguments")
  endif()
  set(${PROJECT_NAME_UC}_CXX_FLAGS "${${PROJECT_NAME_UC}_CXX_FLAGS} ${_suggested_flags}")
  if(${PROJECT_NAME_UC}_CXX_FLAGS)
    remove_duplicates(${${PROJECT_NAME_UC}_CXX_FLAGS} ${PROJECT_NAME_UC}_CXX_FLAGS)
    set_cxx_std(${${PROJECT_NAME_UC}_CXX_FLAGS})
  endif()
  string(STRIP "${${PROJECT_NAME_UC}_CXX_FLAGS}" ${PROJECT_NAME_UC}_CXX_FLAGS_LIST)
  separate_arguments(${PROJECT_NAME_UC}_CXX_FLAGS_LIST)
  unset(cxx-compiler-supports-pipe)
  unset(cxx-compiler-supports-fsigned-char)
  unset(cxx-compiler-supports-fexceptions)
  unset(cxx-compiler-supports-mtune-generic)
  unset(cxx-compiler-supports-Qunused-arguments)
  # Configuration dependent options. Avoid ugly generator expressions
  if(cxx-compiler-supports-fomit-frame-pointer)
    set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -fno-omit-frame-pointer")
  endif()
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -O0")
endmacro(set_compiler_flags)

#----------------------------------------------------------------------------
# Set warning flags according to given argument(s)
macro(set_diagnostic_flags)
  cmake_parse_arguments(SDF "WALL;WEXTRA;WERROR" "" "" ${ARGN})
  if(SDF_UNPARSED_ARGUMENTS)
    message(FATAL_ERROR
      "set_diagnostic_flags: Unrecognized arguments: ${SDF_UNPARSED_ARGUMENTS}"
      )
  endif()

  if(SDF_WALL OR SDF_WEXTRA)
    set(${PROJECT_NAME_UC}_DIAG_FLAGS "${${PROJECT_NAME_UC}_DIAG_FLAGS} -Wall")
  endif()
  if(SDF_WEXTRA)
    set(${PROJECT_NAME_UC}_DIAG_FLAGS "${${PROJECT_NAME_UC}_DIAG_FLAGS} -Wextra")
  endif()
  if(SDF_WERROR)
    set(${PROJECT_NAME_UC}_DIAG_FLAGS "${${PROJECT_NAME_UC}_DIAG_FLAGS} -Werror")
  endif()
  if(${ARGC} GREATER 0)
    remove_duplicates(${${PROJECT_NAME_UC}_DIAG_FLAGS} ${PROJECT_NAME_UC}_DIAG_FLAGS)
  endif()
  string(STRIP "${${PROJECT_NAME_UC}_DIAG_FLAGS}" ${PROJECT_NAME_UC}_DIAG_FLAGS_LIST)
  separate_arguments(${PROJECT_NAME_UC}_DIAG_FLAGS_LIST)
endmacro(set_diagnostic_flags)

#----------------------------------------------------------------------------
# Print build configuration details (build type, project-wde compiler flags)
function(report_build_info)
  string(TOUPPER ${CMAKE_BUILD_TYPE} BUILD_TYPE_UC)
  message(STATUS "Compiling for ${CMAKE_SYSTEM_NAME} with ${CMAKE_CXX_COMPILER} version ${CMAKE_CXX_COMPILER_VERSION}")
  message(STATUS "Build type ${CMAKE_BUILD_TYPE}")
  message(STATUS "${PROJECT_NAME_UC}_CXX_FLAGS = ${${PROJECT_NAME_UC}_CXX_FLAGS} ${${PROJECT_NAME_UC}_DIAG_FLAGS}")
  message(STATUS "CMAKE_CXX_FLAGS_${BUILD_TYPE_UC} = ${CMAKE_CXX_FLAGS_${BUILD_TYPE_UC}}")
endfunction(report_build_info)

#----------------------------------------------------------------------------
# Support for keeping track of dependencies loaded via our FindXXX scripts

# Dependencies already found
set_property(GLOBAL PROPERTY ${PROJECT_NAME_UC}_DEPENDENCY_LIST "")
# Commands to write to Config.cmake
set_property(GLOBAL PROPERTY ${PROJECT_NAME_UC}_FIND_DEPENDENCY_COMMANDS "")

function(config_add_dependency item)
  get_property(deplist GLOBAL PROPERTY ${PROJECT_NAME_UC}_DEPENDENCY_LIST)
  list(FIND deplist ${item} found)
  if(${found} LESS 0)
    set_property(GLOBAL APPEND PROPERTY ${PROJECT_NAME_UC}_DEPENDENCY_LIST ${item})
    get_property(commands GLOBAL PROPERTY ${PROJECT_NAME_UC}_FIND_DEPENDENCY_COMMANDS)
    list(LENGTH commands ncmd)
    if(ncmd EQUAL 0)
      set_property(GLOBAL APPEND PROPERTY ${PROJECT_NAME_UC}_FIND_DEPENDENCY_COMMANDS
	"include(CMakeFindDependencyMacro)")
    endif()
    cmake_parse_arguments(CAD "" "" "" ${ARGN})
    set(version)
    if(CAD_UNPARSED_ARGUMENTS)
      list(GET CAD_UNPARSED_ARGUMENTS 0 version)
    endif()
    if(version)
      set_property(GLOBAL APPEND PROPERTY ${PROJECT_NAME_UC}_FIND_DEPENDENCY_COMMANDS
	"find_dependency(${item} ${version})")
    else()
      set_property(GLOBAL APPEND PROPERTY ${PROJECT_NAME_UC}_FIND_DEPENDENCY_COMMANDS
	"find_dependency(${item})")
    endif()
  endif()
endfunction()
