#----------------------------------------------------------------------------
# Setup the project
#
cmake_minimum_required(VERSION 3.5)
project(remoll VERSION 0.1 LANGUAGES CXX)

# Disallow in-source builds
if(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_BINARY_DIR})
  message(FATAL_ERROR "remoll requires an out-of-source build.")
endif()

# Prepend module search path
set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/modules ${CMAKE_MODULE_PATH})

# Default install path is the source directory
if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    message(STATUS "    Install-prefix was at default -> forcing it to the source-dir" )
    message(STATUS "    Use -DCMAKE_INSTALL_PREFIX=/usr/local to set to something else" )
    set (CMAKE_INSTALL_PREFIX "${CMAKE_SOURCE_DIR}"
         CACHE PATH "default install path" FORCE )
endif()

# Use our modules and utility functions
include(CMakeEnv)

# Use GNU install dirs
include(GNUInstallDirs)

# MAC specific variable
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set(CMAKE_MACOSX_RPATH ON)
    set(CMAKE_SKIP_BUILD_RPATH FALSE)
    set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)
    set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
    set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
    list(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
    if("${isSystemDir}" STREQUAL "-1")
        set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
    endif()
endif()


#----------------------------------------------------------------------------
# Locate sources and headers for this project
#
file(GLOB sources src/remoll*.cc)
file(GLOB headers include/remoll*.hh)


#----------------------------------------------------------------------------
# ROOT
#
set(minimum_root_version 6.0)
find_package(ROOT ${minimum_root_version} REQUIRED)
config_add_dependency(ROOT ${minimum_root_version})


# Update header file
add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/include/gitinfo.hh COMMAND ${PROJECT_SOURCE_DIR}/pullgitinfo.py ${PROJECT_SOURCE_DIR} COMMENT "Creating gitinfo.hh" DEPENDS ${PROJECT_SOURCE_DIR}/pullgitinfo.py)

# this is to ensure we find the header we need
include_directories(${CMAKE_CURRENT_BINARY_DIR}/include)

#----------------------------------------------------------------------------
# main dictionaries
#
message(STATUS "Generating ${PROJECT_NAME} dictionaries for ROOT ${ROOT_VERSION}...")
foreach(file ${headers})
  # Find the header files that have uncommented ClassDef lines
  execute_process(COMMAND grep -l "[^/]ClassDef" ${file} OUTPUT_VARIABLE result)
  if(result)
    list(APPEND needsdict ${file})
  endif()
endforeach()
build_root_dictionary(
  ${PROJECT_NAME}
  ${needsdict}
  TARGETS lib${PROJECT_NAME}
  LINKDEF ${PROJECT_SOURCE_DIR}/include/${PROJECT_NAME}LinkDef.hh
)
list(APPEND sources ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Dict.cxx)

#----------------------------------------------------------------------------
# Find Boost libraries, in particular iostreams

if(USE_BOOST)
    find_package(Boost COMPONENTS iostreams)
    include_directories(${Boost_INCLUDE_DIRS})
    link_directories(${Boost_LIBRARY_DIR})
    ADD_DEFINITIONS(-D__USE_BOOST)
    if(Boost_IOSTREAMS_FOUND)
        ADD_DEFINITIONS(-D__USE_BOOST_IOSTREAMS)
    endif()
endif()


#----------------------------------------------------------------------------
# Find Geant4 package, activating all available UI and Vis drivers by default
# You can set WITH_GEANT4_UIVIS to OFF via the command line or ccmake/cmake-gui
# to build a batch mode only executable
#
option(WITH_GEANT4_UIVIS "Build example with Geant4 UI and Vis drivers" ON)
if(WITH_GEANT4_UIVIS)
  find_package(Geant4 4.10 REQUIRED ui_all vis_all)
else()
  find_package(Geant4 4.10 REQUIRED)
endif()

#----------------------------------------------------------------------------
# Setup Geant4 include directories and compile definitions
# Setup include directory for this project
#
include(${Geant4_USE_FILE})

#----------------------------------------------------------------------------
# Find HepMC (optional package)
#
find_package(HepMC QUIET)
if(HepMC_FOUND)
  message(STATUS "HepMC found --> building with HepMC enabled.")
  add_definitions(-DG4LIB_USE_HEPMC)
  file(GLOB hepmc_sources ${PROJECT_SOURCE_DIR}/src/HepMC*.cc)
  file(GLOB hepmc_headers ${PROJECT_SOURCE_DIR}/include/HepMC*.hh)
  list(APPEND sources ${hepmc_sources})
  list(APPEND headers ${hepmc_headers})
else()
  set(HEPMC_LIBRARIES "")
endif()

#----------------------------------------------------------------------------
# Find Pythia6 (optional package)
#
find_package(Pythia6 QUIET)
if(PYTHIA6_FOUND)
  message(STATUS "Pythia6 found --> building with Pythia6 enabled.")
  add_definitions(-DG4LIB_USE_PYTHIA)
else()
  set(PYTHIA6_LIBRARIES "")
endif()

#----------------------------------------------------------------------------
# Debugging symbols, warnings
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -Wall -pg")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -Wall -g -Woverloaded-virtual")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -Wall -g -Wno-shadow -Wno-pedantic -Wno-overloaded-virtual")

# Ignore warning of struct initialization { } for gcc < 5.0 (after that it is ignored internally)
if(CMAKE_CXX_COMPILER_VERSION VERSION_LESS 5.0)
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-missing-field-initializers")
endif()

# C++11 standard if avialable
include(CheckCXXCompilerFlag)
CHECK_CXX_COMPILER_FLAG("-std=c++0x" COMPILER_SUPPORTS_CXX0X)
CHECK_CXX_COMPILER_FLAG("-std=c++11" COMPILER_SUPPORTS_CXX11)
if(COMPILER_SUPPORTS_CXX11)
        message(STATUS "The compiler ${CMAKE_CXX_COMPILER} has C++11 support.")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
elseif(COMPILER_SUPPORTS_CXX0X)
        message(STATUS "The compiler ${CMAKE_CXX_COMPILER} has C++0x support.")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")
else()
        message(STATUS "The compiler ${CMAKE_CXX_COMPILER} has no C++11 support.")
endif()

#----------------------------------------------------------------------------
# Generate dictionaries as well (so we can write objects to ROOT files

include_directories(${PROJECT_SOURCE_DIR}/include ${ROOT_INCLUDE_DIR} ${CMAKE_CURRENT_BINARY_DIR}/include)


#----------------------------------------------------------------------------
# Add the executable, and link it to the Geant4 libraries
#
set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_FULL_LIBDIR}:${ROOT_LIBRARY_DIR}:${GEANT4_LIBRARY_DIR}")

add_library(lib${PROJECT_NAME} SHARED ${sources} ${headers} ${CMAKE_CURRENT_BINARY_DIR}/include/gitinfo.hh)
set_target_properties(lib${PROJECT_NAME} PROPERTIES OUTPUT_NAME remoll)
target_link_libraries(lib${PROJECT_NAME}
  ${Geant4_LIBRARIES}
  ${ROOT_LIBRARIES}
  ${Boost_LIBRARIES}
  ${HEPMC_LIBRARIES}
  ${PYTHIA6_LIBRARIES})

add_executable(remoll-bin remoll.cc)
set_target_properties(remoll-bin PROPERTIES OUTPUT_NAME remoll)
target_link_libraries(remoll-bin lib${PROJECT_NAME})

add_executable(reroot-bin reroot.cc)
set_target_properties(reroot-bin PROPERTIES OUTPUT_NAME reroot)
target_link_libraries(reroot-bin lib${PROJECT_NAME})

#----------------------------------------------------------------------------
# Copy all scripts to the build directory, i.e. the directory in which we
# build remoll. This is so that we can run the executable directly because it
# relies on these scripts being in the current working directory.
#

install(DIRECTORY geometry DESTINATION ${CMAKE_INSTALL_DATADIR}/remoll)
install(DIRECTORY macros DESTINATION ${CMAKE_INSTALL_DATADIR}/remoll)
install(DIRECTORY vis DESTINATION ${CMAKE_INSTALL_DATADIR}/remoll)

file(GLOB vis_macros RELATIVE ${PROJECT_SOURCE_DIR} vis/*.mac)
set(REMOLL_SCRIPTS
    ${vis_macros}
    macros/runexample.mac
    macros/gui.mac
    macros/moller_dose.mac
  )

foreach(_script ${REMOLL_SCRIPTS})
  configure_file(
    ${PROJECT_SOURCE_DIR}/${_script}
    ${PROJECT_BINARY_DIR}/${_script}
    COPYONLY
    )
endforeach()


#----------------------------------------------------------------------------
# Templates
#
file(GLOB templates RELATIVE ${PROJECT_SOURCE_DIR} cmake/templates/*.in)
foreach(_template_in ${templates})
  string(REPLACE ".in" "" _template_out ${_template_in})
  configure_file(
    ${PROJECT_SOURCE_DIR}/${_template_in}
    ${PROJECT_BINARY_DIR}/${_template_out}
    @ONLY
    )
  install(FILES
    ${PROJECT_BINARY_DIR}/${_template_out}
    DESTINATION bin
    PERMISSIONS
      OWNER_READ OWNER_WRITE OWNER_EXECUTE
      GROUP_READ GROUP_EXECUTE
      WORLD_READ WORLD_EXECUTE
    )
endforeach()

#----------------------------------------------------------------------------
# Download field maps subdirectory
#
set(REMOLL_LOCATION /group/halla/www/hallaweb/html/12GeV/Moller/downloads/remoll/)
set(REMOLL_DOWNLOADS http://hallaweb.jlab.org/12GeV/Moller/downloads/remoll/)
set(REMOLL_MAP_DIR ${PROJECT_SOURCE_DIR}/map_directory)
if(EXISTS ${REMOLL_LOCATION})
  set(REMOLL_DOWNLOADS file://${REMOLL_LOCATION})
endif()
file(DOWNLOAD
     ${REMOLL_DOWNLOADS}/blockyUpstream_rm_1.1.txt
     ${REMOLL_MAP_DIR}/blockyUpstream_rm_1.1.txt
     EXPECTED_MD5 3e2338e1ba74b03da37545e98931f5f3)
file(DOWNLOAD
     ${REMOLL_DOWNLOADS}/blockyHybrid_rm_3.0.txt
     ${REMOLL_MAP_DIR}/blockyHybrid_rm_3.0.txt
     EXPECTED_MD5 b4bfef8f362e0df66f166b4e76a6847e)
install(FILES
     ${REMOLL_MAP_DIR}/blockyHybrid_rm_3.0.txt
     ${REMOLL_MAP_DIR}/blockyUpstream_rm_1.1.txt
     DESTINATION ${CMAKE_INSTALL_DATADIR}/remoll)

#----------------------------------------------------------------------------
# Build analysis subdirectory
# FIXME: this doesn't work due to dependency on remoll-config at cmake time

#----------------------------------------------------------------------------
# Install cmake config file
#
install(FILES
    ${PROJECT_SOURCE_DIR}/cmake/scripts/RemollConfig.cmake
    ${PROJECT_SOURCE_DIR}/cmake/scripts/RemollUseFile.cmake
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/remoll)

#----------------------------------------------------------------------------
# For internal Geant4 use - but has no effect if you build this
# example standalone
#
add_custom_target(REMOLL DEPENDS remoll-bin)

#----------------------------------------------------------------------------
# Install the executable to 'bin' directory under CMAKE_INSTALL_PREFIX
#
install(TARGETS remoll-bin reroot-bin DESTINATION ${CMAKE_INSTALL_BINDIR})
install(TARGETS lib${PROJECT_NAME} DESTINATION ${CMAKE_INSTALL_LIBDIR})

# Attempt at moving libremollDict_rdict.pcm to the bin so reroot doesn't error
#add_custom_command(TARGET remoll-bin POST_BUILD
#    COMMAND ${CMAKE_COMMAND} -E copy_if_different
#    "${PROJECT_BINARY_DIR}/libremollDict_rdict.pcm"
#    ${CMAKE_INSTALL_BINDIR})

#----------------------------------------------------------------------------
# Uninstall target
#
if(NOT TARGET uninstall)
  configure_file(
      "${CMAKE_CURRENT_SOURCE_DIR}/cmake/scripts/cmake_uninstall.cmake.in"
      "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
      IMMEDIATE @ONLY)
  add_custom_target(uninstall
      COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)
endif()
