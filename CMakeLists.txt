#----------------------------------------------------------------------------

# Setup the project
#
cmake_minimum_required(VERSION 2.6 FATAL_ERROR)
project(REMOLL)

# Default install path is the source directory
if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    message(STATUS "    Install-prefix was at default -> forcing it to the source-dir" )
    message(STATUS "    Use -DCMAKE_INSTALL_PREFIX=/usr/local to set to something else" )
    set (CMAKE_INSTALL_PREFIX "${CMAKE_SOURCE_DIR}"
         CACHE PATH "default install path" FORCE )
endif()

#MAC specific variable
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

#ROOT path
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/modules")
find_package(ROOT REQUIRED)

# Update header file
add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/include/gitinfo.hh COMMAND ${PROJECT_SOURCE_DIR}/pullgitinfo.py ${PROJECT_SOURCE_DIR} COMMENT "Creating gitinfo.hh" )

# this is to ensure we find the header we need
include_directories(${CMAKE_CURRENT_BINARY_DIR}/include)

ROOT_GENERATE_DICTIONARY(
    remollDict                         # path to dictionary to generate
    "include/remolltypes.hh"           # list of classes to process
    "include/remollRunData.hh"
    "include/remollTextFile.hh"
    LINKDEF "include/remollLinkDef.h"  # ROOT linkDef file
    OPTIONS -p
)

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
  find_package(Geant4 REQUIRED ui_all vis_all)
else()
  find_package(Geant4 REQUIRED)
endif()

#----------------------------------------------------------------------------
# Setup Geant4 include directories and compile definitions
# Setup include directory for this project
#
include(${Geant4_USE_FILE})


#debugging symbols, complains
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -Wall -pg")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -Wall -g -Woverloaded-virtual")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -Wall -g -Wno-shadow -Wno-pedantic")

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
# Locate sources and headers for this project
# NB: headers are included so they will show up in IDEs
#
file(GLOB sources ${PROJECT_SOURCE_DIR}/src/remoll*.cc)
file(GLOB headers ${PROJECT_SOURCE_DIR}/include/remoll*.hh)

#----------------------------------------------------------------------------
# Add the executable, and link it to the Geant4 libraries
#
add_library(remoll-lib SHARED ${sources} ${headers} ${CMAKE_CURRENT_BINARY_DIR}/include/gitinfo.hh remollDict.cxx)
set_target_properties(remoll-lib PROPERTIES OUTPUT_NAME remoll)
target_link_libraries(remoll-lib ${Geant4_LIBRARIES} ${ROOT_LIBRARIES} ${Boost_LIBRARIES} )

add_executable(remoll-bin remoll.cc)
set_target_properties(remoll-bin PROPERTIES OUTPUT_NAME remoll)
target_link_libraries(remoll-bin remoll-lib)

add_executable(reroot-bin reroot.cc)
set_target_properties(reroot-bin PROPERTIES OUTPUT_NAME reroot)
target_link_libraries(reroot-bin remoll-lib)

#----------------------------------------------------------------------------
# Copy all scripts to the build directory, i.e. the directory in which we
# build remoll. This is so that we can run the executable directly because it
# relies on these scripts being in the current working directory.
#

FILE( GLOB REMOLL_GEO "geometry/*" )

set(REMOLL_SCRIPTS
    vis/OIX.mac
    vis/Qt.mac
    vis/vis.mac
    vis/X.mac
    macros/runexample.mac
    macros/gui.mac
    rootlogon.C
  )

foreach(_script ${REMOLL_SCRIPTS})
  configure_file(
    ${PROJECT_SOURCE_DIR}/${_script}
    ${PROJECT_BINARY_DIR}/${_script}
    COPYONLY
    )
endforeach()

#----------------------------------------------------------------------------
# For internal Geant4 use - but has no effect if you build this
# example standalone
#
add_custom_target(REMOLL DEPENDS remoll-bin)

#----------------------------------------------------------------------------
# Install the executable to 'bin' directory under CMAKE_INSTALL_PREFIX
#
install(TARGETS remoll-bin reroot-bin DESTINATION bin)
install(TARGETS remoll-lib DESTINATION lib)


