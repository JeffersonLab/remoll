#----------------------------------------------------------------------------

# Setup the project
#
cmake_minimum_required(VERSION 2.6 FATAL_ERROR)
project(REMOLL)


#ROOT path
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/modules")
find_package(ROOT REQUIRED)

# Update header file
add_custom_target( gitinfo ALL COMMAND ${PROJECT_SOURCE_DIR}/pullgitinfo.py ${PROJECT_SOURCE_DIR} COMMENT "Creating gitinfo.hh" )


# this is to ensure we find the header we need
include_directories(${CMAKE_CURRENT_BINARY_DIR}/include)

ROOT_GENERATE_DICTIONARY(
    remollDict                         # path to dictionary to generate
    "include/remollRunData.hh"         # list of classes to process
    "include/remollTextFile.hh"
    LINKDEF "include/remollLinkDef.h"  # ROOT linkDef file
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
set(CMAKE_CXX_FLAGS "-g -Wall -pg")

#----------------------------------------------------------------------------
# Generate dictionaries as well (so we can write objects to ROOT files

include_directories(${PROJECT_SOURCE_DIR}/include ${ROOT_INCLUDE_DIR} ${CMAKE_CURRENT_BINARY_DIR}/include)


#----------------------------------------------------------------------------
# Locate sources and headers for this project
# NB: headers are included so they will show up in IDEs
#
file(GLOB sources ${PROJECT_SOURCE_DIR}/src/remoll*.cc)
file(GLOB headers ${PROJECT_SOURCE_DIR}/include/*.hh ${CMAKE_CURRENT_BINARY_DIR}/include/gitinfo.hh)

file(GLOB libsources ${PROJECT_SOURCE_DIR}/src/remollRunData.cc ${PROJECT_SOURCE_DIR}/src/remollTextFile.cc)
file(GLOB libheaders ${PROJECT_SOURCE_DIR}/include/*.hh ${CMAKE_CURRENT_BINARY_DIR}/include/gitinfo.hh)

#----------------------------------------------------------------------------
# Add the executable, and link it to the Geant4 libraries
#
add_executable(remoll remoll.cc ${sources} ${headers} remollDict.cxx)
add_library(remollroot SHARED ${libsources} ${libheaders} remollDict.cxx)
target_link_libraries(remoll ${Geant4_LIBRARIES} ${ROOT_LIBRARIES} ${Boost_LIBRARIES} )
target_link_libraries(remollroot ${ROOT_LIBRARIES} ${Boost_LIBRARIES} )

#----------------------------------------------------------------------------
# Copy all scripts to the build directory, i.e. the directory in which we
# build remoll. This is so that we can run the executable directly because it
# relies on these scripts being in the current working directory.
#

FILE( GLOB REMOLL_GEO "geometry/*" )

set(REMOLL_SCRIPTS
    geometry/mollerMother.gdml
    geometry/detectorDaughter.gdml
    geometry/hybridDaughter.gdml
    geometry/targetDaughter.gdml
    geometry/upstreamDaughter.gdml
    geometry/schema/gdml.xsd
    geometry/schema/gdml_core.xsd
    geometry/schema/gdml_define.xsd
    geometry/schema/gdml_extensions.xsd
    geometry/schema/gdml_materials.xsd
    geometry/schema/gdml_parameterised.xsd
    geometry/schema/gdml_replicas.xsd
    geometry/schema/gdml_solids.xsd
    geometry_sculpt/mollerMother.gdml
    geometry_sculpt/detectorDaughter.gdml
    geometry_sculpt/hybridDaughter.gdml
    geometry_sculpt/targetDaughter.gdml
    geometry_sculpt/upstreamDaughter.gdml
    geometry_sculpt/schema/gdml.xsd
    geometry_sculpt/schema/gdml_core.xsd
    geometry_sculpt/schema/gdml_define.xsd
    geometry_sculpt/schema/gdml_extensions.xsd
    geometry_sculpt/schema/gdml_materials.xsd
    geometry_sculpt/schema/gdml_parameterised.xsd
    geometry_sculpt/schema/gdml_replicas.xsd
    geometry_sculpt/schema/gdml_solids.xsd
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
add_custom_target(REMOLL DEPENDS remoll)

#----------------------------------------------------------------------------
# Install the executable to 'bin' directory under CMAKE_INSTALL_PREFIX
#
install(TARGETS remoll DESTINATION bin)


