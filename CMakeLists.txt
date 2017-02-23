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

# C++11 standard if available
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
    geometry_sculpt/hallDaughter.gdml
    geometry_sculpt/mollerdet.gdml
    geometry_sculpt/materials.xml
    geometry_sculpt/upstreamDaughter_merged_design1.gdml
    geometry_sculpt/upstreamDaughter_merged_design1_krypto.gdml
    geometry_sculpt/upstreamDaughter_merged_design1_check3.gdml
    geometry_sculpt/schema/gdml.xsd
    geometry_sculpt/schema/gdml_core.xsd
    geometry_sculpt/schema/gdml_define.xsd
    geometry_sculpt/schema/gdml_extensions.xsd
    geometry_sculpt/schema/gdml_materials.xsd
    geometry_sculpt/schema/gdml_parameterised.xsd
    geometry_sculpt/schema/gdml_replicas.xsd
    geometry_sculpt/schema/gdml_solids.xsd

    geometry_sculpt_less_hybshld/mollerMother.gdml
    geometry_sculpt_less_hybshld/detectorDaughter.gdml
    geometry_sculpt_less_hybshld/hybridDaughter.gdml
    geometry_sculpt_less_hybshld/hybridDaughter_less_hybshld.gdml
    geometry_sculpt_less_hybshld/hybridDaughter_less_hybshld_roof.gdml
    geometry_sculpt_less_hybshld/hybridDaughter_less_hybshld_roof_less_1.gdml
    geometry_sculpt_less_hybshld/hybridDaughter_less_hybshld_roof_less_2.gdml
    geometry_sculpt_less_hybshld/targetDaughter.gdml
    geometry_sculpt_less_hybshld/upstreamDaughter.gdml
    geometry_sculpt_less_hybshld/hallDaughter.gdml
    geometry_sculpt_less_hybshld/mollerdet.gdml
    geometry_sculpt_less_hybshld/materials.xml
    geometry_sculpt_less_hybshld/upstreamDaughter_merged_design1.gdml
    geometry_sculpt_less_hybshld/upstreamDaughter_merged_design1_krypto.gdml
    geometry_sculpt_less_hybshld/upstreamDaughter_merged_design1_check3.gdml
    geometry_sculpt_less_hybshld/schema/gdml.xsd
    geometry_sculpt_less_hybshld/schema/gdml_core.xsd
    geometry_sculpt_less_hybshld/schema/gdml_define.xsd
    geometry_sculpt_less_hybshld/schema/gdml_extensions.xsd
    geometry_sculpt_less_hybshld/schema/gdml_materials.xsd
    geometry_sculpt_less_hybshld/schema/gdml_parameterised.xsd
    geometry_sculpt_less_hybshld/schema/gdml_replicas.xsd
    geometry_sculpt_less_hybshld/schema/gdml_solids.xsd

    geometry_sculpt_less_targshld/mollerMother.gdml
    geometry_sculpt_less_targshld/detectorDaughter.gdml
    geometry_sculpt_less_targshld/hybridDaughter.gdml
    geometry_sculpt_less_targshld/targetDaughter.gdml
    geometry_sculpt_less_targshld/targetDaughter_less_targshld.gdml
    geometry_sculpt_less_targshld/upstreamDaughter.gdml
    geometry_sculpt_less_targshld/hallDaughter.gdml
    geometry_sculpt_less_targshld/mollerdet.gdml
    geometry_sculpt_less_targshld/materials.xml
    geometry_sculpt_less_targshld/upstreamDaughter_merged_design1.gdml
    geometry_sculpt_less_targshld/upstreamDaughter_merged_design1_krypto.gdml
    geometry_sculpt_less_targshld/upstreamDaughter_merged_design1_check3.gdml
    geometry_sculpt_less_targshld/schema/gdml.xsd
    geometry_sculpt_less_targshld/schema/gdml_core.xsd
    geometry_sculpt_less_targshld/schema/gdml_define.xsd
    geometry_sculpt_less_targshld/schema/gdml_extensions.xsd
    geometry_sculpt_less_targshld/schema/gdml_materials.xsd
    geometry_sculpt_less_targshld/schema/gdml_parameterised.xsd
    geometry_sculpt_less_targshld/schema/gdml_replicas.xsd
    geometry_sculpt_less_targshld/schema/gdml_solids.xsd
 
    geometry_sculpt_wshield/mollerMother.gdml
    geometry_sculpt_wshield/detectorDaughter.gdml
    geometry_sculpt_wshield/hybridDaughter.gdml
    geometry_sculpt_wshield/targetDaughter.gdml
    geometry_sculpt_wshield/upstreamDaughter.gdml
    geometry_sculpt_wshield/shieldDaughter.gdml
    geometry_sculpt_wshield/schema/gdml.xsd
    geometry_sculpt_wshield/schema/gdml_core.xsd
    geometry_sculpt_wshield/schema/gdml_define.xsd
    geometry_sculpt_wshield/schema/gdml_extensions.xsd
    geometry_sculpt_wshield/schema/gdml_materials.xsd
    geometry_sculpt_wshield/schema/gdml_parameterised.xsd
    geometry_sculpt_wshield/schema/gdml_replicas.xsd
    geometry_sculpt_wshield/schema/gdml_solids.xsd
    geometry_sculpt_wshield_cor/mollerMother.gdml
    geometry_sculpt_wshield_cor/detectorDaughter.gdml
    geometry_sculpt_wshield_cor/hybridDaughter.gdml
    geometry_sculpt_wshield_cor/targetDaughter.gdml
    geometry_sculpt_wshield_cor/upstreamDaughter.gdml
    geometry_sculpt_wshield_cor/shieldDaughter.gdml
    geometry_sculpt_wshield_cor/schema/gdml.xsd
    geometry_sculpt_wshield_cor/schema/gdml_core.xsd
    geometry_sculpt_wshield_cor/schema/gdml_define.xsd
    geometry_sculpt_wshield_cor/schema/gdml_extensions.xsd
    geometry_sculpt_wshield_cor/schema/gdml_materials.xsd
    geometry_sculpt_wshield_cor/schema/gdml_parameterised.xsd
    geometry_sculpt_wshield_cor/schema/gdml_replicas.xsd
    geometry_sculpt_wshield_cor/schema/gdml_solids.xsd


    vis/OIX.mac
    vis/Qt.mac
    vis/vis.mac
    vis/X.mac
    macros/runexample.mac
    macros/gui.mac
    macros/moller_dose.mac
    macros/vis.mac
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


