#----------------------------------------------------------------------------
# Setup the project
#
cmake_minimum_required(VERSION 2.6 FATAL_ERROR)
project(hallRad)

set(REMOLL_INSTALL "../..")
find_package(exe REQUIRED)
include_directories(${REMOLL_INCLUDE_DIR})

find_package(ROOT REQUIRED)
include_directories(${ROOT_INCLUDE_DIR})

#----------------------------------------------------------------------------
# generate dictionaries
#
ROOT_GENERATE_DICTIONARY(
    hallRadDict                         # path to dictionary to generate
    "include/hallRadtypes.hh"           # list of classes to process
    LINKDEF "include/hallRadLinkDef.h"  # ROOT linkDef file
    OPTIONS -p
)

#----------------------------------------------------------------------------
#
include_directories(${PROJECT_SOURCE_DIR}/include ${REMOLL_INCLUDE_DIR})

#----------------------------------------------------------------------------
# Locate sources and headers for this project
# NB: headers are included so they will show up in IDEs
#
file(GLOB sources ${PROJECT_SOURCE_DIR}/src/*.cc)
file(GLOB headers ${PROJECT_SOURCE_DIR}/include/*.hh)

#----------------------------------------------------------------------------
# Add the executable, and link it to the Geant4 libraries
#
#----------------------------------------------------------------------------
# Generate dictionaries as well (so we can write objects to ROOT files
#
add_library(hallRad-lib SHARED ${sources} ${headers} hallRadDict.cxx)
set_target_properties(hallRad-lib PROPERTIES OUTPUT_NAME hallRad)
target_link_libraries(hallRad-lib ${ROOT_LIBRARIES} ${REMOLL_LIBRARIES})

add_executable(hallRad-bin hallRad.cc)
set_target_properties(hallRad-bin PROPERTIES OUTPUT_NAME hallRad)
target_link_libraries(hallRad-bin ${REMOLL_BINARY_DIR}/libremoll.so hallRad-lib)
#target_link_libraries(hallRad-bin hallRad-lib ${REMOLL_LIBRARIES})

#----------------------------------------------------------------------------
#
add_custom_target(hallRad DEPENDS hallRad-bin)

#----------------------------------------------------------------------------
# Install the executable to 'bin' directory under CMAKE_INSTALL_PREFIX
#
install(TARGETS hallRad-bin DESTINATION bin)
install(TARGETS hallRad-lib DESTINATION lib)

