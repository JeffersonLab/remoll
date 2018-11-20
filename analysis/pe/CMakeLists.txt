#----------------------------------------------------------------------------
# Setup the project
#
cmake_minimum_required(VERSION 2.6 FATAL_ERROR)
project(pe)

set(REMOLL_INSTALL "../..")
find_package(remoll REQUIRED)
include_directories(${REMOLL_INCLUDE_DIR})

find_package(ROOT REQUIRED)
include_directories(${ROOT_INCLUDE_DIR})

#----------------------------------------------------------------------------
# generate dictionaries
#
ROOT_GENERATE_DICTIONARY(
    peDict                         # path to dictionary to generate
    "include/petypes.hh"           # list of classes to process
    LINKDEF "include/peLinkDef.h"  # ROOT linkDef file
    OPTIONS -p
)

#----------------------------------------------------------------------------
#
include_directories(${PROJECT_SOURCE_DIR}/include ${REMOLL_INCLUDE_DIR})

#----------------------------------------------------------------------------
# Locate sources and headers for this project
# NB: headers are included so they will show up in IDEs
#
file(GLOB sources ${PROJECT_SOURCE_DIR}/src/pe*.cc)
file(GLOB headers ${PROJECT_SOURCE_DIR}/include/pe*.hh)

#----------------------------------------------------------------------------
# Add the executable, and link it to the Geant4 libraries
#
#----------------------------------------------------------------------------
# Generate dictionaries as well (so we can write objects to ROOT files
#
add_library(pe-lib SHARED ${sources} ${headers} peDict.cxx)
set_target_properties(pe-lib PROPERTIES OUTPUT_NAME pe)
target_link_libraries(pe-lib ${ROOT_LIBRARIES} ${REMOLL_LIBRARIES})

add_executable(pe-bin pe.cc)
set_target_properties(pe-bin PROPERTIES OUTPUT_NAME pe)
target_link_libraries(pe-bin ${REMOLL_BINARY_DIR}/libremoll.so pe-lib)
#target_link_libraries(pe-bin pe-lib ${REMOLL_LIBRARIES})

#----------------------------------------------------------------------------
#
add_custom_target(pe DEPENDS pe-bin)

#----------------------------------------------------------------------------
# Install the executable to 'bin' directory under CMAKE_INSTALL_PREFIX
#
install(TARGETS pe-bin DESTINATION bin)
install(TARGETS pe-lib DESTINATION lib)
