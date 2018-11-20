#----------------------------------------------------------------------------
# Setup the project
#
cmake_minimum_required(VERSION 2.6 FATAL_ERROR)
project(ANALYSIS)

set(REMOLL_INSTALL "~/git/remoll/")

set(CMAKE_MODULE_PATH "${REMOLL_INSTALL}/cmake/modules")

find_package(REMOLL REQUIRED)
include_directories(${REMOLL_INCLUDE_DIR})

find_package(ROOT REQUIRED)
include_directories(${ROOT_INCLUDE_DIR})


include_directories(${CMAKE_CURRENT_BINARY_DIR}/include)

#----------------------------------------------------------------------------
# generate dictionaries
ROOT_GENERATE_DICTIONARY(
    peDict                         # path to dictionary to generate
    "include/petypes.hh"           # list of classes to process
    LINKDEF "include/peLinkDef.h"  # ROOT linkDef file
    OPTIONS -p
)

#----------------------------------------------------------------------------
# Debugging symbols, warnings
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -Wall -pg")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -Wall -g -Woverloaded-virtual")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -Wall -g -Wno-shadow -Wno-pedantic")

include_directories(${PROJECT_SOURCE_DIR}/include)

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

# pe counting script
#add_library(remoll-lib SHARED ${sources} ${headers} remollDict.cxx)
#set_target_properties(remoll-lib PROPERTIES OUTPUT_NAME remoll)
#target_link_libraries(remoll-lib ${ROOT_LIBRARIES} )

add_library(pe-lib SHARED ${sources} ${headers} peDict.cxx)
set_target_properties(pe-lib PROPERTIES OUTPUT_NAME pe)
target_link_libraries(pe-lib ${ROOT_LIBRARIES} ${REMOLL_LIBRARIES})

add_executable(pe-bin pe.cc)
set_target_properties(pe-bin PROPERTIES OUTPUT_NAME pe)
target_link_libraries(pe-bin pe-lib)

#----------------------------------------------------------------------------
#
add_custom_target(ANALYSIS DEPENDS pe-bin)

#----------------------------------------------------------------------------
# Install the executable to 'bin' directory under CMAKE_INSTALL_PREFIX
#
install(TARGETS pe-bin DESTINATION bin)
install(TARGETS pe-lib DESTINATION lib)
