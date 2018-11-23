#----------------------------------------------------------------------------
# Setup the project
#
cmake_minimum_required(VERSION 2.6 FATAL_ERROR)
project(pruneTreeEnvelope)

# Default install path is the source directory
if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    message(STATUS "    Install-prefix was at default -> forcing it to the source-dir" )
    message(STATUS "    Use -DCMAKE_INSTALL_PREFIX=/usr/local to set to something else" )
    set (CMAKE_INSTALL_PREFIX "${CMAKE_SOURCE_DIR}"
        CACHE PATH "default install path" FORCE )
endif()

# Use GNU install dirs
include(GNUInstallDirs)

list(APPEND CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}")

find_package(Remoll REQUIRED HINTS "${PROJECT_SOURCE_DIR}/../..")
include(${REMOLL_USE_FILE})
list(APPEND CMAKE_INSTALL_RPATH "${REMOLL_LIBRARY_DIR}")

list(APPEND CMAKE_INSTALL_RPATH "${REMOLL_LIBRARY_DIR}/../..")

find_package(ROOT REQUIRED)
include(${ROOT_USE_FILE})
list(APPEND CMAKE_INSTALL_RPATH "${ROOT_LIBRARY_DIR}")


#----------------------------------------------------------------------------
# generate dictionaries
#
ROOT_GENERATE_DICTIONARY(
    pruneTreeEnvelopeDict                         # path to dictionary to generate
    "include/pruneTreeEnvelopetypes.hh"           # list of classes to process
    LINKDEF "include/pruneTreeEnvelopeLinkDef.h"  # ROOT linkDef file
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
add_library(pruneTreeEnvelope-lib SHARED ${sources} ${headers} pruneTreeEnvelopeDict.cxx)
set_target_properties(pruneTreeEnvelope-lib PROPERTIES OUTPUT_NAME pruneTreeEnvelope)
target_link_libraries(pruneTreeEnvelope-lib ${ROOT_LIBRARIES} ${REMOLL_LIBRARIES})

add_executable(pruneTreeEnvelope-bin pruneTreeEnvelope.cc)
set_target_properties(pruneTreeEnvelope-bin PROPERTIES OUTPUT_NAME pruneTreeEnvelope)
target_link_libraries(pruneTreeEnvelope-bin ${REMOLL_LIBRARIES} pruneTreeEnvelope-lib)

#----------------------------------------------------------------------------
#
add_custom_target(pruneTreeEnvelope DEPENDS pruneTreeEnvelope-bin)

#----------------------------------------------------------------------------
# Install the executable to 'bin' directory under CMAKE_INSTALL_PREFIX
#
install(TARGETS pruneTreeEnvelope-bin DESTINATION ${CMAKE_INSTALL_BINDIR})
install(TARGETS pruneTreeEnvelope-lib DESTINATION ${CMAKE_INSTALL_LIBDIR})
