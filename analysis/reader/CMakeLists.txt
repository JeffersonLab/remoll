#----------------------------------------------------------------------------
# Setup the project
#
cmake_minimum_required(VERSION 2.6 FATAL_ERROR)
project(reader)

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
    readerDict                         # path to dictionary to generate
    "include/readertypes.hh"           # list of classes to process
    LINKDEF "include/readerLinkDef.h"  # ROOT linkDef file
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
add_library(reader-lib SHARED ${sources} ${headers} readerDict.cxx)
set_target_properties(reader-lib PROPERTIES OUTPUT_NAME reader)
target_link_libraries(reader-lib ${ROOT_LIBRARIES} ${REMOLL_LIBRARIES})

add_executable(reader-bin reader.cc)
set_target_properties(reader-bin PROPERTIES OUTPUT_NAME reader)
target_link_libraries(reader-bin ${REMOLL_LIBRARIES} reader-lib)

#----------------------------------------------------------------------------
#
add_custom_target(reader DEPENDS reader-bin)

#----------------------------------------------------------------------------
# Install the executable to 'bin' directory under CMAKE_INSTALL_PREFIX
#
install(TARGETS reader-bin DESTINATION ${CMAKE_INSTALL_BINDIR})
install(TARGETS reader-lib DESTINATION ${CMAKE_INSTALL_LIBDIR})
