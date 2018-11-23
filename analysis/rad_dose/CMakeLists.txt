#----------------------------------------------------------------------------
# Setup the project
#
cmake_minimum_required(VERSION 2.6 FATAL_ERROR)
project(rad_doserad_dose)

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
    rad_doseDict                         # path to dictionary to generate
    "include/rad_dosetypes.hh"           # list of classes to process
    LINKDEF "include/rad_doseLinkDef.h"  # ROOT linkDef file
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
add_library(rad_dose-lib SHARED ${sources} ${headers} rad_doseDict.cxx)
set_target_properties(rad_dose-lib PROPERTIES OUTPUT_NAME rad_dose)
target_link_libraries(rad_dose-lib ${ROOT_LIBRARIES} ${REMOLL_LIBRARIES})

add_executable(rad_dose-bin rad_dose.cc)
set_target_properties(rad_dose-bin PROPERTIES OUTPUT_NAME rad_dose)
target_link_libraries(rad_dose-bin ${REMOLL_LIBRARIES} rad_dose-lib)

#----------------------------------------------------------------------------
#
add_custom_target(rad_dose DEPENDS rad_dose-bin)

#----------------------------------------------------------------------------
# Install the executable to 'bin' directory under CMAKE_INSTALL_PREFIX
#
install(TARGETS rad_dose-bin DESTINATION ${CMAKE_INSTALL_BINDIR})
install(TARGETS rad_dose-lib DESTINATION ${CMAKE_INSTALL_LIBDIR})
