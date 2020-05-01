#----------------------------------------------------------------------------
# Setup the project
#
cmake_minimum_required(VERSION 3.5)
project(remoll VERSION 0.1 LANGUAGES CXX C)

# Disallow in-source builds
if(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_BINARY_DIR})
  message(FATAL_ERROR "remoll requires an out-of-source build.")
endif()

# Prepend module search path
set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/modules ${CMAKE_MODULE_PATH})

# Use our modules and utility functions
include(BuildType)
include(PrefixDir)
include(CMakeEnv)

# Use GNU install dirs
include(GNUInstallDirs)

# Request C++11 standard
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_EXTENSIONS False)

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


#----------------------------------------------------------------------------
# gitinfo.cc
#
set(PRE_CONFIGURE_FILE ${PROJECT_SOURCE_DIR}/cmake/templates/gitinfo.cc.in)
set(POST_CONFIGURE_FILE ${CMAKE_BINARY_DIR}/gitinfo.cc)
include(CMakeGitInfo)
list(APPEND sources ${POST_CONFIGURE_FILE})


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
# Boost
#
find_package(Boost QUIET COMPONENTS iostreams)
if(Boost_FOUND)
  message(STATUS "Boost found --> building with boost enabled.")
  include_directories(${Boost_INCLUDE_DIRS})
  link_directories(${Boost_LIBRARY_DIR})
  ADD_DEFINITIONS(-D__USE_BOOST)
  if(Boost_IOSTREAMS_FOUND)
    message(STATUS "Boost::iostreams found --> building with boost::iostreams enabled.")
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
# HepMC (optional)
#
find_package(HepMC QUIET)
if(HepMC_FOUND)
  message(STATUS "HepMC found --> building with HepMC enabled.")
  add_definitions(-DG4LIB_USE_HEPMC)
  file(GLOB hepmc_sources src/HepMC*.cc)
  file(GLOB hepmc_headers include/HepMC*.hh)
  list(APPEND sources ${hepmc_sources})
  list(APPEND headers ${hepmc_headers})
else()
  set(HEPMC_LIBRARIES "")
endif()


#----------------------------------------------------------------------------
# Pythia6 (optional)
#
find_package(Pythia6 QUIET)
if(PYTHIA6_FOUND)
  message(STATUS "Pythia6 found --> building with Pythia6 enabled.")
  add_definitions(-DG4LIB_USE_PYTHIA)
else()
  set(PYTHIA6_LIBRARIES "")
endif()


#----------------------------------------------------------------------------
# Compiler flags
#
# Set compiler flags to the set given by ROOT set
set_compiler_flags("${ROOT_CXX_FLAGS}")
# FIXME: The following will at some point all disappear (yeah yeah, soon)
# Ignore shadowing of CLHEP 1-letter units with 1-letter local variables
string(APPEND ${PROJECT_NAME_UC}_DIAG_FLAGS " -Wno-shadow")
# Ignore missing field initializers in structs
string(APPEND ${PROJECT_NAME_UC}_DIAG_FLAGS " -Wno-missing-field-initializers")
# Ignore unused parameters in many geant4 functions
string(APPEND ${PROJECT_NAME_UC}_DIAG_FLAGS " -Wno-unused-parameter")
if(CMAKE_CXX_COMPILER_VERSION VERSION_LESS 5.0)
  # Ignore warning of struct initialization { } for gcc < 5.0 (after that it is ignored internally)
  string(APPEND ${PROJECT_NAME_UC}_DIAG_FLAGS " -Wno-missing-field-initializers")
endif()
# Now add the standard WALL and WEXTRA sets
set_diagnostic_flags(WALL WEXTRA)
# Finally print what we use
report_build_info()


#----------------------------------------------------------------------------
# main library
#
add_library(lib${PROJECT_NAME} SHARED ${sources} ${headers})
add_dependencies(lib${PROJECT_NAME} check_git_repository)
set_target_properties(lib${PROJECT_NAME} PROPERTIES OUTPUT_NAME remoll)

target_include_directories(lib${PROJECT_NAME}
  PUBLIC
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
  )

target_compile_options(lib${PROJECT_NAME}
  PUBLIC
    ${${PROJECT_NAME_UC}_CXX_FLAGS_LIST}
  PRIVATE
    ${${PROJECT_NAME_UC}_DIAG_FLAGS_LIST}
  )

target_link_libraries(lib${PROJECT_NAME}
  PUBLIC
    ROOT::Libraries
    ${Geant4_LIBRARIES}
    ${Boost_LIBRARIES}
    ${HEPMC_LIBRARIES}
    ${PYTHIA6_LIBRARIES}
  )

install(TARGETS lib${PROJECT_NAME}
  EXPORT ${MAIN_PROJECT_NAME_LC}-exports
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  )
install(FILES ${headers} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})


#----------------------------------------------------------------------------
# executables
#
file(GLOB exefiles *.cc)
foreach(file ${exefiles})
  get_filename_component(filename ${file} NAME_WE)
  string(TOLOWER ${filename} filelower)

  add_executable(${filelower} ${file})

  target_link_libraries(${filelower}
    PRIVATE
      lib${PROJECT_NAME}
  )
  target_compile_options(${filelower}
    PUBLIC
      ${${PROJECT_NAME_UC}_CXX_FLAGS_LIST}
    PRIVATE
      ${${PROJECT_NAME_UC}_DIAG_FLAGS_LIST}
  )
  if(${CMAKE_SYSTEM_NAME} MATCHES Linux)
    target_compile_options(${filelower} PUBLIC -fPIC)
  endif()

  install(TARGETS ${filelower}
    DESTINATION ${CMAKE_INSTALL_BINDIR}
  )
endforeach()

#
# custom targets
#
add_custom_target(create-manual
  COMMAND ./remoll -g ${PROJECT_SOURCE_DIR}/geometry/mollerMother.gdml ${PROJECT_SOURCE_DIR}/macros/manual.mac | awk '/Command directory path/,/Graphics systems deleted/' > manual.txt
  BYPRODUCTS manual.txt
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS remoll)
add_custom_target(check-manual
  COMMAND diff ${PROJECT_SOURCE_DIR}/manual.txt manual.txt || true
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS create-manual)

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
     ${REMOLL_DOWNLOADS}/hybridJLAB.txt
     ${REMOLL_MAP_DIR}/hybridJLAB.txt
     EXPECTED_MD5 c2da18fd7ab80cc4abe7eafc487963dc
     )
file(DOWNLOAD
     ${REMOLL_DOWNLOADS}/upstreamJLAB_1.25.txt
     ${REMOLL_MAP_DIR}/upstreamJLAB_1.25.txt
     EXPECTED_MD5 af06ed35516c17640e89ba3aa0f5b200
     )
file(DOWNLOAD
     ${REMOLL_DOWNLOADS}/blockyUpstream_rm_1.1.txt
     ${REMOLL_MAP_DIR}/blockyUpstream_rm_1.1.txt
     EXPECTED_MD5 3e2338e1ba74b03da37545e98931f5f3
     )
file(DOWNLOAD
     ${REMOLL_DOWNLOADS}/blockyHybrid_rm_3.0.txt
     ${REMOLL_MAP_DIR}/blockyHybrid_rm_3.0.txt
     EXPECTED_MD5 b4bfef8f362e0df66f166b4e76a6847e
     )
install(FILES
     ${REMOLL_MAP_DIR}/hybridJLAB.txt
     ${REMOLL_MAP_DIR}/upstreamJLAB_1.25.txt
     ${REMOLL_MAP_DIR}/blockyHybrid_rm_3.0.txt
     ${REMOLL_MAP_DIR}/blockyUpstream_rm_1.1.txt
     DESTINATION ${CMAKE_INSTALL_DATADIR}/remoll)
if(ADDITIONAL_FIELDS)
  message(STATUS "Ensuring additional fields are available")
  file(DOWNLOAD
     ${REMOLL_DOWNLOADS}/upstreamSymmetric_sensR_0.1.txt
     ${REMOLL_MAP_DIR}/upstreamSymmetric_sensR_0.1.txt
     EXPECTED_MD5 849d9dc5abab0842fc13fef7f4918648
     )
  file(DOWNLOAD
     ${REMOLL_DOWNLOADS}/hybridSymmetric_sensR_0.1.txt
     ${REMOLL_MAP_DIR}/hybridSymmetric_sensR_0.1.txt
     EXPECTED_MD5 78fad2ffa5b5ae129df11bdf0ce25333
     )
  install(FILES
     ${REMOLL_MAP_DIR}/upstreamSymmetric_sensR_0.1.txt
     ${REMOLL_MAP_DIR}/hybridSymmetric_sensR_0.1.txt
     DESTINATION ${CMAKE_INSTALL_DATADIR}/remoll)
else()
  message(STATUS "Download additional fields with '-DADDITIONAL_FIELDS=ON'.")
endif()


#----------------------------------------------------------------------------
# Install cmake config file
#
install(FILES
    ${PROJECT_SOURCE_DIR}/cmake/scripts/RemollConfig.cmake
    ${PROJECT_SOURCE_DIR}/cmake/scripts/RemollUseFile.cmake
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/remoll)


#----------------------------------------------------------------------------
# uninstall
#
if(NOT TARGET uninstall)
  configure_file(
      "${CMAKE_CURRENT_SOURCE_DIR}/cmake/scripts/cmake_uninstall.cmake.in"
      "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
      IMMEDIATE @ONLY)
  add_custom_target(uninstall
      COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)
endif()

