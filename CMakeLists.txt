#----------------------------------------------------------------------------
# Setup the project
#
cmake_minimum_required(VERSION 3.8)
project(remoll VERSION 0.1 LANGUAGES CXX C)

set(CMAKE_CXX_STANDARD 14)
# Export compile commands as json for run-clang-tidy
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Also use clang-tidy integration in CMake
option(ENABLE_CLANG_TIDY "Enable clang-tidy integration in cmake" OFF)
if(ENABLE_CLANG_TIDY)
  find_program(CLANG_TIDY_EXE NAMES "clang-tidy")
  if (CLANG_TIDY_EXE)
    message(STATUS "clang-tidy found: ${CLANG_TIDY_EXE}")
    set(CMAKE_CXX_CLANG_TIDY "${CLANG_TIDY_EXE}" CACHE STRING "" FORCE)
  else()
    set(CMAKE_CXX_CLANG_TIDY "" CACHE STRING "" FORCE)
  endif()
endif()

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

# For remollSearchPath.hh
# FIXME
# link_libraries(stdc++fs)

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
find_package(Boost QUIET OPTIONAL_COMPONENTS iostreams filesystem)
if(Boost_FOUND)
  message(STATUS "Boost found --> building with boost enabled.")
  include_directories(${Boost_INCLUDE_DIRS})
  link_directories(${Boost_LIBRARY_DIR})
  ADD_DEFINITIONS(-D__USE_BOOST)
  if(Boost_IOSTREAMS_FOUND)
    message(STATUS "Boost::iostreams found --> building with boost::iostreams enabled.")
    #ADD_DEFINITIONS(-D__USE_BOOST_IOSTREAMS)
  endif()
  if(Boost_FILESYSTEM_FOUND)
    message(STATUS "Boost::filesystem found --> building with boost::filesystem enabled.")
    ADD_DEFINITIONS(-D__USE_BOOST_FILESYSTEM)
  endif()
else()
  message(STATUS "Boost not found.")
endif()

#----------------------------------------------------------------------------
# Find CADMesh libraries
set(cadmesh_LIBRARIES "")
find_package(cadmesh QUIET)
if(cadmesh_FOUND)
    # Pretty sure at some point cadmesh will update to use lowercase
    include_directories(${CADMESH_INCLUDE_DIRS})
    include_directories(${cadmesh_INCLUDE_DIRS})
    link_directories(${cadmesh_LIBRARY_DIR})
    ADD_DEFINITIONS(-D__USE_CADMESH)

    find_path(TETGEN_INCLUDE_DIRS tetgen.h
        PATHS
        $ENV{TETGEN_PATH}
        $ENV{TETGEN_PATH}/tetgen
        /usr/local/include
        /usr/include)
    find_library(TETGEN_LIBRARIES NAMES tetgen tet
        PATHS
        $ENV{TETGEN_PATH}
        $ENV{TETGEN_PATH}/tetgen
        /usr/local/lib
        /usr/lib)
    if(TETGEN_INCLUDE_DIRS)
        if(TETGEN_LIBRARIES)
            include_directories(${TETGEN_INCLUDE_DIRS})
            ADD_DEFINITIONS(-D__USE_CADMESH_TETGEN)
        endif()
    endif()

    find_package(assimp QUIET)
    if(assimp_FOUND)
        include_directories(${ASSIMP_INCLUDE_DIRS})
        ADD_DEFINITIONS(-D__USE_CADMESH_ASSIMP)
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
    ${cadmesh_LIBRARIES}
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
set(REMOLL_DOWNLOADS https://hallaweb.jlab.org/12GeV/Moller/downloads/remoll/)
set(REMOLL_MAP_DIR ${PROJECT_SOURCE_DIR}/map_directory)
if(EXISTS ${REMOLL_LOCATION})
  message(STATUS "Fieldmaps will be copied from directory...")
  set(REMOLL_DOWNLOADS file://${REMOLL_LOCATION})
else()
  message(STATUS "Fieldmaps will be downloaded from web...")
endif()
if(Boost_IOSTREAMS_FOUND)
  message(STATUS "Gzipped fieldmaps will be made available...")
endif()
# for i in *.txt ; do echo `md5sum $i|awk '{print$1}'`:`md5sum $i.gz|awk '{print$1}'`:$i ; done
set(REMOLL_MAPS
b8b30b803bae42f69135323ff40fe7db:99bf3fae83677c6c2f3e09a871682b2e:V2U.1a.50cm.parallel.txt
9f97ba09601fc26676c1fd6ebd44731d:070b004ffd281ebceee0b6eaaa847edf:DS_TM1-4_CoilA-G_ll_TM2-4_out3mm.txt
)
set(REMOLL_MAPS_EXTRA
e4a96c2f3136d0eb020d90e74a532a6d:246d48001dd71174b62e16ecf07adc4d:V2U.1a.50cm.parallel.real_asymmetric.txt
90b2096b971970abfeb70f896e7fa7c0:7a57aede9c2b98bba047d424edf38eb4:US_Asym_CP2B_pm_1_3.txt
c062261310e041ddfddd692faba1a5d7:5a7a9ca389d29b12d61dfd7f9b67a3d8:DS_TM1-4_CoilA-G_ll_TM2-4_out3mm_asym_case_I.txt
99196c749da1f2516772639df01c0c62:d5a890bddc8b6b6b1a3c4708cf0298d5:DS_TM1-4_CoilA-G_ll_TM2-4_out3mm_asym_case_II.txt
03d70a984b51194b6b1d049862f6e56e:d550945c74c23a4515dfa0ee03d7d5d7:DS_Asym_CP2B_pm_1_3.txt
)
if(ADDITIONAL_FIELDS)
  message(STATUS "Ensuring additional fields are available.")
else()
  message(STATUS "Download additional fields with '-DADDITIONAL_FIELDS=ON'.")
  set(REMOLL_MAPS_EXTRA "")
endif()
# setting User-agent HTTP header
set(_HTTP_HDR "User-Agent: Mozilla/5.0")
foreach(MAP IN LISTS REMOLL_MAPS REMOLL_MAPS_EXTRA)
  set(REGEX "^(.*):(.*):(.*)$")
  # Hash 1: txt file, Hash 2: txt.gz file
  string(REGEX REPLACE ${REGEX} "\\1" HASH1 ${MAP})
  string(REGEX REPLACE ${REGEX} "\\2" HASH2 ${MAP})
  string(REGEX REPLACE ${REGEX} "\\3" FILE ${MAP})
  if(Boost_IOSTREAMS_FOUND)
    file(DOWNLOAD
      ${REMOLL_DOWNLOADS}/${FILE}.gz
      ${REMOLL_MAP_DIR}/${FILE}.gz
      EXPECTED_MD5 ${HASH2}
      HTTPHEADER ${_HTTP_HDR}
      )
    install(FILES
      ${REMOLL_MAP_DIR}/${FILE}.gz
      DESTINATION ${CMAKE_INSTALL_DATADIR}/remoll/map_directory)
  endif()
  #else()
    file(DOWNLOAD
      ${REMOLL_DOWNLOADS}/${FILE}
      ${REMOLL_MAP_DIR}/${FILE}
      EXPECTED_MD5 ${HASH1}
      HTTPHEADER ${_HTTP_HDR}
      )
    install(FILES
      ${REMOLL_MAP_DIR}/${FILE}
      DESTINATION ${CMAKE_INSTALL_DATADIR}/remoll/map_directory)
  #endif()
endforeach()


#----------------------------------------------------------------------------
# Install cmake config file
#
install(FILES
    ${PROJECT_SOURCE_DIR}/cmake/scripts/RemollConfig.cmake
    ${PROJECT_SOURCE_DIR}/cmake/scripts/RemollUseFile.cmake
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/remoll)

#----------------------------------------------------------------------------
# remollSearchPath

add_definitions(
        -DCMAKE_INSTALL_PREFIX=\"${CMAKE_INSTALL_PREFIX}\"
        -DCMAKE_INSTALL_FULL_DATADIR=\"${CMAKE_INSTALL_FULL_DATADIR}\"
)

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

