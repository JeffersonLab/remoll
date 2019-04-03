# Find ROOT installation. We supply or own search module because
#
# (a) ROOT 5 does not provide a CMake configuration module, and we still
#     want to support ROOT 5 for the moment; and
# (b) ROOT 6's ROOTConfig.cmake is a bit too heavy for our purposes.
#
# Among other things, ROOT 6's ROOTConfig-targets.cmake, which
# is invoked by ROOTConfig.cmake, requires all components to be present,
# which clashes with partial RPM installs from EPEL on RHEL. Using ROOT's
# CMake configuration always requires users to install all components built
# by the packager, even those that are never needed. We don't want that.

if(NOT TARGET ROOT::Libraries)

if(NOT ROOT_CONFIG_EXEC)
  # Only execute this if not already done
  # (spawning a bunch of root-config processes is quite slow)

  # Find root-config either in $CMAKE_PREFIX_PATH/bin, $ROOTSYS/bin, if defined,
  # or in $PATH.
  if(DEFINED ENV{ROOTSYS})
    set(ROOTBIN $ENV{ROOTSYS}/bin)
  endif()
  find_program(ROOT_CONFIG_EXEC root-config
    HINTS "${ROOTBIN}"
    DOC "ROOT configuration utility program"
    NO_CMAKE_ENVIRONMENT_PATH 
    )
  unset(ROOTBIN)

  if(ROOT_CONFIG_EXEC)
    execute_process(
      COMMAND ${ROOT_CONFIG_EXEC} --prefix
      OUTPUT_VARIABLE _rootsys
      OUTPUT_STRIP_TRAILING_WHITESPACE
      )
    set(ROOTSYS ${_rootsys} CACHE PATH "ROOT installation prefix" FORCE)
    #message( STATUS "ROOTSYS = ${ROOTSYS}" )
  else()
    message(FATAL_ERROR "Cannot find ROOT")
  endif()
  unset(_rootsys)
  mark_as_advanced(ROOT_CONFIG_EXEC)

  execute_process(COMMAND ${ROOT_CONFIG_EXEC} --version
    OUTPUT_VARIABLE _config_version
    OUTPUT_STRIP_TRAILING_WHITESPACE
    )
  string(REGEX REPLACE "^([0-9]+)\\.([0-9]+)/([0-9]+).*" "\\1.\\2.\\3"
    _version "${_config_version}")
  set(ROOT_CONFIG_VERSION ${_config_version} CACHE STRING
    "ROOT version from root-config" FORCE)
  set(ROOT_VERSION ${_version} CACHE STRING "ROOT version found" FORCE)
  mark_as_advanced(ROOT_CONFIG_VERSION)
  unset(_config_version)
  unset(_version)

  execute_process(
    COMMAND ${ROOT_CONFIG_EXEC} --bindir
    OUTPUT_VARIABLE _bindir
    OUTPUT_STRIP_TRAILING_WHITESPACE
    )
  set(ROOT_BINARY_DIR "${_bindir}" CACHE PATH "ROOT binary directory" FORCE)
  unset(_bindir)

  execute_process(
    COMMAND ${ROOT_CONFIG_EXEC} --libdir
    OUTPUT_VARIABLE _libdir
    OUTPUT_STRIP_TRAILING_WHITESPACE
    )
  set(ROOT_LIBRARY_DIR "${_libdir}" CACHE PATH "ROOT library directory" FORCE)
  unset(_libdir)

  execute_process(
    COMMAND ${ROOT_CONFIG_EXEC} --incdir
    OUTPUT_VARIABLE _incdir
    OUTPUT_STRIP_TRAILING_WHITESPACE
    )
  set(ROOT_INCLUDE_DIR "${_incdir}" CACHE PATH "ROOT include directory" FORCE)
  unset(_incdir)

  execute_process(
    COMMAND ${ROOT_CONFIG_EXEC} --cflags
    OUTPUT_VARIABLE _cxx_flags
    OUTPUT_STRIP_TRAILING_WHITESPACE
    )
  # Remove include directories from compiler flags; they are handled separately
  string(REGEX REPLACE "-I[^ ]*" "" _cxx_flags "${_cxx_flags}")
  set(ROOT_CXX_FLAGS "${_cxx_flags}" CACHE STRING "ROOT C++ compiler flags" FORCE)
  mark_as_advanced(ROOT_CXX_FLAGS)
  unset(_cxx_flags)

  execute_process(
    COMMAND ${ROOT_CONFIG_EXEC} --ldflags
    OUTPUT_VARIABLE _ldflags
    OUTPUT_STRIP_TRAILING_WHITESPACE
    )
  set(ROOT_LD_FLAGS "${_ldflags}" CACHE STRING "ROOT linker flags" FORCE)
  mark_as_advanced(ROOT_LD_FLAGS)
  unset(_ldflags)

  execute_process(
    COMMAND ${ROOT_CONFIG_EXEC} --libs
    OUTPUT_VARIABLE _libs
    OUTPUT_STRIP_TRAILING_WHITESPACE
    )
  set(ROOT_LIB_FLAGS "${_libs}" CACHE STRING "Linker flags for ROOT libraries" FORCE)
  mark_as_advanced(ROOT_LIB_FLAGS)
  unset(_libs)
endif(NOT ROOT_CONFIG_EXEC)

# We need some of these variables as ;-lists
set(ROOT_CXXFLAG_LIST ${ROOT_CXX_FLAGS})
separate_arguments(ROOT_CXXFLAG_LIST)

# Get list of ROOT's core libraries from the output of "root-config --libs"
separate_arguments(ROOT_LIB_FLAGS)
string(REPLACE "-l" "" ROOT_LIB_FLAGS "${ROOT_LIB_FLAGS}")

# Find absolute paths to the core libraries plus any requested components
set(ROOT_LIBRARIES)
set(targetlist)
list(REMOVE_DUPLICATES ROOT_FIND_COMPONENTS)
foreach(_lib IN LISTS ROOT_LIB_FLAGS ROOT_FIND_COMPONENTS)
  if(_lib MATCHES "^[A-Z].+")
    find_library(ROOT_${_lib}_LIBRARY ${_lib} HINTS ${ROOT_LIBRARY_DIR} NO_CMAKE_ENVIRONMENT_PATH)
    mark_as_advanced(ROOT_${_lib}_LIBRARY)
    if(ROOT_${_lib}_LIBRARY)
      add_library(ROOT::${_lib} SHARED IMPORTED)
      set_target_properties(ROOT::${_lib} PROPERTIES
	INTERFACE_INCLUDE_DIRECTORIES "${ROOT_INCLUDE_DIR}"
	IMPORTED_LINK_INTERFACE_LANGUAGES "CXX"
	IMPORTED_LOCATION "${ROOT_${_lib}_LIBRARY}"
	INTERFACE_COMPILE_OPTIONS
	  "$<$<BUILD_INTERFACE:$<COMPILE_LANGUAGE:CXX>>:${ROOT_CXXFLAG_LIST}>"
	)
      list(APPEND ROOT_LIBRARIES ${ROOT_${_lib}_LIBRARY})
      list(REMOVE_ITEM ROOT_FIND_COMPONENTS ${_lib})
      list(APPEND targetlist ROOT::${_lib})
    endif()
  endif()
endforeach()
if(ROOT_LIBRARIES)
  list(REMOVE_DUPLICATES ROOT_LIBRARIES)
endif()

add_library(ROOT::Libraries INTERFACE IMPORTED)
set_target_properties(ROOT::Libraries PROPERTIES
  INTERFACE_INCLUDE_DIRECTORIES "${ROOT_INCLUDE_DIRS}"
  )
set_target_properties(ROOT::Libraries PROPERTIES
  INTERFACE_LINK_LIBRARIES "${targetlist}"
  )
unset(targetlist)

# If any requested components are remaining, they weren't found above,
# so report a warning or an error
foreach(_remaining ${ROOT_FIND_COMPONENTS})
  if(ROOT_FIND_REQUIRED_${_remaining})
    message(FATAL_ERROR "ROOT component ${_remaining} not found")
  elseif(NOT ROOT_FIND_QUIETLY)
    message(WARNING " ROOT component ${_remaining} not found")
  endif()
  unset(ROOT_FIND_REQUIRED_${_remaining})
endforeach()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(ROOT
  REQUIRED_VARS ROOTSYS ROOT_LIBRARY_DIR ROOT_BINARY_DIR ROOT_INCLUDE_DIR
  ROOT_CONFIG_EXEC ROOT_LIBRARIES
  VERSION_VAR ROOT_VERSION
  )

endif(NOT TARGET ROOT::Libraries)

find_program(MK_ROOTDICT mk_rootdict.sh
  HINTS
    ${CMAKE_CURRENT_LIST_DIR}/..
  PATH_SUFFIXES scripts
  DOC "Wrapper script for ROOT dictionary generator"
  NO_CMAKE_ENVIRONMENT_PATH
  )
if(NOT MK_ROOTDICT)
  message(FATAL_ERROR
    "FindROOT: Cannot find mk_rootdict.sh. Check your installation.")
endif()

# BUILD_ROOT_DICTIONARY(dictionary
#                       LINKDEF <theLinkDef.h>
#                       [ TARGETS <target> [ <target> ... ]
#                       [ INCLUDEDIRS <dir> [ <dir ... ]
#                       [ OPTIONS [ <options> ]
#                       [ PCMNAME <pcmname> ]
#                       <header> [ <header> ... ] )
#
# Build ROOT dictionary for ROOT 5 and 6
#
# Arguments:
#   dictionary:    dictionary base name (required). Output file will be
#                  ${dictionary}Dict.cxx
#   LINKDEF:       name of LinkDef.h file (required)
#   TARGETS:       CMake targets from which to get -I arguments and -D definitions
#   INCLUDEDIRS:   paths to use for -I arguments
#   OPTIONS:       additional options for rootcling/rootcint
#   PCMNAME:       base name of PCM file (ROOT 6 only). Defaults to ${dictionary}.
#                  Output will be lib${pcmname}_rdict.pcm.
#
# Creates a custom target ${dictionary}_ROOTDICT.
# With ROOT 6, also installs the PCM file in ${CMAKE_INSTALL_LIBDIR}
#
function(build_root_dictionary dictionary)

  if(DEFINED ROOT_VERSION AND DEFINED ROOTSYS)
    if(NOT ${ROOT_VERSION} VERSION_LESS 6)
      find_program(ROOTCLING rootcling HINTS "${ROOTSYS}/bin" NO_CMAKE_ENVIRONMENT_PATH)
      if(NOT ROOTCLING)
	message(FATAL_ERROR
	  "root_generate_dictionary: Cannot find rootcling. Check ROOT installation.")
      endif()
    else()
      find_program(ROOTCINT rootcint HINTS "${ROOTSYS}/bin" NO_CMAKE_ENVIRONMENT_PATH)
      if(NOT ROOTCINT)
	message(FATAL_ERROR
	  "root_generate_dictionary: Cannot find rootcint. Check ROOT installation.")
      endif()
    endif()
  else()
    message(FATAL_ERROR "root_generate_dictionary: ROOT is not set up")
  endif()

  cmake_parse_arguments(RGD "" "PCMNAME" "INCLUDEDIRS;LINKDEF;OPTIONS;TARGETS" ${ARGN})

  if(NOT RGD_LINKDEF)
    message(FATAL_ERROR "root_generate_dictionary: Required argument LINKDEF missing")
  endif()

  # Compile definitions and include directories from the given target(s)
  set(defines)
  set(incdirs)
  foreach(tgt IN LISTS RGD_TARGETS)
    list(APPEND defines $<TARGET_PROPERTY:${tgt},COMPILE_DEFINITIONS>)
    list(APPEND incdirs $<TARGET_PROPERTY:${tgt},INCLUDE_DIRECTORIES>)
  endforeach()

  # Add any explicitly specified include directories
  list(APPEND incdirs ${RGD_INCLUDEDIRS})

  if(ROOTCLING)
    # ROOT6
    if(RGD_PCMNAME)
      set(pcmname ${RGD_PCMNAME})
    else()
      set(pcmname ${dictionary})
    endif()
    add_custom_command(
      OUTPUT ${dictionary}Dict.cxx lib${pcmname}_rdict.pcm
      COMMAND ${MK_ROOTDICT}
      ARGS
	${ROOTCLING}
	-f ${dictionary}Dict.cxx
	-s lib${pcmname}
	${RGD_OPTIONS}
	INCDIRS "${incdirs}"
	DEFINES "${defines}"
	${RGD_UNPARSED_ARGUMENTS}
	${RGD_LINKDEF}
	VERBATIM
	DEPENDS ${RGD_UNPARSED_ARGUMENTS} ${RGD_LINKDEF}
      )
    set(PCM_FILE ${CMAKE_CURRENT_BINARY_DIR}/lib${pcmname}_rdict.pcm)
    install(FILES ${PCM_FILE} DESTINATION ${CMAKE_INSTALL_LIBDIR})
  else()
    # ROOT5
    add_custom_command(
      OUTPUT ${dictionary}Dict.cxx
      COMMAND ${MK_ROOTDICT}
      ARGS
	${ROOTCINT}
	-f ${dictionary}Dict.cxx
	-c ${RGD_OPTIONS}
	INCDIRS "${incdirs}"
	DEFINES "${defines}"
	${RGD_UNPARSED_ARGUMENTS}
	${RGD_LINKDEF}
	VERBATIM
	DEPENDS ${RGD_UNPARSED_ARGUMENTS} ${RGD_LINKDEF}
      )
  endif()
  add_custom_target(${dictionary}_ROOTDICT
    DEPENDS ${dictionary}Dict.cxx
  )
endfunction()
