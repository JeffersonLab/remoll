cmake_minimum_required (VERSION 2.6)
project(bggen)
enable_language(Fortran)

# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)

get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # gfortran
  set (CMAKE_Fortran_FLAGS_RELEASE "-ffixed-line-length-none -funroll-all-loops -fno-f2c -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-ffixed-line-length-none -fno-f2c -O0 -g")
elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # ifort (untested)
  set (CMAKE_Fortran_FLAGS_RELEASE "-f77rtl -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-f77rtl -O0 -g")
elseif (Fortran_COMPILER_NAME MATCHES "g77")
  # g77
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g")
else (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # unknown
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")

# cernlib
execute_process(COMMAND cernlib -dy packlib mathlib OUTPUT_VARIABLE CERNLIB)
string(STRIP ${CERNLIB} CERNLIB)

# include directory
include_directories(${PROJECT_SOURCE_DIR}/include)

# source files
file(GLOB libsources ${PROJECT_SOURCE_DIR}/*.F)

# object library
add_library(bggen_object OBJECT ${libsources})
set_target_properties(bggen_object PROPERTIES POSITION_INDEPENDENT_CODE 1)

# static library
add_library(bggen_static STATIC $<TARGET_OBJECTS:bggen_object>)
set_target_properties(bggen_static PROPERTIES OUTPUT_NAME libbggen)
target_link_libraries(bggen_static ${CERNLIB})

# shared library
add_library(bggen_shared SHARED $<TARGET_OBJECTS:bggen_object>)
set_target_properties(bggen_shared PROPERTIES OUTPUT_NAME libbggen)
target_link_libraries(bggen_shared ${CERNLIB})

# executable
add_executable(bggen bggen.cc)
target_link_libraries(bggen bggen_shared ${CERNLIB})

