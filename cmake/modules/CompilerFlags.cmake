# The following compiler flag variables are available (limiting to CXX code):
# - CMAKE_CXX_FLAGS: used by ALL build types
# - CMAKE_CXX_FLAGS_{DEBUG,RELWITHDEBINFO,MINSIZEREL,RELEASE}: used for specified build type only
#
# There is no need to add -g to the debug types since it is already included.

include(CheckCXXCompilerFlag)

# Set up CMAKE_CXX_FLAGS_WARN for warnings that are only used for DEBUG or RELWITHDEBINFO
set(CMAKE_CXX_FLAGS_WARN "-W -Wall -Wnon-virtual-dtor -Wno-long-long -Wcast-align -Wchar-subscripts -Wpointer-arith -Wformat-security -Woverloaded-virtual -fno-check-new -fno-common")
# Ideally no warnings should get turned off below, all should have a plan for getting fixed...
string(APPEND CMAKE_CXX_FLAGS_WARN " -Wno-shadow")
string(APPEND CMAKE_CXX_FLAGS_WARN " -Wno-pedantic")
string(APPEND CMAKE_CXX_FLAGS_WARN " -Wno-overloaded-virtual")
# Ignore warning of struct initialization { } for gcc < 5.0 (after that it is ignored internally)
if(CMAKE_CXX_COMPILER_VERSION VERSION_LESS 5.0)
  string(APPEND CMAKE_CXX_FLAGS_WARN " -Wno-missing-field-initializers")
endif()

# Set up the debug CXX_FLAGS for extra warnings
string(APPEND CMAKE_CXX_FLAGS_DEBUG " ${CMAKE_CXX_FLAGS_WARN}")
string(APPEND CMAKE_CXX_FLAGS_DEBUG " ${CMAKE_CXX_FLAGS_ERROR}")
string(APPEND CMAKE_CXX_FLAGS_RELWITHDEBINFO " ${CMAKE_CXX_FLAGS_WARN}")

# Not returning from a non-void function is an error on some compilers
CHECK_CXX_COMPILER_FLAG("-Werror=return-type" HAVE_GCC_ERROR_RETURN_TYPE)
if(HAVE_GCC_ERROR_RETURN_TYPE)
  string(APPEND CMAKE_CXX_FLAGS_ERROR " -Werror=return-type")
endif()

# If we are compiling on Linux then set some extra linker flags too
if(CMAKE_SYSTEM_NAME MATCHES Linux)
  string(APPEND CMAKE_SHARED_LINKER_FLAGS " -Wl,--fatal-warnings -Wl,--no-undefined -lc")
  string(APPEND CMAKE_MODULE_LINKER_FLAGS " -Wl,--fatal-warnings -Wl,--no-undefined -lc")
  string(APPEND CMAKE_EXE_LINKER_FLAGS    " -Wl,--fatal-warnings -Wl,--no-undefined -lc")
endif()
