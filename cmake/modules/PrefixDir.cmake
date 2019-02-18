# Default install path is the source directory
if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    message(STATUS "Setting install prefix to source dir as none was specified." )
    message(STATUS "    Use -DCMAKE_INSTALL_PREFIX=/usr/local to set to something else" )
    set (CMAKE_INSTALL_PREFIX "${CMAKE_SOURCE_DIR}"
         CACHE PATH "default install path" FORCE )
endif()

