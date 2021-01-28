#ifndef __REMOLLSEARCHPATH_HH
#define __REMOLLSEARCHPATH_HH

/*!
    --------------------------------------------------------------------------------------------
    remollSearchPath

    Header-only file to search for files inside
    the remoll directory.
    --------------------------------------------------------------------------------------------
Usage:
    remollSearchPath mySearchPath;
    mySearchPath.add("macros");  //< Adds the macros directory to the list of directories the next line will search for gui.mac in
    mySearchPath("gui.mac"); //< Returns the canonical path of gui.mac as a std::string

    The add() function by default adds CMAKE_INSTALL_PREFIX, CMAKE_INSTALL_FULL_DATADIR
    and the current working directory to the list of directories the operator() will search in.
    --------------------------------------------------------------------------------------------
*/

#include <unistd.h>

#if defined(__cpp_lib_filesystem)
#include <filesystem>
namespace fs = std::filesystem;
#elif defined(__cpp_lib_experimental_filesystem)
#include <experimental/filesystem>
namespace fs = std::experimental::filesystem;
#elif defined(__USE_BOOST_FILESYSTEM)
#include <boost/filesystem.hpp>
namespace fs = boost::filesystem;
#else
#define NO_FS_SUPPORT
#endif

#include <string>


class remollSearchPath
{
private:
    static remollSearchPath* fInstance;
    #ifndef NO_FS_SUPPORT
    std::vector<fs::path> fSearchPath;
    #endif
    remollSearchPath();

public:
    static remollSearchPath* getInstance();
    virtual ~remollSearchPath();
    void add(const std::string& path);
    std::string operator() (const std::string& filename);
    static std::string resolve(const std::string& filename) {
        return remollSearchPath::getInstance()->operator()(filename);
    }
};

#endif //__REMOLLSEARCHPATH_HH
