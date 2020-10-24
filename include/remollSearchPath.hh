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
#include "experimental/filesystem"
#include "cstring"

namespace fs = std::experimental::filesystem;

class remollSearchPath
{
private:
    static remollSearchPath* fInstance;
    std::vector<fs::path> fSearchPath;
    remollSearchPath();

public:
    static remollSearchPath* getInstance();
    virtual ~remollSearchPath();
    void add(const std::string& path);
    std::string operator() (const std::string& filename);
    static std::string resolve(const std::string& filename) {
        return remollSearchPath::getInstance()->operator()(filename);
    }
    // So now it shouldn't matter if you do (remollSearchPath::resolve(macro)).c_str()
    // or remollSearchPath::resolve(macro.c_str())
    // It will return the same thing
    static const char* resolve(const char* filename) {
        remollSearchPath::resolve(std::string(filename)).c_str();
    }
};

#endif //__REMOLLSEARCHPATH_HH
