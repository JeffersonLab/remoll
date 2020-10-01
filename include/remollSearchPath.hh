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

namespace fs = std::experimental::filesystem;

class remollSearchPath
{
private:
    std::vector<fs::path> fSearchPath;
public:
    remollSearchPath();
    virtual ~remollSearchPath();

    void add(const std::string& path);
    const std::string operator() (const std::string& filename);
};

remollSearchPath::remollSearchPath()
{
    fSearchPath.push_back(fs::path(get_current_dir_name())); //< add cwd to the search path
    fSearchPath.push_back(fs::path(CMAKE_INSTALL_PREFIX)); //< add CMAKE_INSTALL_PREFIX to search path
    fSearchPath.push_back(fs::path(CMAKE_INSTALL_FULL_DATADIR)); //< add CMAKE_INSTALL_FULL_DATADIR to search path
}

remollSearchPath::~remollSearchPath() {}

void remollSearchPath::add(const std::string& path)
{
    // If directory to search in is inside CMAKE_INSTALL_PREFIX
    if(fs::exists(fs::path(std::string(CMAKE_INSTALL_PREFIX) + "/" + path))) {
        fSearchPath.push_back(fs::path(std::string(CMAKE_INSTALL_PREFIX) + "/" + path));
    }
        // If directory to search in is inside CMAKE_INSTALL_FULL_DATADIR
    else if(fs::exists(fs::path(std::string(CMAKE_INSTALL_FULL_DATADIR) + "/" + path))) {
        fSearchPath.push_back(fs::path(std::string(CMAKE_INSTALL_FULL_DATADIR) + "/" + path));
    }
        // If directory to search in is inside the current working directory
    else if(fs::exists(fs::path(get_current_dir_name()) / path)) {
        fSearchPath.push_back(fs::path(get_current_dir_name()) / path);
    }
        // Path not relative to CMAKE_INSTALL_PREFIX, CMAKE_INSTALL_FULL_DATADIR
        // or the current working directory
        // path is probably an absolute path
    else if (fs::exists(fs::path(path))) {
        fSearchPath.push_back(fs::path(path));
    }
}

const std::string remollSearchPath::operator() (const std::string& filename)
{
    // If the file you are looking for exists inside any of the
    // directories inside fSearchPath, return the filename prefixed with
    // the directory for which the full path exists
    for (auto path: fSearchPath) {
        if(fs::exists(path / filename)) {
            return (path / filename).string();
        }
    }

    // File not found in any of the search directories,
    // return the filename
    return std::string(filename); /* and pray everything work */
}

#endif //__REMOLLSEARCHPATH_HH
