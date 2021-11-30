#include "remollSearchPath.hh"

#include <climits>

remollSearchPath* remollSearchPath::fInstance = nullptr;

remollSearchPath::remollSearchPath() {
  // add CMAKE_INSTALL_FULL_DATADIR, CMAKE_INSTALL_PREFIX and CWD to search path
  #ifndef NO_FS_SUPPORT
  #ifdef _GNU_SOURCE
    fSearchPath.push_back(fs::path(get_current_dir_name()));
  #else
    char* dir = (char*) malloc(PATH_MAX * sizeof(char));
    if (getcwd(dir, PATH_MAX) != nullptr) {
      fSearchPath.push_back(fs::path(dir));
      free(dir);
    }
  #endif
    fSearchPath.push_back(fs::path(CMAKE_INSTALL_PREFIX));
    fSearchPath.push_back(fs::path(CMAKE_INSTALL_FULL_DATADIR));
  #endif
}

remollSearchPath *remollSearchPath::getInstance() {
  if (fInstance == nullptr) {
    fInstance = new remollSearchPath();
  }
  return fInstance;
}

remollSearchPath::~remollSearchPath() {}

void remollSearchPath::add(const std::string& path) {
#ifndef NO_FS_SUPPORT
  // Check if path is an absolute path
  if (fs::exists(fs::path(path))) {
    fSearchPath.push_back(fs::path(path));
  }
#endif
}

std::string remollSearchPath::operator() (const std::string& filename) {
  // If the file you are looking for exists inside any of the
  // directories inside fSearchPath, return the filename prefixed with
  // the directory for which the full path exists
#ifndef NO_FS_SUPPORT
  for (auto path: fSearchPath) {
    fs::path test(path.string() + "/" + filename);
    if(fs::exists(test)) {
      return test.string();
    }
    else if(fs::exists(path / "remoll" / filename)) {
      return (path / "remoll" / filename).string();
    }
  }
#endif

  // File not found in any of the search directories,
  // return the filename
  return std::string(filename);
}
