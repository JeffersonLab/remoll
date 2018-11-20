## Remoll CMake configuration files

The files in this directory are structured as follows:
- `cmake/modules` contains modules loaded by remoll during building, i.e. find_package 
  and use files that are not provided by upstream packages,
- `cmake/scripts` contains our own find_package and use files (as templates) for
  installation along with the libraries in the standard CMake locations.
- `cmake/templates` contains other files as templates to be installed by CMake into
  binary, library, or shared data locations (e.g. remoll-config).
