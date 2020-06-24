# Compiling the code

## Requirements

* git
* cmake > 3.5
* Geant4 >= 4.10.00 (>= 4.10.06 recommended)
* ROOT >= 6.0.0
* python
* boost (optional)

## Quickstart screencast

[![asciicast](https://asciinema.org/a/220728.svg)](https://asciinema.org/a/220728)

## Downloading the source code

To download the code, either download the source code for a recent [release](https://github.com/JeffersonLab/remoll/releases) or use git to clone this source code repository:
```
git clone https://github.com/JeffersonLab/remoll
```
You can optionally specify a directory name to clone into:
```
git clone https://github.com/JeffersonLab/remoll directory_name
```

## Compilation

To build, create the directory you would like to build in, say `build`:
```
mkdir build
cd build
cmake ..
make
make install
```

## Magnetic field maps

Magnetic field maps are required to run the simluations and will be downloaded automatically during the cmake step above. If for some reason you must download the field maps by hand, they are available for download on the [Hall A](http://hallaweb.jlab.org/12GeV/Moller/downloads/remoll/) page.

## Running simulations with the compiled code

You can run simulations from any location provided the following conditions are satisfied:
- the `remoll` executable is in the PATH,
- the `libremoll.so` library is in the LD_LIBRARY_PATH,
- the graphical user interface and visualization macros are accessible from the current directory.

If you are in the directory into which you cloned the source code repository, you can run the simulation as `build/remoll`.

If you have run `make install` and `remoll` is now installed in a directory in your PATH, you can run the simulation in batch mode from any directory.
