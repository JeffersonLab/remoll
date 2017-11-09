# remoll: Simulation for the 12 GeV MOLLER Experiment at Jefferson Lab

## Contact information

The development of remoll is coordinated on the mailing list
12gevmoller_sim@jlab.org. You can subscribe at
https://mailman.jlab.org/mailman/listinfo/12gevmoller_sim

A slack discussion channel is available at https://jlab12gev.slack.com,
in particular the channels #moller_simulation and #moller_simulation_dev.


## Dependencies 

The following packages are required to build `remoll`:
* cmake > 2.6
* Geant4 >= 4.9.6
* ROOT
* Python
* git (optional)
* boost (optional)


## Build instructions

To build, create the directory you would like to build in, say `build`:
```
mkdir build
cd build
cmake ..
make
```

## Magnetic field maps

Magnetic field maps can be downloaded from
http://hallaweb.jlab.org/12GeV/Moller/downloads/remoll/.


## Running remoll

An example macro will be found in `macros/`, in particular the
macro `runexample.mac` will be instructive for new users.

You need to load `libremollroot.so` if you want to access 
the classes written out to the ROOT files.  Building
remoll will create a rootlogon.C which will do this
automatically.

To execute remoll, run `build/remoll` from inside the source
directory. This should bring up a graphical command interface.

To load and visualize the default geometry, use the following
macro commands:
```
/run/initialize
/control/execute vis/Qt.mac
```
You can also load another macro in the `vis` directory, if
you prefer another visualization driver.


## Troubleshooting:

### Missing gitinfo.hh

If you get errors about a missing `gitinfo.hh` file during building, try again
```
cmake ..
make
```

### LLVM SYMBOLS ARE EXPOSED TO CLING

You may encounter the following error message when running in graphical mode:
```
 Error in <UnknownClass::InitInterpreter()>: LLVM SYMBOLS ARE EXPOSED TO CLING!
 This will cause problems; please hide them or dlopen() them after the call to
 TROOT::InitInterpreter()!
```
This is a known issue 40, https://github.com/JeffersonLab/remoll/issues/40. A
workaround is to run remoll with OpenGL disabled:
```
LIBGL_ALWAYS_INDIRECT=1 build/remoll
```

