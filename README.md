# remoll: Simulation for the 12 GeV MOLLER Experiment at Jefferson Lab

[![Build Status](https://travis-ci.org/JeffersonLab/remoll.svg?branch=develop)](https://travis-ci.org/JeffersonLab/remoll)

## Contact information

The development of remoll is coordinated on the mailing list
12gevmoller_sim@jlab.org which you can subscribe to [here](https://mailman.jlab.org/mailman/listinfo/12gevmoller_sim).

There is a [slack channel](https://jlab12gev.slack.com) available for general discussion and questions. In particular, the `#moller_simulation` and `#moller_simulation_dev` channels are used for topics related to this project.


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

Magnetic field maps are **required** to use the software and are available for download [here](http://hallaweb.jlab.org/12GeV/Moller/downloads/remoll/).


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


## Troubleshooting

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
This is a [known issue](https://github.com/JeffersonLab/remoll/issues/40). A
workaround is to run remoll with OpenGL disabled:
```
LIBGL_ALWAYS_INDIRECT=1 build/remoll
```

## Docker container

**Note**: This image will allow you to use remoll in batch mode only. A separate image supporting the GUI mode will be available soon.

### Building

```
sudo docker build -t remoll .
```

### Running

You can use a prebuilt image [available on Docker Hub](https://hub.docker.com/r/jeffersonlab/remoll/).

```
docker run --rm \
    -v `pwd`/output:/jlab/2.1/Linux_CentOS7.3.1611-x86_64-gcc4.8.5/remoll/rootfiles/ \
    jeffersonlab/remoll [macro to run]
```

The ROOT files produced by remoll will be present in the output directory.

## Singularity container

### Building

```
sudo singularity build remoll.img Singularity
```

## Running

```
singularity pull shub://jeffersonlab/remoll-singularity
singularity run --bind `pwd`:/jlab/2.1/Linux_CentOS7.3.1611-x86_64-gcc4.8.5/remoll/rootfiles/ \
    jeffersonlab-remoll-singularity-master.simg \
    macros/tests/test_moller.mac
```

