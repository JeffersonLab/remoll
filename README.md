# Simulations for the MOLLER Experiment at Jefferson Lab

[![Build Status](https://travis-ci.org/JeffersonLab/remoll.svg?branch=develop)](https://travis-ci.org/JeffersonLab/remoll)

## Community

Simulations are coordinated on the [12gevmoller_sim@jlab.org](https://mailman.jlab.org/mailman/listinfo/12gevmoller_sim) mailing list and the [JLab 12 GeV Slack workspace](https://jlab12gev.slack.com) (in particular, the `#moller_simulation` channel). Anyone with a jlab.org email address can join without invitation.

## Preparing to run simulations

There are several options for running simulations:
- [downloading and compiling the code](README.Compiling.md),
- [using a docker container on any operating system](README.Docker.md),
- [using a singularity container on linux systems](README.Singularity.md).
Detailed information can be found at each of the links.

## Running simulations

### Interactive mode

If you do not specify any arguments, the simulation will run in interactive mode. You will have access to a graphical user interface with menus and a 3D visualization of the geometry. This only works in locally compiled code or when using the singularity container.

To start in interactive mode, just run the `remoll` executable without arguments:
```
remoll
```

To load and visualize the default geometry, use the following macro commands:
```
/control/execute vis/Qt.mac
```
Several other [visualization macros](vis/README.md) are available.

### Batch mode

If you specify arguments, the simulation will run in batch mode without graphical user interface. You can specify the following arguments:

To execute the macro `macros/runexample.mac` with random seed 1234 and 4 threads, just run:
```
remoll -s 1234 -t 4 macros/runexample.mac
```

Several other [example macros](macros/README.md) are available.

## Analyzing the output

You can access the output file with a regular root installation (it will warn about non-perfect support). A listing of the [output variables](README.variables.md) is available for reference.

To take advantage of dedicated functionality for the data types in the output file, you will need to follow the more detailed [analysis instructions](analysis/README.md). To simplify this, you can also use the helper executable `reroot` which is available wherever `remoll` is available.

## Known issues

### Error: LLVM SYMBOLS ARE EXPOSED TO CLING

You may encounter the following error message when running in graphical mode:
```
 Error in <UnknownClass::InitInterpreter()>: LLVM SYMBOLS ARE EXPOSED TO CLING!
 This will cause problems; please hide them or dlopen() them after the call to
 TROOT::InitInterpreter()!
```
This is a (generally harmless) [known issue](https://github.com/JeffersonLab/remoll/issues/40). A workaround is to run remoll with OpenGL disabled:
```
LIBGL_ALWAYS_INDIRECT=1 build/remoll
```
