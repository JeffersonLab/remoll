# Simulations for the MOLLER Experiment at Jefferson Lab

[![Build Status](http://travis-ci.org/JeffersonLab/remoll.svg?branch=develop)](https://travis-ci.org/JeffersonLab/remoll)

## Community

Simulations are coordinated on the [12gevmoller_sim@jlab.org](https://mailman.jlab.org/mailman/listinfo/12gevmoller_sim) mailing list and the [JLab 12 GeV Slack workspace](https://jlab12gev.slack.com) (in particular, the `#moller_simulation` channel). Anyone with a jlab.org email address can join without invitation. Feel free to contact developers there with questions.

## Running simulations

There are several options for running simulations:
- [downloading and compiling the code](README.Compiling.md),
- [using a docker container on any operating system](README.Docker.md),
- [using a singularity container on linux systems](README.Singularity.md).

Simulations can be run in interactive mode when not specifying arguments, or in batch mode when specifying a macro:
```
Usage:
 remoll [-g geometry] [-m macro] [-u session] [-r seed] [-t nthreads] [macro]
```

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
