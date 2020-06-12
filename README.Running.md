# Running

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
```
Usage:
 remoll [-g geometry] [-m macro] [-u session] [-r seed] [-t nthreads] [macro]
```

To execute the macro `macros/runexample.mac` with random seed 1234 and 4 threads, just run:
```
remoll -r 1234 -t 4 macros/runexample.mac
```

Several other [example macros](macros/README.md) are available.
