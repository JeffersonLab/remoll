# Singularity container

Simulations for the MOLLER experiment can be run using [Singularity](https://sylabs.io/singularity/) containers on High Performance Computing (HPC) system.

The instructions below explicitly use the `develop` code branch. Every branch is available as a pre-built Docker container on [Docker Hub](https://hub.docker.com/r/jeffersonlab/remoll/) which is used to build a local Singularity container, but only the `develop` and `master` branches are available as pre-built Singularity containers on CernVM-FS.

## Requirements

- Singularity >= 2.6.1

On many HPC systems, the `singularity` command is either installed systemwide or available through environment modules (`module load singularity`).

## Building a local Singularity container

You can build a local Singularity container from the docker container with:
```
singularity pull docker://jeffersonlab/remoll:develop
```
This will create the file `jeffersonlab_remoll_develop.sif` (or similar) in your local directory.

## Using the Singularity container on CernVM-FS

You can use a singularity container hosted on the global CernVM-FS filesystem, courtesy of the Open Science Grid. This pre-built Singularity container is located at `/cvmfs/singularity.opensciencegrid.org/jeffersonlab/remoll:develop`. This is a directory, not a single file, but can be used instead of the file name below.

## Running simulations with the Singularity container

```
singularity run jeffersonlab_remoll_develop.sif remoll -h
```

## Input and output locations

In addition to the geometry and macros that are included in the Singularity container at `/jlab/remoll/`, you can also use any (user) directory in your local filesystem. By default the simulation will run in your current directory. In that directory,
