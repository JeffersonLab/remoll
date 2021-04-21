# Docker container

Simulations for the MOLLER experiment can be run using [Docker](https://www.docker.com/get-started) containers. Docker containers include a fully working simulation environment and all dependencies (Geant4, ROOT).

The instructions below explicitly use the `develop` code branch. Every branch is available as a tag on Docker Hub.

## Requirements

- Docker service

## Running

You can use a pre-built image on [Docker Hub](https://hub.docker.com/r/jeffersonlab/remoll/).

```
docker run --rm \
    -v `pwd`/output:/jlab/2.1/Linux_CentOS7.3.1611-x86_64-gcc4.8.5/remoll/rootfiles/ \
    jeffersonlab/remoll [macro to run]
```

The ROOT files produced by remoll will be present in the output directory.

## Building the Docker container locally

You can build the Docker container directly from the Dockerfile in the git repository with:
```
docker build -t remoll .
```
Additional instructions are included at the top of the [Dockerfile](Dockerfile).
