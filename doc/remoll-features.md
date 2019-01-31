# Features in remoll

The following commands are assumed to be run on a reasonably up-to-date
(Linux or Mac) system.

## Installation

### Prerequisites on local systems
To install Geant4 and ROOT, you can use two helper repositories. These will
install Geant4 and ROOT into `/usr/local/{geant4,root}/<version>/`.
```
cd /usr/local/src
git clone https://github.com/wdconinc/cmake_root
cd cmake_root
./cmake_root 6.16.00
cd ..
git clone https://github.com/wdconinc/cmake_geant4
cd cmake_geant4
./cmake_geant4 4.10.05
cd ..
```
I suggest that you subsequently create a link from `/usr/local/{geant4,root}/pro`
to the version that you wish to use by default (the latest one you installed).
You can then load your installations with the following commands in your `.login`
file (bash syntax since that's likely what you are running there):
```
source /usr/local/root/pro/bin/thisroot.sh
source /usr/local/geant4/pro/bin/geant4.sh
```

### Prerequisites on Jefferson Lab systems
I will assume that you are using the Jefferson Lab Common Environment, which is
at version 2.3 as of this writing. Add the following lines to your `.login` file
(tcsh syntax since that's likely what you are running there):
```
setenv JLAB_VERSION 2.3
setenv JLAB_ROOT /site/12gev_phys
# version >= 2.0
source ${JLAB_ROOT}/softenv.csh ${JLAB_VERSION}
# version < 2.0
#source ${JLAB_ROOT}/${JLAB_VERSION}/ce/jlab.csh
```

When you load this upon login, you should see:
```
 > Common Environment Version: <2.3>  (Wed October 3 2018)
 > Running as wdconinc on ifarm1401.jlab.org
 > OS Release:    Linux_CentOS7.2.1511-x86_64-gcc4.8.5
 > JLAB_ROOT set to:     /site/12gev_phys
 > JLAB_SOFTWARE set to: /site/12gev_phys/2.3/Linux_CentOS7.2.1511-x86_64-gcc4.8.5

 > BANKS         version:  1.4
 > CCDB          version:  1.06.02
 > CLHEP         version:  2.4.0.4
 > EVIO          version:  5.1
 > GEANT4        version:  4.10.04.p02
 > GEMC          version:  2.7
 > JANA          version:  0.8.0
 > MLIBRARY      version:  1.3
 > MYSQL         installed in  /site/12gev_phys/2.3/Linux_CentOS7.2.1511-x86_64-gcc4.8.5/mysql/lib
 > QT            version:  5.10.1
 > ROOT          version:  6.14.04
 > SCONS         version:  1.7
 > XERCESC       version:  3.2.2
```

### Downloading
We first download the latest version of remoll from github, and switch to the
development branch.
```
git clone https://github.com/JeffersonLab/remoll
cd remoll
```
If you want to be able to push changes, you may prefer to use ssh keys instead:
```
git clone git@github.com:JeffersonLab/remoll.git
cd remoll
```

### Selecting the branch
By default you will clone the `master` branch of the repository. This is a stable
version that represents the latest release.  If you are interested in using
recent changes to the code or geometry, you may want to use the `develop` branch.
```
git checkout develop
```
You can see all available branches with `git branch` command.

## Event generators

## Geometry

## Physics lists

## Detectors: hits and sums

## Output
