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
You can see all available branches with the `git branch` command. You can also
see previous released versions with the `git tag` command (and check them out
as if they are a branch).

We use a [semantic versioning and branching model](https://github.com/JeffersonLab/remoll/wiki/Semantic-Versioning-and-Branching-Model)
which allows us to develop new features in a separate branch and merge them
back into the `develop` branch easily. Periodically we release a new version.

### Compiling

We are using an "out of source" build system, which means that the directory
where the build products (compiled files) are created is different from the
directory with the source code. This keeps the directory with the source code
clean and prevents generated files from accidentally being uploaded.

To build you will first need to create the build directory. If you choose
for example a build directory called `build` you would execute:
```
mkdir build
cd build
```
Next, you will use `cmake` to configure the build system:
```
cmake ..
```
Finally, you build the entire simulation by running `make` in your build directory:
```
make
```
Pro tip: During development, it may be easier to run `make -C build` from the
top of the source directory.

### Downloading field maps (automatically done in recent versions)
Some information for the simulation is not included in this repository and must
be downloaded separately. In particular you will need field maps, which can be
found on the [MOLLER downloads](http://hallaweb.jlab.org/12GeV/Moller/downloads/remoll/) page.
Typically we place these field maps in a directory called `map_directory`.

In the `develop` branch these files are automatically downloaded when you
configure the build system. In older releases you may need to download the
map files manually:
```
mkdir map_directory
wget --directory-prefix=map_directory http://hallaweb.jlab.org/12GeV/Moller/downloads/remoll/blockyHybrid_rm_3.0.txt
wget --directory-prefix=map_directory http://hallaweb.jlab.org/12GeV/Moller/downloads/remoll/blockyUpstream_rm_1.1.txt
```

### Running remoll
Finally, to run the simulation just execute the command `remoll` which will be
located in your build directory:
```
build/remoll
```
This will bring up a graphical command interface. You can type geant4 commands
in the bottom command line.

You can choose a number of command line options. You can see which ones by
passing the option `-h`, as in `build/remoll -h`:
```
Usage:
 remoll [-g geometry] [-m macro] [-u UIsession] [-r seed] [-t nThreads] [macro]
```
In particular you can pass a macro, such as:
```
build/remoll macros/runexample.mac
```
which will run the simulation in batch mode (no graphics output).

You can also run in single-threaded mode:
```
build/remoll -t 1
```
which sometimes helps in debugging.

## Typical `remoll` session

When running `remoll`, keep the state machine of geant4 in mind, in particular
the `PreInit` and `Idle` states.
- When `remoll` starts, you enter in the `PreInit` state.
- First you setup the geometry and physics lists.
- Next you initialize and transition from the `PreInit` to the `Idle` state.
- Now you can start runs with the geometry and physics lists defined earlier.

A minimal example session could be:
```
/remoll/geometry/setfile geometry/mollerMother_merged.gdml
/remoll/parallel/setfile geometry/mollerParallel.gdml

/run/initialize

/remoll/addfield map_directory/blockyHybrid_rm_3.0.txt
/remoll/addfield map_directory/blockyUpstream_rm_1.1.txt

/remoll/evgen/set moller

/remoll/filename remollout.root

/run/beamOn 100
```

## Geometry
Our geometry is stored in GDML format, an XML-based format for storing geometry
[developed at CERN](http://gdml.web.cern.ch/GDML/). Details can be found in the
[User Guide](http://gdml.web.cern.ch/GDML/doc/GDMLmanual.pdf).

Geometries can be stored anywhere, but typically you will want to use the ones
in the `geometry/` directory. You may be interested in just checking the
geometry with a simple [gdml viewer](https://github.com/JeffersonLab/gdmlview).

In remoll you can load the geometry with
```
/remoll/geometry/setfile <path/to/mother.gdml>
```

### Auxiliary tags in geometry files
In plain Geant4 there is only support for materials and volumes in GDML. To add
support for detectors and visualization features, we use the auxiliary tags that
GDML supports. We do have to write the interpretation and parsers for this in
remoll, though.
```
<auxiliary auxtype="SensDet" auxvalue="detectorName"/>
<auxiliary auxtype="DetType" auxvalue="lowenergyneutral"/>
<auxiliary auxtype="DetNo" auxvalue="4000"/>
<auxiliary auxtype="Color" auxvalue="blue"/>
<auxiliary auxtype="Alpha" auxvalue="0.5"/>
```

## Physics lists
Several standard physics lists are provided by Geant4. We don't modify those,
except for adding or removing specific processes:
- optical photon physics,
- step limiter physics (with associated auxiliary tags and macro commands),
- parallel world physics.

## Event generators
Event generators don't do much more than adding a cross section weighting factor
to each event. That's it. We almost always sample isotropically in theta, phi
space. Exceptions are the beam generator.

## Detectors: hits and sums
The concept of a hit in Geant4 is very different from what we call a hit in
experimental contexts. One particle Hits in Geant4

## Output
