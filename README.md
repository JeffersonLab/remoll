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

## Analyzing the output

Though you can access the data field alone with a regular root
installation (it will warn about non-perfect support), you will
need to load `libremoll.so` if you want to access the methods on
those classes written out to the ROOT files.

You can use the LD_PRELOAD environment variable to load the
`libremoll.so` library (you could even add this to your login
script):
```
export LD_PRELOAD=build/libremoll.so 
root -l remollout.root 
```
in bash, or
```
setenv LD_PRELOAD build/libremoll.so 
root -l remollout.root 
```
in tcsh, or in a single line
```
LD_PRELOAD=build/libremoll.so root -l remollout.root
```

To simplify this without the need for an environment variable,
you can also use the helper executable `reroot` which is built
every time you compile.

## Standalone Analysis

It is possible to write your own analysis scripts and 
compilable executables. With the inclusion of remoll-specific
classes and methods comes a lot of power in compiled (against
ROOT) anaylsis scripts, where assigning the contents of a 
remoll output .root file to a new Tree allows accessing the 
constituent branches and utilizing the branch names and 
methods from within compiled code.

### Utilizing remoll's classes

For example, utilizing the struct's and classes available
in libremoll.so (exemplified in remolltypes.hh), it is 
possible to do the following in compiled code: 
```
TApplication theApp("App",&argc,argv);
TChain * Tmol =new TChain("T");
Tmol->Add("remollout.root");
Tmol->Draw("branch.variable","cuts");
```
Or, to access the contents of the branches directly:
```
TFile *fileName = new TFile("remollout.root");
TTree *Tree = (TTree*)old->Get("T");
std::vector < remollGenericDetectorHit_t > *fHit = 0;
Tree->SetBranchAddress("hit", &fHit);
for (size_t j = 0; j < Tree->GetEntries(); j++){
  //Do stuff with entry j
  Tree->GetEntry(j);
  for (size_t i = 0; i < fHit->size(); i++){
    remollGenericDetectorHit_t hit = fHit->at(i); 
    //Do stuff with hit i
    if (hit.pid == 11 && hit.mtrid == 1){
      //Do stuff with the hit i for entry j
    }
  }
}
```
### Compiling

In terms of compiling individual analyses against or next
to remoll, it is easiest to use the same `cmake .. ; make`
procedure. In the CMakeLists.txt file, to add your own analysis
find these lines and replace "ana" with your executable's name
(if you are making new classes please make your own header and
LinkDef.h files, like so):

```
ROOT_GENERATE_DICTIONARY(
    anaDict                         # path to dictionary to generate
    "include/anatypes.hh"           # list of classes to process
    LINKDEF "include/anaLinkDef.h"  # ROOT linkDef file
    OPTIONS -p
)
```
Then point to the source and header files:
```
file(GLOB sources ${PROJECT_SOURCE_DIR}/src/ana*.cc)
file(GLOB headers ${PROJECT_SOURCE_DIR}/include/ana*.hh)
```
Then generate a shared object library with your custom classes:
```
add_library(ana-lib SHARED ${sources} ${headers} anaDict.cxx)
set_target_properties(ana-lib PROPERTIES OUTPUT_NAME ana)
target_link_libraries(ana-lib ${ROOT_LIBRARIES} )
```
Then link your executable (and link with a previously generated
libremoll.so if you want to):
```
add_executable(ana-bin ana.cc)
set_target_properties(ana-bin PROPERTIES OUTPUT_NAME ana)
target_link_libraries(ana-bin ${PROJECT_SOURCE_DIR}/libremoll.so ana-lib)
```
Fail if your executable doesn't work:
```
add_custom_target(ANALYSIS DEPENDS ana-bin)
```
Place the results:
```
install(TARGETS ana-bin DESTINATION bin)
install(TARGETS ana-lib DESTINATION lib)
```
The CMakeLists.txt in the "analysis/" folder exemplifies this.


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

