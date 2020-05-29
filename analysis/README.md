# Analysis Macros

## Simulation data structures

If you wish to use functionality specific to the data structures, use the executable `reroot` instead of `root`.

If for various reasons this is not an option on your system, you can load `libremoll.so` in different ways.

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

### Regenerating the geometry from the ROOT output

The geometry tree is saved in the output file so that keeping the geometry files around is not required to visualize what was simulated. To regenerate this geometry, run:
```
build/reroot remollout.root
run_data->RecreateGDML()
```

### Regenerating the macro from the ROOT output

The macro is saved in the output file so that it is possible to determine how an output file was generated. To regenerate the macro, run:
```
build/reroot remollout.root
run_data->fMacro->Print()
```

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

To compile your own analysis alongside remoll it is easiest to
mimic one of the existing analysis CMakeLists.txt procedures
in one of the /analysis/ subfolders, with "pe" being the most
comprehensive example included so far.

You can also put your analysis alongside remoll in the primary
build directory if you have problems linking things from within
/analysis/, but please do not commit any such changes to
remoll's baseline CMakeLists.txt, and here is what is needed:

#### CMakeLists prescription
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
The CMakeLists.txt in the "analysis/" sub-folders exemplify this.


## Using remoll.h to build your analysis code

You can use the class `remoll.h` to build your own analysis object to parse
remoll output ROOT files. Here is a short introduction on how to that.

### Objectives
- Allow users to easily write code that fills multiple histograms while
  looping through the tree only once.
- Move most of the repetitive work into code provided by `remoll.h`, and
  allowing the user to write only the code they care about.

### Approach
- `remoll.h` provides a pure virtual class `remoll` that knows how to
  read in a ROOT file created by remoll. The user can extend this class
  to provide the functionality they care about.
- If the underlying code for the `remoll` class is changed, this should
  not affect the functionality that users have added.

### Example
The file `my_analysis.C` shows how this works. Rather than modifying
this example with that name, first copy it to a more descriptive name.

### Running
To run the analysis, you load the analysis code, instantiate an analysis
object, then use the provided functions:
- `GetEntry(Int_t event_number)` to read a specific event,
- `Show()` to show the values for the current event,
- `Show(Int_t event_number)` to show the values for a specific event,
- `Loop()` to loop over all events.

The following comment is also include in the example `my_analysis.C`.
```
  //   In a ROOT session, you can do:
  //      root> .L my_analysis.C
  //      root> my_analysis t
  //      root> t.GetEntry(12); // Fill t data members with entry number 12
  //      root> t.Show();       // Show values of entry 12
  //      root> t.Show(16);     // Read and show values of entry 16
  //      root> t.Loop();       // Loop on all entries
```  

### Start by modifying working example
First copy the `my_analysis.C` example to a new file. For example,
```
git cp my_analysis.C main_detector_optical_photons.C
```
You can now edit your own copy. Just change every instance of `my_analysis`
in the file to your new name.

### Start from scratch with blank file
Inherit your own analysis class from `remoll`. Let's call our own analysis
class `my_analysis`.
```
#include "remoll.h"

class my_analysis: public remoll {
public:
  my_analysis(TString name): remoll(name) { };
  void Loop();
};

// USER CODE: Initialize histograms

/* your code goes here */

void my_analysis::Loop()
{
  //   In a ROOT session, you can do:
  //      root> .L my_analysis.C
  //      root> my_analysis t
  //      root> t.GetEntry(12); // Fill t data members with entry number 12
  //      root> t.Show();       // Show values of entry 12
  //      root> t.Show(16);     // Read and show values of entry 16
  //      root> t.Loop();       // Loop on all entries
  //

  if (fChain == 0) return;
  for (Long64_t event = 0; event < fChain->GetEntries(); fChain->GetEntry(event++)) {

    // USER CODE: Event selection

    /* your code goes here */

    // USER CODE: Fill histograms with event level quantities

    /* your code goes here */

    // Loop over hits
    for (size_t i = 0; i < hit->size(); i++) {
      remollGenericDetectorHit_t h = hit->at(i);

      // USER CODE: Fill histograms with hit level quantities

      /* your code goes here */

    }
  }

  // USER CODE: Close out, write to output files

  /* your code goes here */

}
```
