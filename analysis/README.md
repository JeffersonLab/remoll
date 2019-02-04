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
