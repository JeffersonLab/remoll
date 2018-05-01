#include "remoll.h"

#include <TH2.h>
#include <TStyle.h>
#include <TCanvas.h>

// Local analysis class
class my_analysis: public remoll {
  public:
    my_analysis(): remoll() { };
    void Loop();
};

// USER CODE: Initialize histograms
TH1D* h_hit_n = new TH1D("h_hit_n","h_hit_n",100,0,100);
TH1D* h_hit_pid = new TH1D("h_hit_pid","h_hit_pi",2300,0,2300);

// LOOP function
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

      // USER CODE: Fill histograms
      h_hit_n->Fill(hit->size());
      for (size_t i = 0; i < hit->size(); i++) {
        h_hit_pid->Fill(hit->at(i).pid);
      }

   }
}
