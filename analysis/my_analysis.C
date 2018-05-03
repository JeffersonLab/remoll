#include "remoll.h"

#include <TH1D.h>
#include <TH2D.h>
#include <TH3D.h>
#include <TStyle.h>
#include <TCanvas.h>

// Local analysis class
class my_analysis: public remoll {
public:
  my_analysis(TString name): remoll(name) { };
  void Loop();
};

// USER CODE: Initialize histograms

// General histograms
TH1D* h_hit_n = new TH1D("h_hit_n","Number of hits per event",100,0,100);
TH1D* h_hit_pid = new TH1D("h_hit_pid","Number of hits vs particle ID",5000,-2500,2500);
TH1D* h_hit_det = new TH1D("h_hit_det","Number of hits vs detector ID",100,4000,4100);

// Hist of detector ID, log-y (where are the most hits?)

// 2D xy hist for detector ID = mylar entry
// 2D xy hist for detector ID = PMT window
// 2D xy hist for detector ID = lightguide entry
// 2D xy hist for detector ID = lightguide exit
TH2D* h_hit_xy_pmt_window = new TH2D("h_hit_xy_pmt_window","XY at PMT window",10000,-1000,+1000,10000,-1000,+1000);
TH2D* h_hit_xy_lg_entry = new TH2D("h_hit_xy_lg_entry","XY at LG entry",100,-10.0,+10.0,100,-10.0,+10.0);
TH2D* h_hit_xy_lg_exit = new TH2D("h_hit_xy_lg_exit","XY at LG exit",100,-10.0,+10.0,100,-10.0,+10.0);
TH3D* h_hit_xyz_pmt_window = new TH3D("h_hit_xyz_pmt_window","XYZ at PMT window",1000,-1000,+1000,1000,-1000,+1000,100,31900,32050);

// Hist of energy for detector ID = lightguide entry
// Hist of energy for detector ID = lightguide exit
// Hist of energy for detector ID = PMT window
TH1D* h_hit_energy_pmt_window = new TH1D("h_hit_energy_pmt_window","Energy spectrum at PMT window",120,0.0,6.0);
TH1D* h_hit_energy_lg_entry = new TH1D("h_hit_energy_lg_entry","Energy spectrum at LG entry",120,0.0,6.0);
TH1D* h_hit_energy_lg_exit = new TH1D("h_hit_energy_lg_exit","Energy spectrum at LG exit",120,0.0,6.0);
TH1D* h_hit_lambda_pmt_window = new TH1D("h_hit_lambda_pmt_window","Wavelength spectrum at PMT window",100,0.0,1000.0);
TH1D* h_hit_lambda_lg_entry = new TH1D("h_hit_lambda_lg_entry","Wavelength spectrum at LG entry",100,0.0,1000.0);
TH1D* h_hit_lambda_lg_exit = new TH1D("h_hit_lambda_lg_exit","Wavelength spectrum at LG exit",100,0.0,1000.0);

TH1D* h_hit_p_dot_r_pmt_window = new TH1D("h_hit_p_dot_r_pmt_window","Momentum dot position at PMT window",120,0.0,6.0);
TH1D* h_hit_p_dot_r_lg_entry = new TH1D("h_hit_p_dot_r_lg_entry","Momentum dot position at LG entry",120,0.0,6.0);
TH1D* h_hit_p_dot_r_lg_exit = new TH1D("h_hit_p_dot_r_lg_exit","Momentum dot position at LG exit",120,0.0,6.0);

// 2D col-z hist for xy at mylar and z = number of hits in PMT window
// 2D col-z hist for xy at mylar and z = number of hits in lightguide entry
// 2D col-z hist for xy at mylar and z = number of hits in lightguide exit



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
    
    // Loop over hits
    for (size_t i = 0; i < hit->size(); i++) {
      remollGenericDetectorHit_t h = hit->at(i);
      
      h_hit_pid->Fill(h.pid);
      h_hit_det->Fill(h.det);
      
      // Fill local coordinate histograms
      if (h.pid == 0) {
        if (h.det == 4000) {
          h_hit_xy_pmt_window->Fill(h.x,h.y);
          h_hit_energy_pmt_window->Fill(h.e/units->eV);
          h_hit_lambda_pmt_window->Fill(532.0*(2.33*units->eV)/h.e);
        }
        if (h.det == 4010) {
          h_hit_xy_lg_entry->Fill(h.x,h.y);
          h_hit_energy_lg_entry->Fill(h.e/units->eV);
          h_hit_lambda_lg_entry->Fill(532.0*(2.33*units->eV)/h.e);
        }
        if (h.det == 4011) {
          h_hit_xy_lg_exit->Fill(h.x,h.y);
          h_hit_energy_lg_exit->Fill(h.e/units->eV);
          h_hit_lambda_lg_exit->Fill(532.0*(2.33*units->eV)/h.e);
        }
      }
      
      if (h.det == 4000 && 0.1*units->eV < h.e && h.e < 10*units->eV) {
        for (float t = 0; t < 1e7; t += 1e6) {
          h_hit_xyz_pmt_window->Fill(h.x-t*h.px,h.y-t*h.py,h.z-t*h.pz);
          h_hit_xy_pmt_window->Fill(h.x-t*h.px,h.y-t*h.py);
        }
      }
    }
  }
  
  // USER CODE: Close out
  TCanvas *c1 = new TCanvas("c1");
  c1->Divide(2,1);
  c1->cd(1);
  c1->GetPad(1)->SetLogy();
  c1->GetPad(2)->SetTitle("Energy spectrum");
  c1->GetPad(1)->RangeAxis(0,10,6,1e5);
  h_hit_energy_lg_entry->Draw();
  h_hit_energy_lg_exit->Draw("same");
  h_hit_energy_pmt_window->Draw("same");
  c1->cd(2);
  c1->GetPad(2)->SetLogy();
  c1->GetPad(2)->SetTitle("Wavelength spectrum");
  c1->GetPad(2)->RangeAxis(0,10,1000,1e5);
  h_hit_lambda_lg_entry->Draw();
  h_hit_lambda_lg_exit->Draw("same");
  h_hit_lambda_pmt_window->Draw("same");
}
