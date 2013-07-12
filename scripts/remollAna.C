#define remollAna_cxx
#include "remollAna.h"
#include "remollH1.h"
#include <TH2.h>
#include <TStyle.h>
#include <TCanvas.h>

void remollAna::InitHists(const char *gen){
  if (strcmp(gen,"moller") == 0 ) fHists.push_back( new remollH1( &ev_A, remollH1::kSingle, this, 0, 60.0, "apv_ee", Form("%s A_{PV}", gen), "A_{PV} [ppb]", "Rate [GHz/0.2ppb]" ) );
  if (strcmp(gen,"elastic") == 0 ) fHists.push_back( new remollH1( &ev_A, remollH1::kSingle, this, -600.0, 0, "apv_ep", Form("%s A_{PV}", gen), "A_{PV} [ppb]", "Rate [GHz/3ppb]" ) );
  if (strcmp(gen,"inelastic") == 0 ) fHists.push_back( new remollH1( &ev_A, remollH1::kSingle, this, 0, 2000.0, "apv_in", Form("%s A_{PV}", gen), "A_{PV} [ppb]", "Rate [GHz/5ppb]" ) );
  fHists.push_back( new remollH1( hit_r, remollH1::kHit, this, 0.6, 1.3, Form("rad_%s", gen), Form("%s Radius", gen), "r [m]", "Rate [GHz/5mm]") );
if (strcmp(gen,"moller") == 0 ) fHists.push_back( new remollH1( &ev_thcom, remollH1::kSingle, this, 0, 180.0, "thcom", Form("%s Theta_COM", gen), "#theta_{CM} [deg]", "Rate [GHz/deg]" ) );
  fHists.push_back( new remollH1( ev_th, remollH1::kEvent, this, 0, 0.03, Form("th_%s", gen), Form("%s Theta_LAB", gen), "#theta_{lab} [deg]", "Rate [GHz/0.3mdeg]" ) );
  fHists.push_back( new remollH1( hit_e, remollH1::kHit, this, 0, 15.0, Form("energy_det_%s", gen), Form("%s Energy_det", gen), "Energy [GeV]", "Rate [GHz/0.1GeV]" ) );
  fHists.push_back( new remollH1( &ev_Q2, remollH1::kSingle, this, 0, 0.015, Form("q2_%s", gen), Form("%s Q2", gen), "Q^{2} [(GeV/c)^{2}]", "Rate [GHz/100(MeV/c)^{2}" ) );
  fHists.push_back( new remollH1( ev_vz, remollH1::kEvent, this, -0.8, 0.8, Form("vert_z0_%s", gen), Form("%s vertex_z", gen), "target z-vertex [m]", "Rate [GHz/1.6cm]" ) );
  fHists.push_back( new remollH1( ev_vy, remollH1::kEvent, this, -0.005, 0.005, Form("vert_y0_%s", gen), Form("%s vertex_y", gen), "target y-vertex [m]", "Rate [GHz/mm]" ) );
  fHists.push_back( new remollH1( ev_vx, remollH1::kEvent, this, -0.005, 0.005, Form("vert_x0_%s", gen), Form("%s vertex_x", gen), "target x-vertex [m]", "Rate [GHz/0.1mm]" ) );
  if (strcmp(gen,"inelastic") == 0 ) fHists.push_back( new remollH1( &ev_W2, remollH1::kSingle, this, 0, 20, "W2_in", Form("%s W2", gen), "W^{2} [(GeV/c)^{2}]", "Rate [GHz/0.1(GeV/c)^{2}" ) );

  //  fHists.push_back( new remollH1( &ev_Q2, remollH1::kSingle, this, 0, 0.016, "q2_eweight", Form("%s Q2_eweight", gen), "Q^{2} [(GeV/c)^{2}]", "Energy weighted Rate [MeV*GHz/80(MeV/c)^{2}" ) );
  //  fHists.push_back( new remollH1( &ev_W2, remollH1::kSingle, this, 0, 20, "W2_q2_weight", Form("%s W2_Q2_weight", gen), "W^{2} [(GeV/c)^{2}]", "Q^{2} weighted Rate [(GeV/c)^{2}*GHz/0.1(GeV/c)^{2}" ) );

}


void remollAna::Loop(const char *gen)
{
//   In a ROOT session, you can do:
//      Root > .L remollAna.C
//      Root > remollAna t
//      Root > t.GetEntry(12); // Fill t data members with entry number 12
//      Root > t.Show();       // Show values of entry 12
//      Root > t.Show(16);     // Read and show values of entry 16
//      Root > t.Loop();       // Loop on all entries
//

//     This is the loop skeleton where:
//    jentry is the global entry number in the chain
//    ientry is the entry number in the current Tree
//  Note that the argument to GetEntry must be:
//    jentry for TChain::GetEntry
//    ientry for TTree::GetEntry and TBranch::GetEntry
//
//       To read only selected branches, Insert statements like:
// METHOD1:
//    fChain->SetBranchStatus("*",0);  // disable all branches
//    fChain->SetBranchStatus("branchname",1);  // activate branchname
// METHOD2: replace line
//    fChain->GetEntry(jentry);       //read all branches
//by  b_branchname->GetEntry(ientry); //read only this branch
   if (fChain == 0) return;

   Long64_t nentries = fChain->GetEntriesFast();

   unsigned int idx;

   Long64_t nbytes = 0, nb = 0;
   for (Long64_t jentry=0; jentry<nentries;jentry++) {
       if( (jentry%200000) == 0 ){
	   printf("Event %10lld\r", jentry );
	   fflush(stdout);
       }

      Long64_t ientry = LoadTree(jentry);
      if (ientry < 0) break;
      nb = fChain->GetEntry(jentry);   nbytes += nb;
      // if (Cut(ientry) < 0) continue;

      for( idx = 0; idx < fHists.size(); idx++ ) {

	fHists[idx]->Fill(gen);
	
      }
   }
}

void remollAna::Draw( bool rcut, const char *gen ){
    unsigned int idx;
    for( idx = 0; idx < fHists.size(); idx++ ){
	if( rcut ){
	  fHists[idx]->Draw(remollH1::kCut, gen);
	} else {
	  fHists[idx]->Draw(remollH1::kNoCut, gen);
	}
    }
    return;
}

void remollAna::Write(const char *gen){
    unsigned int idx;
    for( idx = 0; idx < fHists.size(); idx++ ){
      fHists[idx]->Write(gen);
    }
     return;
}
