// ROOTscript: plot_shower-max_response.C
// -----------------------------------------------------------------------------------
// Author: Sudip Bhattarai
// Date:   01/21/2025
// Purpose: Read the remoll rootfile and use shower-max lookup table to plot the PE response.
// -----------------------------------------------------------------------------------

#include "RtypesCore.h"
#include "TChain.h"
#include "TH1.h"
#include "TROOT.h"
#include "TCanvas.h"
#include <iostream>
#include <fstream>
#include <utility>
#include <vector>
#include <string>
#include "./remollToQsim.hh"
#include "./shower-max_resposne_lookup.hh"

#ifdef __REMOLLTYPES__ // for clangd LSP support
  #include "../include/remolltypes.hh"
#endif // __REMOLLTYPES__


// Namespace list to use
using std::cout, std::cerr, std::endl;
using std::vector, std::string;
using std::ifstream, std::ofstream;
using std::pair, std::map;

// Main function
void plot_showermax_response(TString rootFile = "~/moller/softwares/remoll-zone/rootfiles/sm_stack/stack_v27/sm_tqStack_3sector_moller_50k_1001.root") {

    // Testing shower-max response 
    ShowermaxLookup::getPeResponse(73015, 11, 1000, 0.0, 1080.0);

    // Remoll file path goes here
    int goodFileCount = 1; // Number of good (non-corrupted) files
     
    // Define histograms
    TH1D* h_hitRate = new TH1D("hitRate", "Rate weighted hit; hit.r [mm]; rate[GHz/65uA]", 100, 1000, 1200);
    TH1D* h_hitRateSmPE = new TH1D("hitRateSmPE", "PE*Rate weighted hit; hit.r [mm]; rate [PE*GHz/65uA]", 100, 1000, 1200); 

    // Declare TChain
    TChain* T = new TChain("T");
    T->Add(rootFile);

	Long64_t nEntries = T->GetEntries();
	cout << "Total number of entries in the chain: " << nEntries << endl;
	std::vector<remollGenericDetectorHit_t> *fHit = 0;
	// remollEvent_t *fEv = 0;
	Double_t fRate = 0;
	Float_t energy(-1.0e-12), rate(-1.0e-12),
            hitx(-1.0e-12), hity(-1.0e-12), hitr(-1.0e-12), hitpz(-1.0e-12),
            pe(0);
	Int_t pid(0), det(0);
	
	T->SetBranchAddress("hit", &fHit);
	T->SetBranchAddress("rate", &fRate);
	// T->SetBranchAddress("ev", &fEv);

	// This loop goes over all the events in the root files
	for (Long64_t iEvent = 0; iEvent < nEntries; iEvent++){
		if (iEvent % (nEntries/10) == 0)
			cout << "Analyzed " << iEvent << " events."  << endl;
		T->GetEntry(iEvent); //Reads all the branches for entry(iEvent) and writes it to the respective variable(fHit or fRate) 

		//This loop goes over all the hits in the specific event
		for (Int_t iHit = 0; iHit < fHit->size(); iHit++){
			det = fHit->at(iHit).det;
			energy = fHit->at(iHit).e;
			pid = fHit->at(iHit).pid;
            hitx = fHit->at(iHit).x;
            hity = fHit->at(iHit).y;
            hitr = fHit->at(iHit).r;
            hitpz = fHit->at(iHit).pz;
			rate = fRate/1.0e9/goodFileCount; // convert to GHz/uA

			//Fill histograms with proper cuts
			bool all_cuts = (det >= 73001 && det <= 73028 &&											    		//det number 30 is SM plane
                            hitr>1020 && hitr<1180)&&
                            (pid==11 || pid==-11 || pid==22 || pid==13 || pid==-13 || pid==211 || pid==-211 || pid==2112) &&    //particle selection
							energy>10 &&  										//energy cut
                            hitpz>0;															//particle coming from upstream (pz>0)
			
			if (all_cuts) {
                // Double_t pe = ShowermaxLookup::getPeResponseWithoutLP(pid, energy, hitx, hity);
                Double_t pe = ShowermaxLookup::getPeResponse(det, pid, energy, hitx, hity);
                std::cout << "pid: " << pid << ", energy: " << energy << ", pe: " << pe << std::endl;

                h_hitRate->Fill(hitr, rate);
                h_hitRateSmPE->Fill(hitr,rate*pe);
            }
        }
    }

    // Print the rate in shower-max for script validation
    double rateTotal = h_hitRate->Integral(); // in GH
    cout << "Accepted rate: " << rateTotal << " GHz" << endl;

    double cathCurrent = h_hitRateSmPE->Integral()*1e9*1.6e-19*1e9; // in nA
    cout << "Total cathode current: " << cathCurrent << " nA" << endl;
    
    // Plot the histograms
    // TCanvas* c1 = new TCanvas("c1", "c1", 1300, 500);
    // c1->Divide(2, 1);
    // c1->cd(1);
    // h_hitRate->Draw("hist E");
    // c1->cd(2);
    // h_hitRateSmPE->Draw("hist E");

}

