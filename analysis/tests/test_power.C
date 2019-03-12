/*
This program for calculating the power on the collimator 1,2,4,5 for MOLLER 
experiment
Chandan Ghosh
part of this is taken from Rakitha's code coll_scan.cc
*/
#include <vector>
#include <iostream>

#include "remolltypes.hh"

#include <TTree.h>
#include <TFile.h>
#include <TChain.h>

void test_power (const TString& inputdir = ".", const TString& inputname = "remollout")
{
  // Input files
  TChain* T = new TChain("T");
  T->Add(inputdir + "/" + inputname + ".root");

  // Output files
  TString outputdir = inputdir + "/" + "analysis";
  TFile file(outputdir + "/" + inputname + ".root","RECREATE");

  // Collimators and particles to track
  std::vector<Int_t> collimators = {2001, 2006, 2002, 2004, 2005};
  std::vector<Int_t> particles   = {11, -11, 22, 2112};

  // Energy vectors
  std::vector<Double_t> esum(collimators.size(), 0.0);
  std::vector<std::vector<Double_t>> esum2(collimators.size(), std::vector<Double_t>(particles.size(), 0.0));

  // Connect to tree
  std::vector<remollGenericDetectorSum_t>* fGenDetSum = 0;
  T->SetBranchAddress("sum",&fGenDetSum);

  // Event loop
  Int_t nentries = T->GetEntries();
  std::cout << "Number of entries in TChain " << nentries << std::endl;
  for (int ev = 0; ev < T->GetEntries(); ev++) {

    T->GetEntry(ev);

    for (size_t isum = 0; isum < fGenDetSum->size(); isum++) {

      Int_t volume = fGenDetSum->at(isum).det;
      Double_t edep = fGenDetSum->at(isum).edep;

      for (size_t coll = 0; coll < collimators.size(); coll++) {

        if (volume == collimators[coll]) {

          esum[coll] += edep;

          std::vector<remollGenericDetectorSumByPID_t> sumbypid = fGenDetSum->at(isum).by_pid;

          for (size_t isum2 = 0; isum2 < sumbypid.size(); isum2++) {

            Double_t edep = sumbypid.at(isum2).edep;
            Int_t pid = sumbypid.at(isum2).pid;

            for (size_t part = 0; part < particles.size(); part++) {
              if (pid == particles[part])
                esum2[coll][part] += edep;
            }
          }
        }
      }
    }
  }

  for (size_t coll = 0; coll < collimators.size(); coll++) {
    std::cout << "Collimator " << collimators[coll] << std::endl;
    std::vector<Double_t> esum_pid(collimators.size(), 0.0);
    std::cout << "Deposited energy (from sum.edep) " << std::setw(8) << esum[coll]*85./nentries << " W/85uA" << std::endl;
    for (size_t part = 0; part < particles.size(); part++) {
      std::cout << "particle " << std::setw(5) << particles[part] << " energy deposited (from by_pid) " << std::setw(6) << esum2[coll][part]*85./nentries << " W/85uA" << std::endl;
      esum_pid[coll] += esum2[coll][part];
    }
    std::cout << "By pid sum edep (from by_pid) " << esum_pid[coll]*85./nentries << " W/85uA" << std::endl;
  }
  std::cout << "Total energy deposited inside collimator (from sum.edep) " << esum[0]*85./nentries + esum[1]*85./nentries << " W/85uA" <<std::endl;

  // Cleanup
  delete T;
}

