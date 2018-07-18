//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Tue Oct 25 13:57:03 2016 by ROOT version 6.06/08
// from TTree T/Geant4 Moller Simulation
// found on file: remollout.root
//////////////////////////////////////////////////////////

#ifndef remoll_h
#define remoll_h
#include <iostream>
#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>
#include <TH1D.h>
#include "remolltypes.hh"

// Header file for the classes stored in the TTree if any.

class remoll {
public :
   TChain         *fChain;   //!pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //!current Tree number in a TChain

   // Declaration of leaf types
   Double_t rate;
   remollUnits_t *units;
   remollEvent_t *ev;
   remollBeamTarget_t *bm;
   std::vector<remollEventParticle_t> *part;
   std::vector<remollGenericDetectorHit_t> *hit;
   std::vector<remollGenericDetectorSum_t> *sum;

   remoll(const TString& name = "remollout.root");
   virtual ~remoll();
   virtual Int_t    Cut(Long64_t entry);
   virtual Int_t    GetEntry(Long64_t entry);
   virtual Long64_t LoadTree(Long64_t entry);
   virtual void     Init(TChain *tree);
   virtual void     Loop() = 0;
   virtual Bool_t   Notify();
   virtual void     Show(Long64_t entry = -1);
};

remoll::remoll(const TString& name) : fChain(0) 
{
TChain* tree = new TChain("T");
fCurrent = tree->Add(name);

// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
//   if (tree == 0) {
  //    TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject(name);
    //  if (!f || !f->IsOpen()) {
      //   f = new TFile(name);
     // }
    //  f->GetObject("T",tree);

  // }

   Init(tree);
}

remoll::~remoll()
{
   if (!fChain) return;
   delete fChain->GetCurrentFile();
}

Int_t remoll::GetEntry(Long64_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}
Long64_t remoll::LoadTree(Long64_t entry)
{
// Set the environment to read one entry
   if (!fChain) return -5;
   Long64_t centry = fChain->LoadTree(entry);
   if (centry < 0) return centry;
   if (fChain->GetTreeNumber() != fCurrent) {
      fCurrent = fChain->GetTreeNumber();
      Notify();
   }
   return centry;
}

void remoll::Init(TChain *tree)
{
   // The Init() function is called when the selector needs to initialize
   // a new tree or chain. Typically here the branch addresses and branch
   // pointers of the tree will be set.
   // It is normally not necessary to make changes to the generated
   // code, but the routine can be extended by the user if needed.
   // Init() will be called many times when running on PROOF
   // (once per file to be processed).

   // Set branch addresses and branch pointers
   if (!tree) return;
   fChain = tree;
   fCurrent = -1;
   std::cout<< "setting branches" << std::endl;
   fChain->SetBranchAddress("rate", &rate);
   fChain->SetBranchAddress("bm", &bm);
   fChain->SetBranchAddress("ev", &ev);
   fChain->SetBranchAddress("part", &part);
   fChain->SetBranchAddress("hit", &hit);
   fChain->SetBranchAddress("sum", &sum);

   Notify();
}

Bool_t remoll::Notify()
{
   // The Notify() function is called when a new file is opened. This
   // can be either for a new TTree in a TChain or when when a new TTree
   // is started when using PROOF. It is normally not necessary to make changes
   // to the generated code, but the routine can be extended by the
   // user if needed. The return value is currently not used.

   return kTRUE;
}

void remoll::Show(Long64_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}

Int_t remoll::Cut(Long64_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
   return 1;
}

#endif
