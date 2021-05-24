#include "TROOT.h"
#include "TFile.h"
#include "TTree.h"
#include <vector>
using namespace std;

void remoll_SkimTree(){
    
  TFile* ifile = new TFile("../o_remoll.root","READ");
  TTree* itree = (TTree*)ifile->Get("T");
  long nEntries = itree->GetEntries();
  cout<<"\tTotal events: "<<nEntries<<endl;

  Double_t newrate=0;
  vector<remollGenericDetectorHit_t>  *newhit=0;
  TFile* ofile = new TFile("remoll_SkimTree.root","RECREATE");

  TTree *otree     = new TTree("T", "outputtree");
  TBranch *b_rate = otree->Branch("rate", &newrate);
  TBranch *b_hit   = otree->Branch("hit", &newhit);

  Double_t *rate=0;
  vector<remollGenericDetectorHit_t>  *hit=0;
  itree->SetBranchAddress("rate", &rate);
  itree->SetBranchAddress("hit", &hit);

  for (int i=0; i< nEntries;i++){
    itree->GetEntry(i);
    for(int j=0; j<hit->size(); j++){
      if(hit->at(j).det == 5614 && hit->at(j).k > 10.*CLHEP::MeV) newhit->push_back(hit->at(j));
    }
    otree->Fill();
    newhit->clear();
  }
  otree->Write();
  return 0;
}
