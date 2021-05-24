#include "TROOT.h"
#include "TFile.h"
#include "TTree.h"
#include <vector>
using namespace std;

TFile *ofile;
TTree *otree;
TBranch *b_rate,*b_hit;
Double_t newrate=0;
vector<remollGenericDetectorHit_t>  *newhit=0;

long processOne(string fnm);

void skimTree(string finNm, int testRun=0){
  
  long nTotHits(0);
  int nFiles(0);

  if(finNm==""){
    cout<<"\t did not find input file. Quitting!"<<endl;
    return;
  }

  ofile = new TFile("o_remollSkimTree.root","RECREATE");

  otree     = new TTree("T", "skim tree");
  b_rate = otree->Branch("rate", &newrate);
  b_hit   = otree->Branch("hit", &newhit);

  if( finNm.find(".root") < finNm.size() ){
    cout<<"Processing single file:\n\t"<<finNm<<endl;
    nTotHits+=processOne(finNm);
    nFiles=1;
  }else{
    cout<<"Attempting to process list of output from\n\t"<<finNm<<endl;
    ifstream ifile(finNm.c_str());
    string data;
    while(ifile>>data){
      cout<<" processing: "<<data<<endl;
      nTotHits+=processOne(data);
      nFiles++;
      if(testRun==1 && nFiles>10)
	break;
    }
  }

  ofile->cd();
  otree->Write();
  ofile->Close();

  cout<<"\nFinished processing a total of "<<nTotHits<<endl;
}

long processOne(string fnm){

  TFile* ifile = new TFile(fnm.c_str(),"READ");
  TTree* itree = (TTree*)ifile->Get("T");

  long nEntries = itree->GetEntries();
  cout<<"\tTotal events: "<<nEntries<<endl;

  Double_t rate;
  vector<remollGenericDetectorHit_t>  *hit=0;
  itree->SetBranchAddress("rate", &rate);
  itree->SetBranchAddress("hit", &hit);

  long nHits(0);
  for (long i=0; i < nEntries;i++){
    itree->GetEntry(i);
    for(int j=0; j<hit->size(); j++){
      if(hit->at(j).det == 5614 && hit->at(j).k > 10.*CLHEP::MeV) {
	newhit->push_back(hit->at(j));	
	newrate = rate;
	if(rate == 0)
	  newrate = 1;
	nHits++;
      }
    }
    if(newhit->size()>0){
      otree->Fill();
      newhit->clear();
      newrate = 0;
    }
  }
  ifile->Close();
  delete ifile;

  return nHits;
}

