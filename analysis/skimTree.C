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

double scaleRate(1);

long processOne(string fnm);
long getEvents(string);

void skimTree(string finNm, int testRun=0, int beamGen=1){
  
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
    if(beamGen){
      scaleRate = getEvents(finNm);
    }

    nTotHits+=processOne(finNm);
    nFiles=1;
  }else{
    cout<<"Attempting to process list of output from\n\t"<<finNm<<endl;
    ifstream ifile(finNm.c_str());
    string data;
    while(ifile>>data){
      nFiles++;
      if(beamGen)
	scaleRate += getEvents(data);

      if(testRun==1 && nFiles>10)
	break;
    }
    ifile.close();
    if(!beamGen)
      scaleRate = nFiles;
    nFiles=0;

    cout<<"\t scale rate by "<<scaleRate<<endl;

    ifile.open(finNm.c_str());
    while(ifile>>data){
      nFiles++;
      cout<<" processing: "<<data<<endl;
      nTotHits+=processOne(data);
      if(testRun==1 && nFiles>10)
	break;
    }
  }

  ofile->cd();
  otree->Write();
  ofile->Close();

  cout<<"\nFinished processing a total of "<<nTotHits<<" hits."<<endl;
}

long getEvents(string fnm){
  TFile* ifile = new TFile(fnm.c_str(),"READ");
  TTree* itree = (TTree*)ifile->Get("T");
  long evn=0;
  if(itree)
    evn = itree->GetEntries();

  ifile->Close();
  delete ifile;

  return evn;
}


long processOne(string fnm){

  TFile* ifile = new TFile(fnm.c_str(),"READ");
  TTree* itree = (TTree*)ifile->Get("T");

  if(!itree) 
    return 0;

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
      if((hit->at(j).det == 5522 || hit->at(j).det == 5523 || 
	  hit->at(j).det == 5524) &&
      	 hit->at(j).r>500  && hit->at(j).pz < 0 )
	{
	  newhit->push_back(hit->at(j));	
	  newrate = rate/scaleRate;
	  
	  if(rate == 0)
	    newrate = 1/scaleRate;
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

