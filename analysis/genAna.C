// Run commands in this order:
//
// //Start reroot
// >/path/to/build/reroot
//
// //Load in the script, and run it
// > .L radDamage.cc+
// > gSystem->Load("radDamage_cc.so");
//
// //Load in the script, and run it
// >.L genAna.C
// > genAna(<remoll output file>,<1 for beam generator, 0 else>, det numner)
// run for detnumber 6666,6667,6668,6669

#include "radDamage.hh"
#include "histogramUtilities.h"
#include "GenDetHist.h"

TFile *fout;
string fileNm;
int beamGen(1);
long nTotEv(0);
int nFiles(0);
long currentEvNr(0);
int detUT(0);
radDamage radDmg;

void initHisto();
void writeOutput();
long processOne(string);
void process();

void genAna(const string& finName = "./remollout.root", int beamGenerator=1, int testdet=0){
  fileNm = finName;
  beamGen = beamGenerator;
  detUT = testdet;
  initHisto();
  process();
  writeOutput();
}

void process(){

  if(fileNm==""){
    cout<<"\t did not find input file. Quitting!"<<endl;
    return 2;
  }

  if( fileNm.find(".root") < fileNm.size() ){
    cout<<"Processing single file:\n\t"<<fileNm<<endl;
    nTotEv+=processOne(fileNm);
    nFiles=1;
  }else{
    cout<<"Attempting to process list of output from\n\t"<<fileNm<<endl;
    ifstream ifile(fileNm.c_str());
    string data;
    while(ifile>>data){
      cout<<" processing: "<<data<<endl;
      nTotEv+=processOne(data);
      nFiles++;
    }
  }

  cout<<"\nFinished processing a total of "<<nTotEv<<endl;
}

long processOne(string fnm){
  TFile *fin=TFile::Open(fnm.c_str(),"READ");
  if(!fin->IsOpen() || fin->IsZombie()){
    cout<<"Problem: can't find file: "<<fnm<<endl;
    fin->Close();
    delete fin;
    return 0;
  }
  TTree *t=(TTree*)fin->Get("T");
  if (t == 0) return 0;
  Double_t rate=0;
  remollEvent_t *ev=0;
  remollBeamTarget_t *bm=0;
  std::vector<remollEventParticle_t> *part=0;
  std::vector<remollGenericDetectorHit_t> *hit=0;
  //std::vector<remollGenericDetectorSum_t> *sum;
  t->SetBranchAddress("rate", &rate);
  t->SetBranchAddress("bm", &bm);
  t->SetBranchAddress("ev", &ev);
  t->SetBranchAddress("part", &part);
  t->SetBranchAddress("hit", &hit);
  //t->SetBranchAddress("sum", &sum);

  long nEntries = t->GetEntries();
  cout<<"\tTotal events: "<<nEntries<<endl;
  float currentProc=1,procStep=30;
  vector<int> procID;
  int sector(-1);

  //for (Long64_t event = 0; event < 5; t->GetEntry(event++)) {
  for (Long64_t event = 0; event < nEntries/1.; t->GetEntry(event++)) {
    currentEvNr++;
    if( float(event+1)/nEntries*100 > currentProc){
      cout<<"at tree entry\t"<<event<<"\t"<< float(event+1)/nEntries*100<<endl;
      currentProc+=procStep;
    }

    for(int j=0;j<hit->size();j++){

      if(std::isnan(rate) || std::isinf(rate)) continue;
      if(rate==0) {rate=1;}

      int sp = spM[int(hit->at(j).pid)]-1;
      if(sp==-1) continue;

      double kinE = hit->at(j).k;
      double Edep = hit->at(j).edep;
      int det = hit->at(j).det;
 
      if(det==6666 || det==6668)//this is done because these two dets are kryptonite
	kinE=Edep;

      double niel = radDmg.GetNIEL(hit->at(j).pid,kinE,0);
      if(niel<0) niel=0;
      double vz0 = hit->at(j).vz;
      double vx0 = hit->at(j).vx;
      double vy0 = hit->at(j).vy;
      double vr0 = sqrt(pow(hit->at(j).vx,2)+pow(hit->at(j).vy,2));
      double rr=hit->at(j).r;

      double rdDmg[3]={rate,rate*kinE,rate*niel};
      double xx = hit->at(j).x;
      double yy = hit->at(j).y;
      double zz = hit->at(j).z;
      //if(det==6666 || det==6668 || det==6667 || det==6669)
      //if(det==6666 || det==6668)
      //std::cout<<"Det  "<<det<<"  kinE  "<<kinE<<" hit.e  "<<hit->at(j).e<<"  hit.m "<<hit->at(j).m<<" hit.edep "<<hit->at(j).edep<<"  hit.x  "<<hit->at(j).x<<"  hit.vx "<<hit->at(j).vx<<std::endl; 

      if(det==detUT)
	fillHisto_gendet(sp, rdDmg, xx, yy, zz, vx0, vy0, vz0,rr,kinE,sector,det);

      if((sp==0 || sp==5) && kinE>1){
	if(det==detUT)
	  fillHisto_gendet(1, rdDmg, xx, yy, zz, vx0, vy0, vz0,rr,kinE,sector,det);

	if(hit->at(j).trid==1 || hit->at(j).trid==2){
	  if(det==detUT)
	    fillHisto_gendet(4, rdDmg, xx, yy, zz, vx0, vy0, vz0,rr,kinE,sector,det);
	}
      }

    }
  }

  fin->Close();
  delete fin;
  return nEntries;
};


void initHisto(){
  string foutNm = Form("%s_det_%d.root",fileNm.substr(0,fileNm.find_last_of(".")).c_str(),detUT);

  fout = new TFile(foutNm.c_str(),"RECREATE");
  initHisto_gendet(detUT,fout);

}

void writeOutput(){

  double scaleFactor = 1./nFiles;
  if(beamGen)
    scaleFactor = 1./nTotEv;

  writeOutput_gendet(detUT,fout,scaleFactor);

  fout->Close();
}
