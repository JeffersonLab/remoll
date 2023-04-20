// Run commands in this order:
//
// //Start reroot
// >/path/to/build/reroot
//
// //Load in the script, and run it
// > .L radDamage.cc+
// 
//
// //Load in the script, and run it
// >.L tgtShldAna.C
// > tgtShldAna(<remoll output file>,
//          <1 for tgt sphere, 2 for plane dets, 4 for hall Det>, 
//          <additional scale factor>,
//          <0 to update the file, 1 to recreate>, 
//          <1 for beam generator, 0 else>)

R__LOAD_LIBRARY(radDamage_cc.so)

#include "radDamage.hh"
#include "histogramUtilities.h"
#include "beamLineDetHistos.h"
#include "det28Histos.h"
#include "mainDetUtilities.h"

TFile *fout;
string fileNm;
int beamGen(1);
long nTotEv(0);
int nFiles(0);
long currentEvNr(0);

radDamage radDmg;
beamLineDetHistos beamLine;
det28Histos mainDet;

void initHisto(int);
void writeOutput(double);
long processOne(string);
void process();

void tgtDSana(const string& finName = "./remollout.root", double addScale=1, int overWriteFile = 1, int beamGenerator=1){
  fileNm = finName;
  beamGen = beamGenerator;

  beamLine.SetAnaDet(3); //std+source

  initHisto(overWriteFile);
  process();
  writeOutput(addScale);
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
  for (Long64_t event = 0; event < nEntries; event++) {
    currentEvNr++;
    if( float(event+1)/nEntries*100 > currentProc){
      cout<<"at tree entry\t"<<event<<"\t"<< float(event+1)/nEntries*100<<endl;
      currentProc+=procStep;
    }

    t->GetEntry(event);
    for(int j=0;j<hit->size();j++){

      if(std::isnan(rate) || std::isinf(rate)) continue;
      if(rate==0) {rate=1;}

      int sp = spM[int(hit->at(j).pid)]-1;
      if(sp==-1) continue;

      double kinE = hit->at(j).k;
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
      double pz = hit->at(j).pz;
      int det = hit->at(j).det;

      beamLine.fillHisto(det, sp, rdDmg, pz, xx, yy, kinE, vx0, vy0, vz0);
      if((sp==0 || sp==5) && kinE>1){
	beamLine.fillHisto(det, 1, rdDmg, pz, xx, yy, kinE, vx0, vy0, vz0);
	if((hit->at(j).trid==1 || hit->at(j).trid==2) && hit->at(j).mtrid==0){
	  beamLine.fillHisto(det, 4, rdDmg, pz, xx, yy, kinE, vx0, vy0, vz0);
	}
      }

      if(det!=28) continue;

      double phi = atan2(hit->at(j).y,hit->at(j).x);
      if(phi<0) phi+=2*pi;
      int foundRing = findDetector(sector, phi, hit->at(j).r,1);
      if(foundRing==-1) continue;

      mainDet.fillHisto(sp,foundRing+1, rdDmg, xx, yy, vx0, vy0, vz0,rr,kinE,pz,sector);

      if((sp==0 || sp==5) && kinE>1){
	mainDet.fillHisto(1,foundRing+1, rdDmg, xx, yy, vx0, vy0, vz0,rr,kinE,pz,sector);

	if((hit->at(j).trid==1 || hit->at(j).trid==2) && hit->at(j).mtrid==0){
	  mainDet.fillHisto(4,foundRing+1, rdDmg, xx, yy, vx0, vy0, vz0,rr,kinE,pz,sector);
	}
      }
      
    }
  }

  fin->Close();
  delete fin;
  return nEntries;
};


void initHisto(int fileType){
  string foutNm = Form("%s_tgtDSanaV0.root",fileNm.substr(0,fileNm.find_last_of(".")).c_str());

  const string fTp[2]={"UPDATE","RECREATE"};
  cout<<"Will "<<fTp[fileType]<<" file!"<<endl;
  fout = new TFile(foutNm.c_str(),fTp[fileType].c_str());

  beamLine.initHisto(fout,5543,"Flat: outside tgt bunker DS",
		     -6000,6000,
		     -6000,6000,
		     -6000,-1000,
		     "",4000,0);

  mainDet.initHisto(fout);
  
}

void writeOutput(double addScale){

  double scaleFactor = 1./nFiles;
  if(beamGen)
    scaleFactor = 1./nTotEv;

  scaleFactor /= addScale;

  beamLine.writeOutput(fout,5543,scaleFactor);
  mainDet.writeOutput(fout,scaleFactor);

  fout->Close();
}
