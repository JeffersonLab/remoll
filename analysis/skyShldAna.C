// Run commands in this order:
//
// //Start reroot
// >/path/to/build/reroot
//
// //Load in the script, and run it
// > .L radDamage.cc+
// 
R__LOAD_LIBRARY(radDamage_cc.so)
//
// //Load in the script, and run it
// >.L heShldAna.C
// > heShldAna(<remoll output file>,
//          <additional scale factor>,
//          <0 to update the file, 1 to recreate>, 
//          <1 for beam generator, 0 else>)

#include "radDamage.hh"
#include "histogramUtilities.h"
#include "beamLineDetHistos.h"
#include "flatHEdetHistos.h"

TFile *fout;
string fileNm;
int beamGen(1);
long nTotEv(0);
int nFiles(0);
long currentEvNr(0);

radDamage radDmg;
flatHEdetHistos flatHE;
flatHEdetHistos flatHEhole;
const double dProjY = 60*25.4; //60 inches [mm]
flatHEdetHistos flatHEholeProj;

void initHisto(int);
void writeOutput(double);
long processOne(string);
void process();

const std::vector<int> planeDets={5555};

void skyShldAna(const string& finName = "./remollout.root", double addScale=1, int overWriteFile=1, int beamGenerator=1){
  fileNm = finName;
  beamGen = beamGenerator;

  initHisto(overWriteFile);
  process();
  writeOutput(addScale);
}

void process(){
  if(fileNm==""){
    cout<<"\t did not find input file. Quitting!"<<endl;
    return;
  }

  if (fileNm.find(".root") < fileNm.size()) {
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

long processOne(string fnm) {
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
      double tgtZZ = zz + 4500;
      double px = hit->at(j).px;
      double py = hit->at(j).py;
      double pz = hit->at(j).pz;
      int det = hit->at(j).det;

      if(det!=5555) continue;

      if(kinE > 10){
	//cout<<xx<<" "<<yy<<" "<<zz<<" "<<px<<" "<<py<<" "<<pz<<endl;
	if(sqrt(xx*xx + tgtZZ*tgtZZ) < 1000)
	  flatHEhole.fillHisto(det, sp, py, xx, tgtZZ, vx0, vy0, vz0);	
	flatHE.fillHisto(det, sp, py, xx, tgtZZ, vx0, vy0, vz0);	
	
	double projX = xx - px/py *dProjY;
	double projZ = tgtZZ - pz/py *dProjY;
	if(sqrt(projX*projX + projZ*projZ) < 1000)
	  flatHEholeProj.fillHisto(det, sp, py, projX, projZ, vx0, vy0, vz0);
	// cout<<"\t"<<projX<<" "<<projZ<<endl;
	// std::cin.ignore();
      }
    }
  }
  fin->Close();
  delete fin;
  return nEntries;
};


void initHisto(int fileType){
  string foutNm = Form("%s_skyShldAnaV1.root",fileNm.substr(0,fileNm.find_last_of(".")).c_str());

  const char * fTp[2]={"UPDATE","RECREATE"};
  cout<<"Will " << fTp[fileType] << " file!" << endl;
  fout = new TFile(foutNm.c_str(), fTp[fileType]);
  

  flatHE.initHisto(fout, 5555, "Flat: inside tgt bunker above tgt", "", 4000, -7000, -2000);
  flatHEhole.initHisto(fout, 5555, "Flat: inside tgt bunker above tgt with hole", "Hole", 4000, -7000, -2000);
  flatHEholeProj.initHisto(fout, 5555, "Flat: inside tgt bunker above tgt with hole 60 in below", "HoleProj", 4000, -7000, -2000);
}

void writeOutput(double addScale){
  double scaleFactor = 1./nFiles;
  if(beamGen)
    scaleFactor = 1./nTotEv;

  scaleFactor /= addScale;

  flatHE.writeOutput(fout, 5555, scaleFactor);
  flatHEhole.writeOutput(fout, 5555, scaleFactor);
  flatHEholeProj.writeOutput(fout, 5555, scaleFactor);

  fout->Close();
}
