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
// >.L tgtShldAna.C
// > tgtShldAna(<remoll output file>,
//          <1 for tgt sphere, 2 for plane dets, 4 for hall Det>, 
//          <additional scale factor>,
//          <0 to update the file, 1 to recreate>, 
//          <1 for beam generator, 0 else>)

#include "radDamage.hh"
#include "histogramUtilities.h"
#include "beamLineDetHistos.h"
#include "sphereDetHistos.h"
#include "hallDetHistos.h"
#include "flatHEdetHistos.h"

TFile *fout;
string fileNm;
int beamGen(1);
long nTotEv(0);
int nFiles(0);
long currentEvNr(0);

radDamage radDmg;

void initHisto(int);
void writeOutput(double);
long processOne(string);
void process();

const std::vector<int> planeDets={5555, 5556};

void tgtShldAna(const string& finName = "./remollout.root", double addScale=1, int overWriteFile = 1, int beamGenerator=1){
  fileNm = finName;
  beamGen = beamGenerator;

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
  for (Long64_t event = 0; event < nEntries; t->GetEntry(event++)) {
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
      double sphereZZ = zz + 4500;
      double pz = hit->at(j).pz;
      int det = hit->at(j).det;

      // transformed coordinates
      double zz_tr=0;
      double yy_tr=0;
      double xx_tr=0;
      double pz_tr(0);

      if (det==5555 || det==5556){
	xx_tr=xx;
	yy_tr=zz+4500;
	pz_tr = hit->at(j).py;
      }else if(det==101){
	xx_tr=xx;
	yy_tr=zz;
	pz_tr = hit->at(j).py;
      }
	

      fillHisto_beamLine(det, sp, rdDmg, pz, xx_tr, yy_tr, kinE);
      if(kinE > 10)
	fillHisto_flatHE(det, sp, pz_tr, xx_tr, yy_tr, vz0);
      
      fillHisto_hall(det,sp,rdDmg,xx,yy,zz,vx0,vy0,vz0,kinE);

      if((sp==0 || sp==5) && kinE>1){
	fillHisto_beamLine(det, 1, rdDmg, pz, xx_tr, yy_tr, kinE);
	if(kinE > 10)
	  fillHisto_flatHE(det, 1, pz_tr, xx_tr, yy_tr, vz0);

	fillHisto_hall(det,1,rdDmg,xx,yy,zz,vx0,vy0,vz0,kinE);

	if((hit->at(j).trid==1 || hit->at(j).trid==2) && hit->at(j).mtrid==0){
	  fillHisto_beamLine(det, 4, rdDmg, pz, xx_tr, yy_tr, kinE);
	  if(kinE > 10)
	    fillHisto_flatHE(det, 4, pz_tr, xx_tr, yy_tr, vz0);
	  
	  fillHisto_hall(det,4,rdDmg,xx,yy,zz,vx0,vy0,vz0,kinE);
	}
      }
      
    }
  }

  fin->Close();
  delete fin;
  return nEntries;
};


void initHisto(int fileType){
  string foutNm = Form("%s_tgtShldAnaV0.root",fileNm.substr(0,fileNm.find_last_of(".")).c_str());

  const string fTp[2]={"UPDATE","RECREATE"};
  cout<<"Will "<<fTp[fileType]<<" file!"<<endl;
  fout = new TFile(foutNm.c_str(),fTp[fileType].c_str());
  
  initHisto_beamLine(fout,5556,"Flat: inside tgt bunker above tgt");
  initHisto_beamLine(fout,5555,"Flat: outside tgt bunker above tgt");

  initHisto_flatHE(fout,5556,"Flat: inside tgt bunker above tgt",3900,-7000,-2000);
  initHisto_flatHE(fout,5555,"Flat: inside tgt bunker above tgt",3900,-7000,-2000);
  initHisto_flatHE(fout,101,"Flat: hall lid",27000);

  initHisto_hall(fout);

}

void writeOutput(double addScale){

  double scaleFactor = 1./nFiles;
  if(beamGen)
    scaleFactor = 1./nTotEv;

  scaleFactor /= addScale;

  writeOutput_beamLine(fout,5556,scaleFactor);
  writeOutput_beamLine(fout,5555,scaleFactor);

  writeOutput_flat(fout,5556,scaleFactor);
  writeOutput_flat(fout,5555,scaleFactor);
  writeOutput_flat(fout,101,scaleFactor);

  writeOutput_hall(fout,scaleFactor);


  fout->Close();
}
