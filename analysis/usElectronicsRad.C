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
// >.L usElectronicsRad.C
// > usElectronicsRad(<remoll output file>,
//          <additional scale factor>,
//          <0 to update the file, 1 to recreate>, 
//          <1 for beam generator, 0 else>)

#include "radDamage.hh"
#include "histogramUtilities.h"
#include "beamLineDetHistos.h"

TFile *fout;
string fileNm;
int beamGen(1);
long nTotEv(0);
int nFiles(0);
long currentEvNr(0);

radDamage radDmg;
beamLineDetHistos beamLine;

void initHisto(int);
void writeOutput(double);
long processOne(string);
void process();

void usElectronicsRad(const string& finName = "./remollout.root", double addScale=1, int overWriteFile = 1, int beamGenerator=1){
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

      int det = hit->at(j).det;
      if(det!=5500 && det!=5501 && det!=5502 && det!=5542) continue;

      double kinE = hit->at(j).k;
      double niel = radDmg.GetNIEL(hit->at(j).pid,kinE,0);
      if(niel<0) niel=0;
      double rr=hit->at(j).r;

      double rdDmg[3]={rate,rate*kinE,rate*niel};
      double xx = hit->at(j).x;
      double yy = hit->at(j).y;
      double zz = hit->at(j).z;
      double pz = hit->at(j).pz;

      if(det==5500)      
	beamLine.fillHisto(det, sp, rdDmg, pz, xx, yy, kinE);
      else if(det==5542){
	if(sqrt(xx*xx+yy*yy)<100)
	  beamLine.fillHisto(det, sp, rdDmg, pz, xx, yy, kinE,1);
	else if( yy<-1000 && yy>-2000 && abs(xx)<500)
	  beamLine.fillHisto(det, sp, rdDmg, pz, xx, yy, kinE,2);
      }else{
	if(det==5501 && sqrt(xx*xx+yy*yy)>200) continue;
	if(det==5502 && ((abs(xx)>300 || abs(xx)<100) || (yy>-200 || yy<-500))) continue;
	beamLine.fillHisto(det, sp, rdDmg, pz, xx, yy, kinE,1);
      }

    }
  }

  fin->Close();
  delete fin;
  return nEntries;
};


void initHisto(int fileType){
  string foutNm = Form("%s_usElectronicsRadV0.root",fileNm.substr(0,fileNm.find_last_of(".")).c_str());

  const string fTp[2]={"UPDATE","RECREATE"};
  cout<<"Will "<<fTp[fileType]<<" file!"<<endl;
  fout = new TFile(foutNm.c_str(),fTp[fileType].c_str());

  beamLine.initHisto(fout,5500,"Flat: beamline hall entrance");
  beamLine.initHisto(fout,5501,"Flat: beamline moller pol tgt R<20cm","r20cm",200,1);
  beamLine.initHisto(fout,5502,"Flat: beamline moller pol det","det",500,1);

  beamLine.initHisto(fout,5542,"Flat: outside tgt bunker US tgt beamline","bmLine",100,1);
  beamLine.initHisto(fout,5542,"Flat: outside tgt bunker US tgt beamline","electronics",2000,2);

}

void writeOutput(double addScale){

  double scaleFactor = 1./nFiles;
  if(beamGen)
    scaleFactor = 1./nTotEv;

  scaleFactor /= addScale;

  beamLine.writeOutput(fout,5500,scaleFactor);
  beamLine.writeOutput(fout,5501,scaleFactor,1);
  beamLine.writeOutput(fout,5502,scaleFactor,1);

  beamLine.writeOutput(fout,5542,scaleFactor,1);
  beamLine.writeOutput(fout,5542,scaleFactor,2);

  fout->Close();
}
