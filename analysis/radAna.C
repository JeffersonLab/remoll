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
// >.L radAna.C
// > radAna(<remoll output file>,
//          <1 for MD, 2 for beamline, 4 for hall Det>, 
//          <0 to update the file, 1 to recreate>, 
//          <1 for beam generator, 0 else>)

#include "radDamage.hh"
#include "histogramUtilities.h"
#include "mainDetUtilities.h"
#include "det28Histos.h"
#include "beamLineDetHistos.h"
#include "hallDetHistos.h"

TFile *fout;
string fileNm;
int beamGen(1);
long nTotEv(0);
int nFiles(0);
long currentEvNr(0);
int analyzeDet(0);

radDamage radDmg;

void initHisto(int);
void writeOutput();
long processOne(string);
void process();

void radAna(const string& finName = "./remollout.root", int anaDet=1, int overWriteFile = 1, int beamGenerator=1){
  fileNm = finName;
  beamGen = beamGenerator;

  analyzeDet = anaDet;
  int foundDet(0);
  const string anaNm[3]={"MD","beamline","hall"};
  for(int i=0;i<3;i++)
    if( (analyzeDet & int(pow(2,i))) == int(pow(2,i)) ){
      cout<<"You decided to analyze "<<anaNm[i]<<endl;
      foundDet++;
    }
  if(!foundDet){
    cout<<"For now you can only analyze\n\tthe main Detector: anaDet=1\n\tbeamline detectors: anaDet=2\n\thall detector: anaDet=4"<<endl;
    cout<<"You provided "<<anaDet<<" Quitting!!"<<endl;
    return;
  }

  initHisto(overWriteFile);
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
      int det = hit->at(j).det;
      double pz = hit->at(j).pz;
      if(det==28  && ( (analyzeDet & 1) ==1))
	fillHisto_det28(sp,0, rdDmg, xx, yy, vx0, vy0, vz0,rr,kinE,pz,0);
      else if(det>=40 && det<=46  && ( (analyzeDet & 2) ==2))
	fillHisto_beamLine(det, sp, rdDmg, pz, xx, yy, kinE);
      else if( (det==99 || det==101) && ((analyzeDet & 4) == 4) )
	fillHisto_hall(det,sp,rdDmg,xx,yy,zz,vx0,vy0,vz0,kinE);

      if((sp==0 || sp==5) && kinE>1){
	if(det==28 && ( (analyzeDet & 1) ==1))
	  fillHisto_det28(1,0, rdDmg, xx, yy, vx0, vy0, vz0,rr,kinE,pz,0);
	else if(det>=40 && det<=46  && ( (analyzeDet & 2) ==2))
	  fillHisto_beamLine(det, 1, rdDmg, pz, xx, yy, kinE);
	else if( (det==99 || det==101) && ((analyzeDet & 4) == 4) )
	  fillHisto_hall(det,1,rdDmg,xx,yy,zz,vx0,vy0,vz0,kinE);

	if((hit->at(j).trid==1 || hit->at(j).trid==2) && hit->at(j).mtrid==0){
	  if(det==28 && ( (analyzeDet & 1) ==1))
	    fillHisto_det28(4,0, rdDmg, xx, yy, vx0, vy0, vz0,rr,kinE,pz,0);
	  else if(det>=40 && det<=46  && ( (analyzeDet & 2) ==2))
	    fillHisto_beamLine(det, 4, rdDmg, pz, xx, yy, kinE);
	  else if( (det==99 || det==101) && ((analyzeDet & 4) == 4) )
	    fillHisto_hall(det,4,rdDmg,xx,yy,zz,vx0,vy0,vz0,kinE);

	}
      }
      
      double phi = atan2(hit->at(j).y,hit->at(j).x);
      if(phi<0) phi+=2*pi;
      int foundRing = findDetector(sector, phi, hit->at(j).r,1);
      if(foundRing==-1) continue;

      if(det==28 && ( (analyzeDet & 1) ==1))
	fillHisto_det28(sp,foundRing+1, rdDmg, xx, yy, vx0, vy0, vz0,rr,kinE,pz,sector);

      if((sp==0 || sp==5) && kinE>1){
	if(det==28 && ( (analyzeDet & 1) ==1))
	  fillHisto_det28(1,foundRing+1, rdDmg, xx, yy, vx0, vy0, vz0,rr,kinE,pz,sector);

	if(hit->at(j).trid==1 || hit->at(j).trid==2){
	  if(det==28 && ( (analyzeDet & 1) ==1))
	    fillHisto_det28(4,foundRing+1, rdDmg, xx, yy, vx0, vy0, vz0,rr,kinE,pz,sector);
	}
      }

    }
  }

  fin->Close();
  delete fin;
  return nEntries;
};


void initHisto(int fileType){
  string foutNm = Form("%s_radAnaV5.root",fileNm.substr(0,fileNm.find_last_of(".")).c_str());

  const string fTp[2]={"UPDATE","RECREATE"};
  cout<<"Will "<<fTp[fileType]<<" file!"<<endl;
  fout = new TFile(foutNm.c_str(),fTp[fileType].c_str());
  if( (analyzeDet & 1) == 1)
    initHisto_det28(fout);
  if( (analyzeDet & 2) == 2){
    initHisto_beamLine(fout,40,"BL: front collar1");
    initHisto_beamLine(fout,41,"BL: front collar2");
    initHisto_beamLine(fout,42,"BL: front sam");
    initHisto_beamLine(fout,43,"BL: front dump tunnel");
    initHisto_beamLine(fout,44,"BL: front donut");
    initHisto_beamLine(fout,45,"BL: back donut");
    initHisto_beamLine(fout,46,"BL: front Al-wall");
    initHisto_beamLine(fout,51,"BL: behind diffuser");
  }
  if( (analyzeDet & 4) == 4){
    initHisto_hall(fout);
  }
}

void writeOutput(){

  double scaleFactor = 1./nFiles;
  if(beamGen)
    scaleFactor = 1./nTotEv;


  if( (analyzeDet & 1) == 1)
    writeOutput_det28(fout,scaleFactor);
  if( (analyzeDet & 2) == 2){
    writeOutput_beamLine(fout,40,scaleFactor);
    writeOutput_beamLine(fout,41,scaleFactor);
    writeOutput_beamLine(fout,42,scaleFactor);
    writeOutput_beamLine(fout,43,scaleFactor);
    writeOutput_beamLine(fout,44,scaleFactor);
    writeOutput_beamLine(fout,45,scaleFactor);
    writeOutput_beamLine(fout,46,scaleFactor);
    writeOutput_beamLine(fout,51,scaleFactor);
  }
  if( (analyzeDet & 4) == 4){
    writeOutput_hall(fout,scaleFactor);
  }

  fout->Close();
}
