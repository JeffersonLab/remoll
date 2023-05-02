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
// >.L pmtRegAna.C
// > c1ShldAna(<remoll output file>)


R__LOAD_LIBRARY(radDamage_cc.so)
#include "radDamage.hh"
#include "beamLineDetHistos.h"
#include <iterator>
#include <iostream>
#include <vector>

using namespace std;
TFile *fout;
string fileNm;
int beamGen(1);
long nTotEv(0);
int nFiles(0);
long currentEvNr(0);

radDamage radDmg;
beamLineDetHistos beamLine(3);
beamLineDetHistos beamLineHole(3);
beamLineDetHistos beamLineem;
beamLineDetHistos xDet;

void initHisto();
void initHisto_detX();
void initHisto_det5614();
void fillHisto_detX();
void fillHisto_det5614();
void writeOutput_detX();
void writeOutput_det5614();

void writeOutput();
long processOne(string);
void process(int);

struct hitx{
  int evid;
  int dID;
  int tID;
  int PID;
  double energy;
  double time;
  double posx;
  double posy;
  double pZ;
    
  hitx(int evid_ = 0, int dID_ = 0,  int tID_ = 0,  double PID_ = 0,  double energy_ = 0,  double time_ = 0,  double posx_ = 0,  double posy_ = 0, double pZ_ = 0) :
    evid(evid_), dID(dID_), tID(tID_), PID(PID_),
    energy(energy_), time(time_), posx(posx_), posy(posy_), pZ(pZ_) { };
};

void c1ShldAna(const string& finName = "./remollout.root", const int testRun=0){
  fileNm = finName;

  initHisto();
  process(testRun);
  writeOutput();
}

void process(const int testRun){

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
      if(testRun==1 && nFiles>10)
	break;
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
  //t->SetBranchAddress("bm", &bm);
  //t->SetBranchAddress("ev", &ev);
  //t->SetBranchAddress("part", &part);
  t->SetBranchAddress("hit", &hit);
  //t->SetBranchAddress("sum", &sum);

  long nEntries = t->GetEntries();
  cout<<"\tTotal events: "<<nEntries<<endl;
  float currentProc=1,procStep=60;
  vector<int> procID;
  int sector(-1);

  std::vector<hitx> hits5614;
  hitx bHit;
  for (Long64_t event = 0; event < nEntries; t->GetEntry(event++)) {
    currentEvNr++;
    if( float(event+1)/nEntries*100 > currentProc){
      cout<<"at tree entry\t"<<event<<"\t"<< float(event+1)/nEntries*100<<endl;
      currentProc+=procStep;
    }
   
    hits5614.clear();

    for(int j=0;j<hit->size();j++){

      if(std::isnan(rate) || std::isinf(rate)) continue;
      rate=1; //this is for beam simulations only (right now)

      int sp = spM[int(hit->at(j).pid)]-1;
        
      if(sp==-1) continue;
      double kinE = hit->at(j).k;

      double niel = radDmg.GetNIEL(hit->at(j).pid,kinE,0);
      if(niel<0) niel=0;
      double vz0 = hit->at(j).vz;
      double vx0 = hit->at(j).vx;
      double vy0 = hit->at(j).vy;
      double vr0 = sqrt(pow(hit->at(j).vx,2)+pow(hit->at(j).vy,2));
      double rr = hit->at(j).r;

      double rdDmg[3] = {rate,rate*kinE,rate*niel};
      double xx = hit->at(j).x;
      double yy = hit->at(j).y;
      double zz = hit->at(j).z;
      int det = hit->at(j).det;
      double pz = hit->at(j).pz;
      
      //beam perp: 5620, 5543
      //side det: 5600, 5610, 5601, 5611, 
      //top/bot det: 5603,5613, 5614
      //magnetPShutFrontFace: 5710
      //hybridIronVirtualPlane: 5714
      double localXX = xx;
      double localYY = yy;
      double localRR = rr;
      double localPZ = pz;
      if( det == 5600 || det == 5610 ){
	localXX = (zz - 1200.5); //857.5 for config5
	localYY = yy;
	localRR = sqrt(localXX*localXX + localYY*localYY);
	localPZ = -hit->at(j).px;
      }else if( det == 5601 || det == 5611 ){
	localXX = (zz - 1200.5); //857.5 for config5
	localYY = yy;
	localRR = sqrt(localXX*localXX + localYY*localYY);
	localPZ = hit->at(j).px;
      }else if( det == 5603 || det == 5613 ){
	localXX = (zz - 1200.5); //857.5 for config5
	localYY = xx;
	localRR = sqrt(localXX*localXX + localYY*localYY);
	localPZ = hit->at(j).py;
      }else if( det==5614){
	localXX = (zz - 1200.5); //857.5 for config5
	localYY = xx;
	localRR = sqrt(localXX*localXX + localYY*localYY);
	localPZ = -hit->at(j).py;
      }else if( det==5714){ 
	localXX = (zz - 12000); // y="-1670" z="12000" in mollerparallel.gdml
	localYY = xx;
	localRR = sqrt(localXX*localXX + localYY*localYY);
	localPZ = -hit->at(j).py;
      }
      else if( det==5710){ 
	localXX = (zz - 8000); //x="5740" y="-1000" z="8000" in mollerparallel.gdml
	localYY = (yy + 1000);
	localRR = sqrt(localXX*localXX + localYY*localYY);
      }

      if ( det==5614 && hit->at(j).pid==11) {
	bHit.evid =event;
	bHit.dID = hit->at(j).det;
	bHit.tID = hit->at(j).trid;
	bHit.PID = hit->at(j).pid;
	bHit.energy = hit->at(j).k;
	bHit.time = hit->at(j).t;
	bHit.posx = localXX;
	bHit.posy = localYY;
	bHit.pZ = localPZ;
	hits5614.push_back(bHit);
      }
                
      beamLine.fillHisto(det, sp, rdDmg, localPZ, localXX, localYY, 
			 kinE, localRR,vx0,vy0,vz0,0);
        
      if((det==5620 || det==5619 ) && rr > 265) beamLineHole.fillHisto(det, sp, rdDmg, localPZ, localXX, localYY,
								       kinE, localRR,vx0,vy0,vz0,0);

      if(det==5614 && hit->at(j).trid==1 && hit->at(j).pid==11) beamLineem.fillHisto_det5614(det, rdDmg, localPZ, localXX, localYY,
                                                                                             kinE, localRR,vx0,vy0,vz0,0);

      if(hit->at(j).trid==1 && hit->at(j).mtrid==0 && kinE>1) beamLine.fillHisto(det, 4, rdDmg, localPZ, localXX, localYY,
										 kinE, localRR,vx0,vy0,vz0,0);      

      if((sp==0 || sp==5) && kinE>1) beamLine.fillHisto(det, 1, rdDmg, localPZ, localXX, localYY,
							kinE, localRR,vx0,vy0,vz0,0);      
 
    }

    for(int j=0;j<hit->size();j++){
      if(std::isnan(rate) || std::isinf(rate)) continue;
      rate=1; //this is for beam simulations only (right now)
      if ( hit->at(j).det != 28) continue;

      double kinE = hit->at(j).k;
      double rdDmg[2] = {rate,rate*kinE};
      double rr = hit->at(j).r;
      double xx = hit->at(j).x;
      double yy = hit->at(j).y;
      int det = hit->at(j).det;
      double pz = hit->at(j).pz;
        
      for (auto &k : hits5614){
	double _kinE = k.energy;
	double _rr = sqrt(k.posx*k.posx + k.posy*k.posy);
	double _rdDmg[2] = {rate,rate*_kinE};
	double _xx =k.posx;
	double _yy =k.posy;
	int _det = k.dID;
	double _pz = k.pZ;

	if (hit->at(j).trid  == k.tID ){
	  xDet.fillHisto_detX(_det, _rdDmg, _pz, _xx, _yy,_kinE, _rr);
	  if(hit->at(j).pid == 11)   xDet.fillHisto_detX(det, rdDmg, pz, xx, yy,kinE, rr);
	}

      }
    }
  }

  fin->Close();
  delete fin;
  return nEntries;
};


void initHisto(){
  string foutNm = Form("%s_c1CollAnaV1.root",fileNm.substr(0,fileNm.find_last_of(".")).c_str());

  fout = new TFile(foutNm.c_str(),"RECREATE");

  // void beamLineDetHistos::initHisto(TFile *fout, const int detID, const string detNm, 
  // 				  const float vxRgMin , const float vxRgMax,
  // 				  const float vyRgMin , const float vyRgMax, 
  // 				  const float vzRgMin , const float vzRgMax,
  // 				  const string postfix = "", const float range=2000, const int subDet=0)
  beamLine.initHisto(fout,5620,"After C4PbWall"    ,-3500,3500,-3500,3500,-5600,6000,"",3500);
  beamLine.initHisto(fout,5619,"Before C4PbWall"   ,-3500,3500,-3500,3500,-5600,6000,"",3500);
  beamLine.initHisto(fout,5543,"After Tgt bunker"  ,-3500,3500,-3500,3500,-5600,6000,"",3500);

  beamLine.initHisto(fout,5600,"c1 In  Beam Right" ,-3500,3500,-3500,3500,-5600,6000,"",3500);
  beamLine.initHisto(fout,5610,"c1 Out Beam Right" ,-3500,3500,-3500,3500,-5600,6000,"",3500);
  beamLine.initHisto(fout,5601,"c1 In  Beam Left"  ,-3500,3500,-3500,3500,-5600,6000,"",3500);
  beamLine.initHisto(fout,5611,"c1 Out Beam Left"  ,-3500,3500,-3500,3500,-5600,6000,"",3500);

  beamLine.initHisto(fout,5613,"c1 Out Top"        ,-3500,3500,-3500,3500,-5600,6000,"",3500);
  beamLine.initHisto(fout,5603,"c1 In  Top"        ,-3500,3500,-3500,3500,-5600,6000,"",3500);
  beamLine.initHisto(fout,5614,"c1 Out Bottom"     ,-3500,3500,-3500,3500,-5600,6000,"",3500);
  beamLine.initHisto(fout,5714,"c1 hybrid Bottom"  ,-3500,3500,-3500,3500,-5600,25000,"",8000);
  beamLine.initHisto(fout,5710,"c1 magnetPShut Front Face" ,-4000,4000,-4000,4000,-5600,15000,"",6000);

  beamLineHole.initHisto(fout,5620,"After C4PbWall with Hole" ,-3500,3500,-3500,3500,-5600,6000,"hole",3500);
  beamLineHole.initHisto(fout,5619,"Before C4PbWall with Hole" ,-3500,3500,-3500,3500,-5600,6000,"hole",3500);
  beamLineem.initHisto_det5614(fout,5614,"c1 Out Bottom trackid==1","tr",3500);

  xDet.initHisto_detX(fout,28,"Main detector for only electrons");
  xDet.initHisto_detX(fout,5614,"c1 Out Bottom detector for only electrons");

}

void writeOutput(){

  double scaleFactor = 1./nFiles;
  if(beamGen)
    scaleFactor = 1./nTotEv;

  beamLine.writeOutput(fout,5543,scaleFactor);
  beamLine.writeOutput(fout,5620,scaleFactor);
  beamLine.writeOutput(fout,5619,scaleFactor);
  beamLine.writeOutput(fout,5600,scaleFactor);
  beamLine.writeOutput(fout,5601,scaleFactor);
  beamLine.writeOutput(fout,5603,scaleFactor);
  beamLine.writeOutput(fout,5610,scaleFactor);
  beamLine.writeOutput(fout,5611,scaleFactor);
  beamLine.writeOutput(fout,5613,scaleFactor);
  beamLine.writeOutput(fout,5614,scaleFactor);
  beamLine.writeOutput(fout,5714,scaleFactor);
  beamLine.writeOutput(fout,5710,scaleFactor);
    
  beamLineHole.writeOutput(fout,5619,scaleFactor);
  beamLineHole.writeOutput(fout,5620,scaleFactor);
  beamLineem.writeOutput_det5614(fout,5614,scaleFactor);

  xDet.writeOutput_detX(fout,28,scaleFactor);
  xDet.writeOutput_detX(fout,5614,scaleFactor);

  fout->Close();
}
