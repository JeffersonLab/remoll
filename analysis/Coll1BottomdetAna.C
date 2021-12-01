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
// >.L Coll1BottomdetAna.C
// > Coll1BottomdetAna(<remoll output file>)


R__LOAD_LIBRARY(radDamage_cc.so)
#include "radDamage.hh"
#include "beamLineDetHistos.h"
#include <iterator>
#include <iostream>
#include <vector>
#include <fstream>

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
void initHisto_Botdet();
void fillHisto_detX();
void fillHisto_Botdet();
void writeOutput_detX();
void writeOutput_Botdet();

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

void Coll1BottomdetAna(const string& finName = "./remollout.root", const int testRun=0){
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
  ofstream output_5614;
  ofstream output_28;
  ofstream output_28_part;

  output_5614.open("output_5614_v8.txt",  ofstream::app);
  output_28.open("output_28_v8.txt",  ofstream::app);
  output_28_part.open("output_28_part_v8.txt",  ofstream::app);

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
      if (hit->at(j).det != 5614) continue;

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
      
      //Coll1 bot det: 5614
      double localXX = xx;
      double localYY = yy;
      double localRR = rr;
      double localPZ = pz;
        
      localXX = (zz - 1200.5); //857.5 for config5
      localYY = xx;
      localRR = sqrt(localXX*localXX + localYY*localYY);
      localPZ = -hit->at(j).py;
      

      if (hit->at(j).pid==11) {
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
        
      if(hit->at(j).trid==1 && hit->at(j).pid==11) beamLineem.fillHisto_Botdet(det, rdDmg, localPZ, localXX, localYY,
									       kinE, localRR,vx0,vy0,vz0,0);
        
      if(hit->at(j).trid==1 && hit->at(j).mtrid==0 && kinE>1) beamLine.fillHisto(det, 4, rdDmg, localPZ, localXX, localYY,
										 kinE, localRR,vx0,vy0,vz0,0);      

      if((sp==0 || sp==5) && kinE>1) beamLine.fillHisto(det, 1, rdDmg, localPZ, localXX, localYY,
							kinE, localRR,vx0,vy0,vz0,0);      
 
    }

    for(int j=0;j<hit->size();j++){
      if(std::isnan(rate) || std::isinf(rate)) continue;
      rate=1; //this is for beam simulations only (right now)
      if (hit->at(j).det != 28) continue;

      double kinE = hit->at(j).k;
      double rdDmg[2] = {rate,rate*kinE};
      double rr = hit->at(j).r;
      double xx = hit->at(j).x;
      double yy = hit->at(j).y;
      int det = hit->at(j).det;
      double pz = hit->at(j).pz;
      double vz0 = hit->at(j).vz;
      double vx0 = hit->at(j).vx;
      double vy0 = hit->at(j).vy;
      int idx = 0;

      for (auto &k : hits5614){
     
	double _kinE = k.energy;
	double _rr = sqrt(k.posx*k.posx + k.posy*k.posy);
	double _rdDmg[2] = {rate,rate*_kinE};
	double _xx =k.posx;
	double _yy =k.posy;
	int _det = k.dID;
	double _pz = k.pZ;

	if (hit->at(j).trid  == k.tID ){
	  ++idx;
	  xDet.fillHisto_detX(_det, _rdDmg, _pz, _xx, _yy,_kinE, _rr);
	  output_5614 <<  std::setprecision(12) << k.tID  << " " << k.PID  << " " << _det << " "  << _xx << " " << _yy << " " << _rr << " " <<  _kinE << " " << _pz << " " << k.time <<endl;

	  if(idx == 1 && hit->at(j).pid == 11){ xDet.fillHisto_detX(det, rdDmg, pz, xx, yy,kinE, rr);
	    output_28 <<  std::setprecision(12) << hit->at(j).trid << " " << hit->at(j).pid  << " " << det << " "  << xx << " " << yy << " " << rr << " " <<  kinE << " " << pz << " " << hit->at(j).t <<endl;
	    if((rr>600 && rr<1200) && kinE>1)    output_28_part <<  std::setprecision(12) << hit->at(j).trid << " " << hit->at(j).pid  << " " << det << " "  << xx << " " << yy << " " << rr << " " <<  kinE << " " << pz << " " << hit->at(j).t <<endl;
	  }

	}
      }
    }

    fin->Close();
    delete fin;
    return nEntries;
  };


  void initHisto(){
    string foutNm = Form("%s_Det5614.root",fileNm.substr(0,fileNm.find_last_of(".")).c_str());

    fout = new TFile(foutNm.c_str(),"RECREATE");

    // void beamLineDetHistos::initHisto(TFile *fout, const int detID, const string detNm, 
    // 				  const float vxRgMin , const float vxRgMax,
    // 				  const float vyRgMin , const float vyRgMax, 
    // 				  const float vzRgMin , const float vzRgMax,
    // 				  const string postfix = "", const float range=2000, const int subDet=0)
    beamLine.initHisto(fout,5614,"c1 Out Bottom"     ,-3500,3500,-3500,3500,-5600,7000,"",3500);
    beamLineem.initHisto_Botdet(fout,5614,"c1 Out Bottom trackid==1","tr",3500);

    xDet.initHisto_detX(fout,28,"Main detector for only electrons");
    xDet.initHisto_detX(fout,5614,"c1 Out Bottom detector for only electrons");

  }

  void writeOutput(){

    double scaleFactor = 1./nFiles;
    if(beamGen)
      scaleFactor = 1./nTotEv;
    beamLine.writeOutput(fout,5614,scaleFactor);
    
    beamLineem.writeOutput_Botdet(fout,5614,scaleFactor);

    xDet.writeOutput_detX(fout,28,scaleFactor);
    xDet.writeOutput_detX(fout,5614,scaleFactor);

    fout->Close();
  }
