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

TFile *fout;
string fileNm;
int beamGen(1);
long nTotEv(0);
int nFiles(0);
long currentEvNr(0);
int analyzeDet(0);

radDamage radDmg;
beamLineDetHistos beamLine;
sphereDetHistos sphere;
hallDetHistos hall;

void initHisto(int);
void writeOutput(double);
long processOne(string);
void process();

const std::vector<int> planeDets={5500, 5501, 5510, 5540, 5541, 5542, 5543, 5544, 5545, 5546, 5547, 5555, 5556, 5531,5560};//FIXME add side/over/sbs dets when ready

void tgtShldAna(const string& finName = "./remollout.root", int anaDet=1, double addScale=1, int overWriteFile = 1, int beamGenerator=1){
  fileNm = finName;
  beamGen = beamGenerator;

  analyzeDet = anaDet;
  int foundDet(0);
  const string anaNm[3]={"tgt","planes","hall"};
  for(int i=0;i<3;i++)
    if( (analyzeDet & int(pow(2,i))) == int(pow(2,i)) ){
      cout<<"You decided to analyze "<<anaNm[i]<<endl;
      foundDet++;
    }
  if(!foundDet){
    cout<<"For now you can only analyze\n\t sphere around tgt: anaDet=1\n\tplane detectors: anaDet=2\n\thall detector: anaDet=4"<<endl;
    cout<<"You provided "<<anaDet<<" Quitting!!"<<endl;
    return;
  }

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
      double sphereZZ = zz + 4500;
      double pz = hit->at(j).pz;
      int det = hit->at(j).det;

      TVector3 mom(hit->at(j).px,hit->at(j).py,hit->at(j).pz);
      TVector3 pos(hit->at(j).x,hit->at(j).y,hit->at(j).z-4500);
      double spherePZ = mom*pos;
      if(det==5530 && spherePZ<0 && (hit->at(j).trid==1 || hit->at(j).trid==2) && hit->at(j).mtrid==0 && kinE>10950)
          continue;//for primaries coming into the sphere
      bool doBeamLine = std::find(planeDets.begin(), planeDets.end(), det) != planeDets.end();
      
      // transformed coordinates
      double zz_tr=0;
      double yy_tr=0;
      double xx_tr=0;

      if (det==5540 || det==5541 || det==5544 || det==5545){
	  xx_tr=zz+4500;
	  yy_tr=yy;
      } else if (det==5555 || det==5556){
	  xx_tr=xx;
	  yy_tr=zz+4500;
      } else if (det==5510){
	  xx_tr=(zz+11000)*1.0/2.0+(xx-6240)*sqrt(3)/2.0; // The face of this detector is makes a 60 degree angle with the translated coordinate system z-axis.
          yy_tr=yy+1000;
      } else{
          xx_tr=xx;
	  yy_tr=yy;
      }

      if(det==5530  && ( (analyzeDet & 1) ==1))
	sphere.fillHisto(det, sp, rdDmg, spherePZ, xx, yy, sphereZZ, kinE);
      else if( doBeamLine && ( (analyzeDet & 2) ==2))
	beamLine.fillHisto(det, sp, rdDmg, pz, xx_tr, yy_tr, kinE);
      else if( (det==99 || det==101) && ((analyzeDet & 4) == 4) )
	hall.fillHisto(det,sp,rdDmg,xx,yy,zz,vx0,vy0,vz0,kinE);

      if((sp==0 || sp==5) && kinE>1){
	if(det==5530 && ( (analyzeDet & 1) ==1))
	  sphere.fillHisto(det, 1, rdDmg, spherePZ, xx, yy, sphereZZ, kinE);
	else if(doBeamLine  && ( (analyzeDet & 2) ==2))
	  beamLine.fillHisto(det, 1, rdDmg, pz, xx_tr, yy_tr, kinE);
	else if( (det==99 || det==101) && ((analyzeDet & 4) == 4) )
	  hall.fillHisto(det,1,rdDmg,xx,yy,zz,vx0,vy0,vz0,kinE);

	if((hit->at(j).trid==1 || hit->at(j).trid==2) && hit->at(j).mtrid==0){
	  if(det==5530 && ( (analyzeDet & 1) ==1))
	    sphere.fillHisto(det, 4, rdDmg, spherePZ, xx, yy, sphereZZ, kinE);
	  else if(doBeamLine  && ( (analyzeDet & 2) ==2))
	    beamLine.fillHisto(det, 4, rdDmg, pz, xx_tr, yy_tr, kinE);
	  else if( (det==99 || det==101) && ((analyzeDet & 4) == 4) )
	    hall.fillHisto(det,4,rdDmg,xx,yy,zz,vx0,vy0,vz0,kinE);

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
  if( (analyzeDet & 1) == 1)
    sphere.initHisto(fout,5530,"Tgt: sphere detector");
  if( (analyzeDet & 2) == 2){
    beamLine.initHisto(fout,5500,"Flat: beamline hall entrance");
    beamLine.initHisto(fout,5501,"Flat: beamline moller pol");
    beamLine.initHisto(fout,5547,"Flat: inside tgt bunker US Pb");
    beamLine.initHisto(fout,5546,"Flat: inside tgt bunker US tgt");
    beamLine.initHisto(fout,5543,"Flat: outside tgt bunker DS Pb");
    beamLine.initHisto(fout,5542,"Flat: outside tgt bunker US tgt");
    beamLine.initHisto(fout,5531,"Flat: inside tgt bunker DS Pb");
    beamLine.initHisto(fout,5560,"Flat: beamline US hybrid");

    
    beamLine.initHisto(fout,5556,"Flat: inside tgt bunker above tgt");//FIXME
    beamLine.initHisto(fout,5545,"Flat: inside tgt bunker right of tgt");//FIXME
    beamLine.initHisto(fout,5544,"Flat: inside tgt bunker left of tgt");//FIXME
    beamLine.initHisto(fout,5555,"Flat: outside tgt bunker above tgt");//FIXME
    beamLine.initHisto(fout,5541,"Flat: outside tgt bunker right of tgt");//FIXME
    beamLine.initHisto(fout,5540,"Flat: outside tgt bunker left of tgt");//FIXME
    beamLine.initHisto(fout,5510,"Flat: front SBS bunker");//FIXME

  }
  if( (analyzeDet & 4) == 4){
    hall.initHisto(fout);
  }
}

void writeOutput(double addScale){

  double scaleFactor = 1./nFiles;
  if(beamGen)
    scaleFactor = 1./nTotEv;

  scaleFactor /= addScale;

  if( (analyzeDet & 1) == 1)
    sphere.writeOutput(fout,5530,scaleFactor);
  if( (analyzeDet & 2) == 2){
    beamLine.writeOutput(fout,5500,scaleFactor);
    beamLine.writeOutput(fout,5501,scaleFactor);
    beamLine.writeOutput(fout,5547,scaleFactor);
    beamLine.writeOutput(fout,5546,scaleFactor);
    beamLine.writeOutput(fout,5543,scaleFactor);
    beamLine.writeOutput(fout,5542,scaleFactor);
    beamLine.writeOutput(fout,5531,scaleFactor);
    beamLine.writeOutput(fout,5560,scaleFactor);

    beamLine.writeOutput(fout,5556,scaleFactor);//FIXME
    beamLine.writeOutput(fout,5545,scaleFactor);//FIXME
    beamLine.writeOutput(fout,5544,scaleFactor);//FIXME
    beamLine.writeOutput(fout,5555,scaleFactor);//FIXME
    beamLine.writeOutput(fout,5541,scaleFactor);//FIXME
    beamLine.writeOutput(fout,5540,scaleFactor);//FIXME
    beamLine.writeOutput(fout,5510,scaleFactor);//FIXME
  }
  if( (analyzeDet & 4) == 4){
    hall.writeOutput(fout,scaleFactor);
  }

  fout->Close();
}
