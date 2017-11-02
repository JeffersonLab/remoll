#include <iostream>
#include <vector>
#include <algorithm>
#include <string>
#include <ctime>
#include <fstream>
#include <cmath>

#include "TFile.h"
#include "TTree.h"
#include "TH1D.h"

using namespace std;
clock_t tStart;

void findDetector(vector<int> &ring, int &sector, double phi, double r);
long processOne(string fnm);
void writeOutput();
void initOutput(string fnm);
TFile *fout;
vector<vector<TH1D*>> hAsym;
TH1D *hSums;

const double pi=acos(-1);

int main(int argc, char **argv){
  tStart = clock();

  if(argc == 1 || (strcmp("--help",argv[1])==0) ){
    cout<<"usage: build/bkgAna [options] "<<endl
	<<"\t--infile <path to rootfile or file with a list to rootfiles>\n";
    return 1;
  }

  string fin="";
  for(int i=1;i<argc;i++){
    if(strcmp(argv[i],"--infile")==0)
      fin=argv[i+1];
  }

  if(fin==""){
    cout<<"\t did not find input file. Quitting!"<<endl;
    return 2;
  }
  initOutput(fin);

  int nTotEv(0);
  if( fin.find(".root") < fin.size() ){
    cout<<"Processing single file:\n\t"<<fin<<endl;
    nTotEv+=processOne(fin);
  }else{
    cout<<"Attempting to process list of output from\n\t"<<fin<<endl;
    ifstream ifile(fin.c_str());
    string data;
    while(ifile>>data){
      cout<<" processing: "<<data<<endl;
      nTotEv+=processOne(data);
    }
  }

  cout<<"\nFinished processing a total of "<<nTotEv<<endl;
  writeOutput();
  return 0;
}

long processOne(string fnm){
  TFile *fin=new TFile(fnm.c_str(),"READ");
  if(!fin->IsOpen()){
    cout<<"Problem: can't find file: "<<fnm<<endl;
    fin->Close();
    delete fin;
    return 0;
  }

  TTree *t=(TTree*)fin->Get("T");
  double ev_A=0;
  double ev_xs=0; //ub
  double rate=0;  //weighting factor
  int hit_n=0;
  int hit_det[10000]={0};
  int hit_pid[10000]={0};
  int hit_trid[10000]={0};
  int hit_mtrid[10000]={0};
  double hit_x[10000]={0};
  double hit_y[10000]={0};
  double hit_z[10000]={0};
  double hit_r[10000]={0};
  double hit_p[10000]={0};
  double hit_px[10000]={0};
  double hit_py[10000]={0};
  double hit_pz[10000]={0};
  double hit_vx[10000]={0};
  double hit_vy[10000]={0};
  double hit_vz[10000]={0};
  double hit_e[10000]={0};
  double hit_m[10000]={0};

  t->SetBranchAddress("ev.A",&ev_A);
  t->SetBranchAddress("ev.xs",&ev_xs);
  t->SetBranchAddress("hit.n",&hit_n);
  t->SetBranchAddress("rate",&rate);
  t->SetBranchAddress("hit.det",hit_det);
  t->SetBranchAddress("hit.pid",hit_pid);
  t->SetBranchAddress("hit.trid",hit_trid);
  t->SetBranchAddress("hit.mtrid",hit_mtrid);
  t->SetBranchAddress("hit.x",hit_x);
  t->SetBranchAddress("hit.y",hit_y);
  t->SetBranchAddress("hit.z",hit_z);
  t->SetBranchAddress("hit.r",hit_r);
  t->SetBranchAddress("hit.p",hit_p);
  t->SetBranchAddress("hit.px",hit_px);
  t->SetBranchAddress("hit.py",hit_py);
  t->SetBranchAddress("hit.pz",hit_pz);
  t->SetBranchAddress("hit.vx",hit_vx);
  t->SetBranchAddress("hit.vy",hit_vy);
  t->SetBranchAddress("hit.vz",hit_vz);
  t->SetBranchAddress("hit.e",hit_e);
  t->SetBranchAddress("hit.m",hit_m);

  long nEntries = t->GetEntries();
  cout<<"\tTotal events: "<<nEntries<<endl;
  float currentProc=1,procStep=10;
  vector<int> procID,ringHit;
  int sector;
  for(long i=0;i<nEntries;i++){
    t->GetEntry(i);
    if( float(i+1)/nEntries*100 > currentProc){
      cout<<"at tree entry\t"<<i<<"\t"<< float(i+1)/nEntries*100<<" %\ttime:\t"
	  <<(double) ((clock() - tStart)/CLOCKS_PER_SEC)<<" [s]"<<endl;
      currentProc+=procStep;

    }

    procID.clear();
    for(int j=0;j<hit_n;j++){
      if(hit_z[j] <= 26) continue;

      //select only e- and pi-
      if(hit_pid[j]!=11 && hit_pid[j]!=-211) continue;

      //make sure this is the detector you want
      if(hit_det[j] != 28) continue;

      //this will ensure that we process only one hit from a particular track per event
      if( find(procID.begin(),procID.end(), hit_trid[j]) != procID.end() ) continue;
      procID.push_back(hit_trid[j]);

      findDetector(ringHit, sector, atan2(hit_y[j],hit_x[j]), hit_r[j]);
      for(unsigned int k=0;k<ringHit.size();k++){
	if(ringHit[k]!=-1){
	  hAsym[ringHit[k]][sector]->Fill(ev_A,rate);
	  hSums->SetBinContent(ringHit[k]*3+sector,
			       rate + hSums->GetBinContent(ringHit[k]*3+sector));
	}
      }
    }
  }
  return nEntries;
}

void initOutput(string fnm){
  fout = new TFile(Form("%s_bkgAna.root",fnm.substr(0,fnm.find(".")).c_str()),"RECREATE");

  hSums = new TH1D("hSums","Sums for all rings and sectors",18,0,18);
  const string secNm[3]={"closed","transition","open"};
  for(int i=1;i<=18;i++)
    hSums->GetXaxis()->SetBinLabel(i,Form("R%d %s",(i-1-(i-1)%3)/3+1,secNm[(i-1)%3].c_str()));

  for(int i=0;i<6;i++){
    vector<TH1D*> dt;
    for(int j=0;j<3;j++){
      TH1D *h = new TH1D(Form("hAsym_R%d_S%d",i+1,j),
			 Form("rate weighted Asyms for Ring %d Sector %s;asymmetry [ppb]",i+1,secNm[j].c_str()),
			 100,-1000000,1000000);
      dt.push_back(h);
    }
    hAsym.push_back(dt);
  }
}

void writeOutput(){
  fout->cd();
  for(int i=0;i<6;i++)
    for(int j=0;j<3;j++)
      hAsym[i][j]->Write();
  hSums->Write();
  fout->Close();
}


void findDetector(vector<int> &ring, int &sector, double phi, double r){
  const double rMin[8]={0.690, 0.730, 0.780, 0.855, 0.935, 0.960, 0.960, 1.100};
  const double rMax[8]={0.730, 0.780, 0.855, 0.930, 1.040, 1.075, 1.100, 1.200};
  const int region2ring[8]={0,1,2,3,4,4,4,5};

  ring.clear();
  int found=0;
  for(int i=0;i<8;i++)
    if( r >= rMin[i] && r <= rMax[i]){
      ring.push_back(region2ring[i]);
      found++;
    }else
      ring.push_back(-1);

  if(!found) return;

  if( phi < 0 ) phi += 2*pi;
  const double secPhi = fmod(phi,2*pi/7);

  //0,1,2 == closed, transition, open
  if( secPhi < pi/28 )
    sector = 0;
  else if( secPhi < 3*pi/28 )
    sector = 1;
  else if( secPhi < 5*pi/28 )
    sector = 2;
  else if( secPhi < 7*pi/28 )
    sector = 1;
  else if( secPhi < 8*pi/28 )
    sector = 0;
}
