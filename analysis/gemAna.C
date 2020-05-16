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
// >.L gemAna.C
// > genAna(<remoll output file>,<1 for beam generator, 0 else>)

#include "radDamage.hh"
#include "histogramUtilities.h"
#include "anaConst.h"

TFile *fout;
string fileNm;
int beamGen(1);
long nTotEv(0);
int nFiles(0);
long currentEvNr(0);
int detUT = 50;
radDamage radDmg;

TH1F *gemdet_energy[nSpecies], *gemdet_energyNIEL[nSpecies];

TH1F *gemdet_z0[nSpecies][nDmg];
TH1F *gemdet_z0HE[nSpecies][nDmg];

TH2F *gemdet_xy[nSpecies][nDmg];
TH2F *gemdet_z0r0[nSpecies][nDmg];
TH2F *gemdet_z0x0[nSpecies][nDmg];
TH2F *gemdet_rph[nSpecies];
TH2F *gemdet_zph[nSpecies];

//source plots for different z cuts
TH2F *gemdet_x0y0Zcut[nSpecies][nZcut];

void fillHisto_gemdet(int sp, double rdDmg[3],
		     double xx, double yy, double zz, double vx0,
		     double vy0, double vz0, double rr, 
		     double kinE, int sector);
void writeOutput_gemdet(TFile *fout, double scaleFactor);

void initHisto();
void writeOutput();
long processOne(string);
void process();

void gemAna(const string& finName = "./remollout.root", int beamGenerator=1){
  fileNm = finName;
  beamGen = beamGenerator;

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
  t->SetBranchAddress("rate", &rate);
  t->SetBranchAddress("bm", &bm);
  t->SetBranchAddress("ev", &ev);
  t->SetBranchAddress("part", &part);
  t->SetBranchAddress("hit", &hit);

  long nEntries = t->GetEntries();
  cout<<"\tTotal events: "<<nEntries<<endl;
  float currentProc=1,procStep=30;
  vector<int> procID;
  int sector(-1);


  for (Long64_t event = 0; event < nEntries/1.; t->GetEntry(event++)) {
  //for (Long64_t event = 0; event < 100.; t->GetEntry(event++)) {
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
      

      if(det==detUT)
	fillHisto_gemdet(sp, rdDmg, xx, yy, zz, vx0, vy0, vz0,rr,kinE,sector);

      if((sp==0 || sp==5) && kinE>1){
	if(det==detUT)
	  fillHisto_gemdet(1, rdDmg, xx, yy, zz, vx0, vy0, vz0,rr,kinE,sector);

	if(hit->at(j).trid==1 || hit->at(j).trid==2){
	  if(det==detUT)
	    fillHisto_gemdet(4, rdDmg, xx, yy, zz, vx0, vy0, vz0,rr,kinE,sector);
	}
      }

    }
  }

  fin->Close();
  delete fin;
  return nEntries;
};


void initHisto(){
  string foutNm = Form("%s_gemana.root",fileNm.substr(0,fileNm.find_last_of(".")).c_str());

  fout = new TFile(foutNm.c_str(),"RECREATE");
  fout->mkdir("gemdet","GEM detector");

  for(int i=0;i<nSpecies;i++){
    for(int k=0;k<nZcut;k++){
      gemdet_x0y0Zcut[i][k]=new TH2F(Form("gemdet_x0y0Zcut_%s_ZC%d",spH[i].c_str(),k),
				  Form("hits per electron for %s %s;x0[mm];y0[mm]",
				       spTit[i].c_str(),zCutTit[k].c_str()),
				  400,-1470,1470,
				  400,-1470,1470);
    }


      gemdet_energy[i]=new TH1F(Form("gemdet_energy_%s",spH[i].c_str()),
				Form("rate weighted for %s;E [MeV]",spTit[i].c_str()),
				121,-8,4.1);
      niceLogXBins(gemdet_energy[i]);

      gemdet_energyNIEL[i]=new TH1F(Form("gemdet_energyNEIL_%s",spH[i].c_str()),
				    Form("rate weighted for %s;E [MeV]",spTit[i].c_str()),
				    121,-8,4.1);
      niceLogXBins(gemdet_energyNIEL[i]);
	
      gemdet_rph[i]=new TH2F(Form("gemdet_rph_%s",spH[i].c_str()),
                    Form("Rate*NEIL weighted %s;ph[deg]",spTit[i].c_str()),380,-190,190,300,1440,1470);

      gemdet_zph[i]=new TH2F(Form("gemdet_zph_%s",spH[i].c_str()),
                    Form("Rate*NEIL weighted %s;ph[deg]",spTit[i].c_str()),380,-190,190,310,18825,21925);

      for(int k=0;k<nDmg;k++){
	gemdet_z0[i][k]=new TH1F(Form("gemdet_z0_%s_Dmg%d",spH[i].c_str(),k),
				 Form("%s weighted %s;z0[mm]",dmgTit[k].c_str(),spTit[i].c_str()),
				 2550,-6000,45000);
      
	gemdet_z0HE[i][k]=new TH1F(Form("gemdet_z0HE_%s_Dmg%d",spH[i].c_str(),k),
				   Form("%s weighted %s;z0HE[mm]",dmgTit[k].c_str(),spTit[i].c_str()),
				   2550,-6000,45000);
      

	gemdet_xy[i][k]=new TH2F(Form("gemdet_xy_%s_Dmg%d",spH[i].c_str(),k),
				 Form("%s for %s;x[mm];y[mm]",dmgTit[k].c_str(),spTit[i].c_str()),
				 800,-1470,1470,
				 800,-1470,1470);
      
      
	gemdet_z0r0[i][k]=new TH2F(Form("gemdet_z0r0_%s_Dmg%d",spH[i].c_str(),k),
				   Form("%s for %s;z0[mm];r0[mm]",dmgTit[k].c_str(),spTit[i].c_str()),
				   2550,-6000,45000,
				   200,1440,1470);

	gemdet_z0x0[i][k]=new TH2F(Form("gemdet_z0x0_%s_Dmg%d",spH[i].c_str(),k),
				   Form("%s for %s;z0[mm];x0[mm]",dmgTit[k].c_str(),spTit[i].c_str()),
				   2550,-6000,45000,
				   200,-1470,1470);
      }
  }
}



void writeOutput(){

  double scaleFactor = 1./nFiles;
  if(beamGen)
    scaleFactor = 1./nTotEv;

  writeOutput_gemdet(fout,scaleFactor);

  fout->Close();
}


void fillHisto_gemdet(int sp, double rdDmg[3],
		     double xx, double yy, double zz, double vx0,
		     double vy0, double vz0, double rr, 
		     double kinE, int sector){
  double phi = (180./pi)*atan2(yy,xx);
  //phi = phi*180./pi;
  double vr0=sqrt(vx0*vx0+vy0*vy0);
  for(int kk=0;kk<nDmg;kk++){
    gemdet_z0[sp][kk]->Fill(vz0,rdDmg[kk]);
    if(kinE>10)
      gemdet_z0HE[sp][kk]->Fill(vz0,rdDmg[kk]);
    
    gemdet_xy[sp][kk]->Fill(xx,yy,rdDmg[kk]);
    gemdet_z0r0[sp][kk]->Fill(vz0,vr0,rdDmg[kk]);
    gemdet_z0x0[sp][kk]->Fill(vz0,vx0,rdDmg[kk]);
    
  }

  gemdet_rph[sp]->Fill(phi,rr,rdDmg[2]);
  gemdet_zph[sp]->Fill(phi,zz,rdDmg[2]);
  gemdet_energy[sp]->Fill(kinE,rdDmg[0]);
  gemdet_energyNIEL[sp]->Fill(kinE,rdDmg[2]);

    for(int ii=0;ii<nZcut;ii++)
      if(vz0>zCuts[ii][0] && vz0<zCuts[ii][1]){
	gemdet_x0y0Zcut[sp][ii]->Fill(vx0,vy0,rdDmg[0]);
      }
}

void writeOutput_gemdet(TFile *fout, double scaleFactor){
  fout->cd("gemdet");
  for(int i=0;i<nSpecies;i++){
    for(int k=0;k<nZcut;k++){
      gemdet_x0y0Zcut[i][k]->Scale(scaleFactor);
      gemdet_x0y0Zcut[i][k]->Write();
    }

    
      gemdet_energy[i]->Scale(scaleFactor);
      gemdet_energy[i]->Write();

      gemdet_energyNIEL[i]->Scale(scaleFactor);
      gemdet_energyNIEL[i]->Write();
    
      gemdet_rph[i]->Scale(scaleFactor);
      gemdet_rph[i]->Write();
    
      gemdet_zph[i]->Scale(scaleFactor);
      gemdet_zph[i]->Write();

      for(int k=0;k<nDmg;k++){
	gemdet_z0[i][k]->Scale(scaleFactor);
	gemdet_z0[i][k]->Write();

	gemdet_z0HE[i][k]->Scale(scaleFactor);
	gemdet_z0HE[i][k]->Write();
      

	gemdet_xy[i][k]->Scale(scaleFactor);
	gemdet_xy[i][k]->Write();
      
      
	gemdet_z0r0[i][k]->Scale(scaleFactor);
	gemdet_z0r0[i][k]->Write();

	gemdet_z0x0[i][k]->Scale(scaleFactor);
	gemdet_z0x0[i][k]->Write();
      }
  }
}
