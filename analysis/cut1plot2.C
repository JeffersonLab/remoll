// Run commands in this order:
//
// //Start reroot
// >/path/to/build/reroot
//
// //Load in the script, and run it
// >.L cut1plot2.C
// > cut1plot2(<remoll output file>,
//            <1 for beam generator, 0 else>)

#include "histogramUtilities.h"
#include "anaConst.h"

TFile *fout;
string fileNm;
int beamGen(1);
long nTotEv(0);
int nFiles(0);
long currentEvNr(0);

TH2F* dBL_xy[nSpecies][nDmg];
TH2F* dBL_xyFold[nSpecies][nDmg];
TH2F* dBL_rzFold[nSpecies][nDmg];
TH1F* dBL_r[nSpecies][nDmg];
TH1F* dBL_energy[nSpecies];
TH1F* dBL_energyLin[nSpecies];
TH1F* dCoil_energy[nSpecies];
TH1F* dCoil_energyLin[nSpecies];
TH2F* dCoil_rz[nSpecies][nDmg];

TH2F *dBL_thE[nSpecies][nDmg];
TH2F *dBL_zE[nSpecies][nDmg];
TH2F *dBL_thZ[nSpecies][nDmg];

TH2F *dBL_phE[nSpecies][nDmg];
TH2F *dBL_phZ[nSpecies][nDmg];
TH2F *dBL_phZc2[nSpecies][nDmg];

TH1F *dBL_vZ[nSpecies][nDmg];
TH2F *dBL_vZE[nSpecies][nDmg];
TH2F *dBL_vRZ[nSpecies][nDmg];

void initHisto(int);
void writeOutput();
long processOne(string);
void process();

void cut1plot2(const string& finName = "./remollout.root", int beamGenerator=1, int overWriteFile=1){
  fileNm = finName;
  beamGen = beamGenerator;

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

  std::vector<int> trackNr;
  std::vector<double> zzAtCoil;
  //for (Long64_t event = 0; event < 5; t->GetEntry(event++)) {
  for (Long64_t event = 0; event < nEntries; t->GetEntry(event++)) {
    currentEvNr++;
    if( float(event+1)/nEntries*100 > currentProc){
      cout<<"at tree entry\t"<<event<<"\t"<< float(event+1)/nEntries*100<<endl;
      currentProc+=procStep;
    }
    trackNr.clear();
    zzAtCoil.clear();
    //pass find tracks that hit the coil
    for(int j=0;j<hit->size();j++){

      if(hit->at(j).det <4001 || hit->at(j).det>4014) continue;
      //if(hit->at(j).det != 4001 && hit->at(j).det != 4008) continue;

      if(std::isnan(rate) || std::isinf(rate)) continue;
      if(rate==0) {rate=1;}

      int sp = spM[int(hit->at(j).pid)]-1;
      if(sp==-1) continue;

      double kinE = hit->at(j).k;
      double rr=hit->at(j).r;
      double rdDmg[3]={rate,rate*kinE,0};
      double zz = hit->at(j).z;
      
      //if( rr > 90 ) continue;
      if( rr > 90 || zz<1500 || zz>2500) continue;
      dCoil_energy[sp]->Fill(kinE);
      dCoil_energyLin[sp]->Fill(kinE);
      for(int kk=0;kk<3;kk++){
	dCoil_rz[sp][kk]->Fill(zz,rr,rdDmg[kk]);
      }

      if( find(trackNr.begin(),trackNr.end(),hit->at(j).trid) == trackNr.end() ){ 
	trackNr.push_back(hit->at(j).trid);
	zzAtCoil.push_back(zz);
      }
    }

    for(int j=0;j<hit->size();j++){
      if(hit->at(j).det != 26) continue;

      std::vector<int>::iterator it = find(trackNr.begin(),trackNr.end(),hit->at(j).trid);
      if( it == trackNr.end() ) continue;
      int index = std::distance(trackNr.begin(),it);

      if(std::isnan(rate) || std::isinf(rate)) continue;
      if(rate==0) {rate=1;}

      int sp = spM[int(hit->at(j).pid)]-1;
      if(sp==-1) continue;

      double kinE = hit->at(j).k;
      double rr=hit->at(j).r;

      double rdDmg[3]={rate,rate*kinE,0};
      double xx = hit->at(j).x;
      double yy = hit->at(j).y;
      double th = atan2(sqrt(hit->at(j).px*hit->at(j).px + hit->at(j).py*hit->at(j).py),hit->at(j).pz);
      double ph = atan2(hit->at(j).py,hit->at(j).px);

      if(hit->at(j).pz<0) continue;

      if(kinE<30) continue;
      dBL_energy[sp]->Fill(kinE);
      dBL_energyLin[sp]->Fill(kinE);
      for(int kk=0;kk<3;kk++){
	double phi = atan2(yy,xx);
	double secPhi = fmod(phi,2*pi/7);
	double xr = rr*cos(secPhi);
	double yr = rr*sin(secPhi);
	dBL_xyFold[sp][kk]->Fill(xr,yr,rdDmg[kk]);

	dBL_xy[sp][kk]->Fill(xx,yy,rdDmg[kk]);
	dBL_r[sp][kk]->Fill(rr,rdDmg[kk]);
	dBL_thE[sp][kk]->Fill(th,kinE/1000,rdDmg[kk]);
	dBL_phE[sp][kk]->Fill(ph,kinE/1000,rdDmg[kk]);
	dBL_thZ[sp][kk]->Fill(zzAtCoil[index],th,rdDmg[kk]);
	dBL_phZ[sp][kk]->Fill(zzAtCoil[index],ph,rdDmg[kk]);
	dBL_phZc2[sp][kk]->Fill(hit->at(j).vz,ph,rdDmg[kk]);
	dBL_zE[sp][kk]->Fill(zzAtCoil[index],kinE/1000,rdDmg[kk]);
	dBL_rzFold[sp][kk]->Fill(zzAtCoil[index],rr,rdDmg[kk]);
 	dBL_vZ[sp][kk]->Fill(hit->at(j).vz,rdDmg[kk]);
	dBL_vRZ[sp][kk]->Fill(hit->at(j).vz,sqrt(hit->at(j).vx*hit->at(j).vx+hit->at(j).vy*hit->at(j).vy),rdDmg[kk]);
 	dBL_vZE[sp][kk]->Fill(hit->at(j).vz,kinE,rdDmg[kk]);
      }

    }
    
  }

  fin->Close();
  delete fin;
  return nEntries;
}


void initHisto(int fileType){
  string foutNm = Form("%s_Elarger30_c1p2V7.root",fileNm.substr(0,fileNm.find_last_of(".")).c_str());

  const string fTp[2]={"UPDATE","RECREATE"};
  cout<<"Will "<<fTp[fileType]<<" file!"<<endl;
  fout = new TFile(foutNm.c_str(),fTp[fileType].c_str());

  for(int i=0;i<nSpecies;i++){
    dBL_energyLin[i] = new TH1F(Form("aC2_energyLin_%s",spH[i].c_str()),
				Form("energy distribution %s",spH[i].c_str()),
				200,0,1000);
    dBL_energy[i] = new TH1F(Form("aC2_energy_%s",spH[i].c_str()),
			     Form("energy distribution %s",spH[i].c_str()),
			     121,-8,4.1);
    niceLogXBins(dBL_energy[i]);

    dCoil_energyLin[i] = new TH1F(Form("aCoil_energyLin_%s",spH[i].c_str()),
				  Form("energy distribution %s",spH[i].c_str()),
				  200,0,1000);
    dCoil_energy[i] = new TH1F(Form("aCoil_energy_%s",spH[i].c_str()),
			       Form("energy distribution %s",spH[i].c_str()),
			       121,-8,4.1);
    niceLogXBins(dCoil_energy[i]);
    
    for(int j=0;j<nDmg;j++){
      dBL_xy[i][j]= new TH2F(Form("aC2_xy_%s_Dmg%d",spH[i].c_str(),j),
			     Form("%s for %s;x[mm];y[mm]",dmgTit[j].c_str(),spTit[i].c_str()),
			     800,-1300,1300,
			     800,-1300,1300);
      dBL_xyFold[i][j]= new TH2F(Form("aC2_xyFold_%s_Dmg%d",spH[i].c_str(),j),
				 Form("%s for %s;x[mm];y[mm]",dmgTit[j].c_str(),spTit[i].c_str()),
				 800,-100,0,
				 800,-100,100);

      dBL_r[i][j] = new TH1F(Form("aC2_r_%s_Dmg%d",spH[i].c_str(),j),
			     Form("%s for %s;x[mm];y[mm]",dmgTit[j].c_str(),spTit[i].c_str()),
			     800,0,1300);
      dCoil_rz[i][j] = new  TH2F(Form("dCoil_xy_%s_Dmg%d",spH[i].c_str(),j),
				 Form("%s for %s;z[mm];r[mm]",dmgTit[j].c_str(),spTit[i].c_str()),
				 800,0,3200,
				 800,0,300);

      dBL_rzFold[i][j] = new  TH2F(Form("dBL_rzFold_%s_Dmg%d",spH[i].c_str(),j),
				   Form("%s for %s;z at coil [mm];r at C2 [mm]",dmgTit[j].c_str(),spTit[i].c_str()),
				   800,1100,3200,
				   800,0,300);

      dBL_thE[i][j]= new TH2F(Form("aC2_thE_%s_Dmg%d",spH[i].c_str(),j),
			      Form("%s for %s;theta [rad];E[GeV]",dmgTit[j].c_str(),spTit[i].c_str()),
			      800,0,0.6,
			      800,0,0.5);
      dBL_phE[i][j]= new TH2F(Form("aC2_phE_%s_Dmg%d",spH[i].c_str(),j),
			      Form("%s for %s;phi [rad];E[GeV]",dmgTit[j].c_str(),spTit[i].c_str()),
			      800,-3.2,3.2,
			      800,0,0.5);

      dBL_thZ[i][j]= new TH2F(Form("aC2_thZ_%s_Dmg%d",spH[i].c_str(),j),
			      Form("%s for %s;z at coil [mm];theta after Coll2 [rad]",dmgTit[j].c_str(),spTit[i].c_str()),
			      800,500,3200,
			      800,0,0.6);
      dBL_phZ[i][j]= new TH2F(Form("aC2_phZ_%s_Dmg%d",spH[i].c_str(),j),
			      Form("%s for %s;z at coil [mm];phi after Coll2 [rad]",dmgTit[j].c_str(),spTit[i].c_str()),
			      800,500,3200,
			      800,-3.2,3.2);
      dBL_phZc2[i][j]= new TH2F(Form("aC2_phZc2_%s_Dmg%d",spH[i].c_str(),j),
			      Form("%s for %s;vz at c2 [mm];phi after Coll2 [rad]",dmgTit[j].c_str(),spTit[i].c_str()),
			      600,700,1000,
			      800,-3.2,3.2);

      dBL_zE[i][j]= new TH2F(Form("aC2_zE_%s_Dmg%d",spH[i].c_str(),j),
			      Form("%s for %s;z at coil [mm];kinE after Coll2 [GeV]",dmgTit[j].c_str(),spTit[i].c_str()),
			      800,500,3200,
			      800,0,0.5);

      dBL_vZ[i][j] = new TH1F(Form("aC2_vZ_%s_Dmg%d",spH[i].c_str(),j),
			      Form("%s for %s;vZ[mm]",dmgTit[j].c_str(),spTit[i].c_str()),
			      2000,-6000,1000);
      dBL_vZE[i][j] = new TH2F(Form("aC2_vZE_%s_Dmg%d",spH[i].c_str(),j),
			       Form("%s for %s;vZ[mm];kinE [MeV]",dmgTit[j].c_str(),spTit[i].c_str()),
			       2000,-6000,1000,
			       800,0,500);
      dBL_vRZ[i][j] = new TH2F(Form("aC2_vRZ_%s_Dmg%d",spH[i].c_str(),j),
			       Form("%s for %s;vZ[mm];vR[mm]",dmgTit[j].c_str(),spTit[i].c_str()),
			       800,-6000,1000,
			       800,-6000,1000);

    }
  }
}

void writeOutput(){

  double scaleFactor = 1./nFiles;
  if(beamGen)
    scaleFactor = 1./nTotEv;

  fout->cd();
  for(int i=0;i<nSpecies;i++){
    dBL_energy[i]->Scale(scaleFactor);
    dBL_energy[i]->Write();
    dBL_energyLin[i]->Scale(scaleFactor);
    dBL_energyLin[i]->Write();
    dCoil_energy[i]->Scale(scaleFactor);
    dCoil_energy[i]->Write();
    dCoil_energyLin[i]->Scale(scaleFactor);
    dCoil_energyLin[i]->Write();
      
    for(int j=0;j<nDmg;j++){
      dBL_xy[i][j]->Scale(scaleFactor);
      dBL_xy[i][j]->Write();
      dBL_xyFold[i][j]->Scale(scaleFactor);
      dBL_xyFold[i][j]->Write();
      dBL_r[i][j]->Scale(scaleFactor);
      dBL_r[i][j]->Write();
      dCoil_rz[i][j]->Scale(scaleFactor);
      dCoil_rz[i][j]->Write();
      dBL_rzFold[i][j]->Scale(scaleFactor);
      dBL_rzFold[i][j]->Write();
      dBL_thE[i][j]->Scale(scaleFactor);
      dBL_thE[i][j]->Write();
      dBL_thZ[i][j]->Scale(scaleFactor);
      dBL_thZ[i][j]->Write();
      dBL_phE[i][j]->Scale(scaleFactor);
      dBL_phE[i][j]->Write();
      dBL_phZ[i][j]->Scale(scaleFactor);
      dBL_phZ[i][j]->Write();
      dBL_phZc2[i][j]->Scale(scaleFactor);
      dBL_phZc2[i][j]->Write();
      dBL_zE[i][j]->Scale(scaleFactor);
      dBL_zE[i][j]->Write();
      dBL_vZ[i][j]->Scale(scaleFactor);
      dBL_vZ[i][j]->Write();
      dBL_vZE[i][j]->Scale(scaleFactor);
      dBL_vZE[i][j]->Write();
      dBL_vRZ[i][j]->Scale(scaleFactor);
      dBL_vRZ[i][j]->Write();

    }
  }
  fout->Close();
}
