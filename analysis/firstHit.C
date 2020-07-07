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

//these are all for the hotspot
TH2F* dCoil_xy[nSpecies][nDmg];
TH2F* dCoil_rz[nSpecies][nDmg];

TH2F* dCoil_EinEdep[nSpecies];
TH1F* dCoil_Ein[nSpecies];
TH1F* dCoil_EinLin[nSpecies];

void initHisto(int);
void writeOutput();
long processOne(string);
void process();

void firstHit(const string& finName = "./remollout.root", int beamGenerator=1, int overWriteFile=1){
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

  std::vector<int> firstHit;
  std::vector<int> species;
  std::vector<double> powerDep,kinE;
  //for (Long64_t event = 0; event < 5; t->GetEntry(event++)) {
  for (Long64_t event = 0; event < nEntries; t->GetEntry(event++)) {
    currentEvNr++;
    if( float(event+1)/nEntries*100 > currentProc){
      cout<<"at tree entry\t"<<event<<"\t"<< float(event+1)/nEntries*100<<endl;
      currentProc+=procStep;
    }
    firstHit.clear();
    powerDep.clear();
    kinE.clear();
    species.clear();
    //pass find tracks that hit the coil
    for(int j=0;j<hit->size();j++){

      //if(hit->at(j).det <4001 || hit->at(j).det>4014) continue;
      if(hit->at(j).det != 4001 && hit->at(j).det != 4008) continue;

      if(std::isnan(rate) || std::isinf(rate)) continue;
      if(rate==0) {rate=1;}

      int sp = spM[int(hit->at(j).pid)]-1;
      if(sp==-1) continue;

      int trid = hit->at(j).trid;
      int foundUS(0);
      for(int k=0;k<hit->size() && foundUS==0;k++)
	if(trid == hit->at(k).trid)
	  foundUS++;
      if(!foundUS) continue;

      std::vector<int>::iterator it = find(trackNr.begin(),trackNr.end(),trid);
      if( it == trackNr.end() ){
	firstHit.push_back(trid);
	kinE.push_back(hit->at(j).k);	
	species.push_back(sp);

	dCoil_Ein[sp]->Fill(hit->at(j).k);
	dCoil_EinLin[sp]->Fill(hit->at(j).k);
	dCoil_xy[sp][0]->Fill(hit->at(j).x,hit->at(j).y);
	dCoil_xy[sp][1]->Fill(hit->at(j).x,hit->at(j).y,hit->at(j).k);
	dCoil_rz[sp][0]->Fill(hit->at(j).z,hit->at(j).r);
	dCoil_rz[sp][1]->Fill(hit->at(j).z,hit->at(j).r,hit->at(j).k);
      }else{
	int index = std::distance(trackNr.begin(),it);
	powerDep[index]+=hit->at(j).edep;
      }
    }

    for(int j=0;j<firstHit.size();j++){
      dCoil_EinEdep[species[j]]->Fill(kinE[j],powerDep[j]);
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
    dCoil_EinLin[i] = new TH1F(Form("dCoil_EinLin_%s",spH[i].c_str()),
				  Form("energy distribution %s",spH[i].c_str()),
				  200,0,1000);
    dCoil_Ein[i] = new TH1F(Form("dCoil_Ein_%s",spH[i].c_str()),
			       Form("energy distribution %s",spH[i].c_str()),
			       121,-8,4.1);
    niceLogXBins(dCoil_Ein[i]);

    dCoil_EinEdep[i] = new TH2F(Form("dCoil_EinEdep_%s",spH[i].c_str()),
				Form("%s;Ein [MeV]; Edep[MeV]",spH[i].c_str()),
				500,0,1000,
				500,0,1000);
    
    for(int j=0;j<nDmg;j++){
      dCoil_xy[i][j]= new TH2F(Form("dCoil_xy_%s_Dmg%d",spH[i].c_str(),j),
			       Form("%s for %s;x[mm];y[mm]",dmgTit[j].c_str(),spTit[i].c_str()),
			       800,-1300,1300,
			       800,-1300,1300);

      dCoil_rz[i][j]= new TH2F(Form("dCoil_rz_%s_Dmg%d",spH[i].c_str(),j),
			       Form("%s for %s;x[mm];y[mm]",dmgTit[j].c_str(),spTit[i].c_str()),
			       800,1100,3200,
			       800,0,300);
    }
  }
}

void writeOutput(){

  double scaleFactor = 1./nFiles;
  if(beamGen)
    scaleFactor = 1./nTotEv;

  fout->cd();
  for(int i=0;i<nSpecies;i++){
    dCoil_EinEdep[i]->Scale(scaleFactor);
    dCoil_EinEdep[i]->Write();
    dCoil_Ein[i]->Scale(scaleFactor);
    dCoil_Ein[i]->Write();
    dCoil_EinLin[i]->Scale(scaleFactor);
    dCoil_EinLin[i]->Write();

    for(int j=0;j<nDmg;j++){
      dCoil_xy[i]->Scale(scaleFactor);
      dCoil_xy[i]->Write();
      dCoil_rz[i]->Scale(scaleFactor);
      dCoil_rz[i]->Write();
    }
  }
  fout->Close();
}
