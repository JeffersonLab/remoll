// Run commands in this order:
// 
// //Start reroot
// >build/reroot
//
// //Load in the script, and run it
// >.L analysis/tidAna.C
// > tidAna(<remoll output file>)


TFile *fout;

const int nDt=3;
const int nWg=3;
const int nSp=5;
// [Det1 Det2 Det3][hits eDep TID][nSp]
TH2D *hXY[nWg][nDt][nSp];
const string wgTit[nWg]={"hits [#/whole run]","eDet[MeV/whole run]", "TID[rad/whole run]"};
const string spTit[nSp]={"o","ep","em","g","n"};

long nTotEv(0);
string fin;

void initHisto();
long processOne(string);
void process();
void writeOutput();

void tidAna(const string& finName = "./remollout.root"){
  fin = finName;
  initHisto();
  process();
  writeOutput();
}

void process(){

  if(fin==""){
    cout<<"\t did not find input file. Quitting!"<<endl;
    return 2;
  }

  int nFiles(0);
  if( fin.find(".root") < fin.size() ){
    cout<<"Processing single file:\n\t"<<fin<<endl;
    nTotEv+=processOne(fin);
    nFiles=1;
  }else{
    cout<<"Attempting to process list of output from\n\t"<<fin<<endl;
    ifstream ifile(fin.c_str());
    string data;
    while(ifile>>data){
      cout<<" processing: "<<data<<endl;
      nTotEv+=processOne(data);
      nFiles++;
    }
  }
  
  cout<<"\nFinished processing a total of "<<nTotEv<<endl;
}

void initHisto(){
  string foutNm = Form("%s_tidAnaV0.root",fin.substr(0,fin.find_last_of(".")).c_str());

  fout = new TFile(foutNm.c_str(),"RECREATE");

  for(int i=0;i<nDt;i++)
    for(int j=0;j<nWg;j++)
      for(int k=0;k<nSp;k++){
	hXY[i][j][k]=new TH2D(Form("hXY_dt%d_wg%d_%s",i,j,spTit[k].c_str()),
			      Form("det%d %s %s;x[mm];y[mm]",i,wgTit[j].c_str(),spTit[k].c_str()),
			      1000,-2500,2500,
			      1000,-2500,2500);			 
      }
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
  std::vector<remollGenericDetectorHit_t> *hit=0;
  t->SetBranchAddress("rate", &rate);
  t->SetBranchAddress("hit", &hit);
  
  long nEntries = t->GetEntries();
  float currentProc=1,procStep=60;
  vector<int> procID;
  int sector(-1);

  //for (Long64_t event = 0; event < 100; t->GetEntry(event++)) {
  for (Long64_t event = 0; event < nEntries; t->GetEntry(event++)) {
    if( float(event+1)/nEntries*100 > currentProc){
      cout<<"at tree entry\t"<<event<<"\t"<< float(event+1)/nEntries*100<<endl;
      currentProc+=procStep;
    }

    if(std::isnan(rate) || std::isinf(rate)) continue;

    for(int j=0;j<hit->size();j++){

      int nD = hit->at(j).det-1;
      if(nD<0 || nD>2) continue;      
      double rr = hit->at(j).r;
      if( rr < 100) continue;
      if(std::isnan(hit->at(j).x) || std::isinf(hit->at(j).x)) continue;
      if(std::isnan(hit->at(j).y) || std::isinf(hit->at(j).y)) continue;

      int sp=0;
      if(hit->at(j).pid==11) sp=1;
      else if(hit->at(j).pid==-11) sp=2;
      else if(hit->at(j).pid==22) sp=3;
      else if(hit->at(j).pid==2112) sp=4;

      if(hit->at(j).edep==0) continue;

      // cout<<nD<<" "<<sp<<" "<<hit->at(j).edep<<endl;
      // std::cin.ignore();

      hXY[nD][0][sp]->Fill(hit->at(j).x,hit->at(j).y,rate);
      hXY[nD][1][sp]->Fill(hit->at(j).x,hit->at(j).y,rate*hit->at(j).edep);

    }
  }
  fin->Close();
  delete fin;
  return nEntries;
};
    

void writeOutput(){
  fout->cd();

  const double vol = 0.5*0.5*0.1; // cm3
  //  const double density = 2.32/1000;//SiO2 kg/cm3
  //const double density = 2.33/1000;//Si kg/cm3
  const double density = 1.8/1000;//Viton kg/cm3
  const double MeV2J = 1.6e-13;
  const double nElecPerS = 60e-6/(1.6e-19);
  const double h2s = 3600;
  const double PAChours = 24 * (235 + 95 + 14);
  const double Gy2rad = 100;

  for(int i=0;i<nDt;i++)
    for(int k=0;k<nSp;k++){
      hXY[i][2][k]->Add(hXY[i][1][k]);
      hXY[i][2][k]->Scale(MeV2J * PAChours * h2s * nElecPerS / (vol*density) * Gy2rad);
      if(i==1)
	hXY[i][2][k]->Scale(1./10);//middle section 10mm long

      for(int j=0;j<nWg;j++){
	hXY[i][j][k]->Write();
      }
    }

  fout->Close();
}

  

