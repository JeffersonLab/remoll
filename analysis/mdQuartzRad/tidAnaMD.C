// Run commands in this order:
// 
// //Start reroot
// >build/reroot
//
// //Load in the script, and run it
// >.L analysis/tidAna.C
// > tidAna(<remoll output file>)


TFile *fout;
TH2D *hXY,*hXYrate;//hits
TH2D *eXYtot,*eXYrate;//energy deposition
TH2D *rXYtot,*rXYrate;//radiation damage
long nTotEv(0);

double R5rate(0);
double R2rate(0);
//quartz 1cm thick

string fin;
const double pi = acos(-1);

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
  string foutNm = Form("%s_tidAnaMDV1.root",fin.substr(0,fin.find_last_of(".")).c_str());

  fout = new TFile(foutNm.c_str(),"RECREATE");

  eXYtot = new TH2D("eXYtot","hits * energy deposition [MeV]; x[mm]; y[mm]",
		    480,-1200,1200,
		    480,-1200,1200);

  eXYrate = new TH2D("eXYrate","rate * energy deposition [MeV]; x[mm]; y[mm]",
		     480,-1200,1200,
		     480,-1200,1200);

  hXY = new TH2D("hXY","hits; x[mm]; y[mm]",
		 480,-1200,1200,
		 480,-1200,1200);

  hXYrate = new TH2D("hXYrate","rate [Hz]; x[mm]; y[mm]",
		     480,-1200,1200,
		     480,-1200,1200);

  rXYtot = new TH2D("rXYtot","all part: radiation damage [rad]; x[mm]; y[mm]",
		     480,-1200,1200,
		     480,-1200,1200);

  rXYrate = new TH2D("rXYrate","e/pi:  radiation damage [rad]; x[mm]; y[mm]",
		     480,-1200,1200,
		     480,-1200,1200);

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
    //if(rate>1e8) continue;
    for(int j=0;j<hit->size();j++){

      if(hit->at(j).det != 2) continue;      
      double rr = hit->at(j).r;
      if( rr < 600 || rr > 1200) continue;
      if(std::isnan(hit->at(j).x) || std::isinf(hit->at(j).x)) continue;
      if(std::isnan(hit->at(j).y) || std::isinf(hit->at(j).y)) continue;

      //select only e- and pi-
      if(hit->at(j).pid==11 || hit->at(j).pid==-211){
	if( rr >920 && rr < 1060 && hit->at(j).e>1)
	  R5rate += rate;
	else if( rr> 680 && rr<740 && hit->at(j).e>1)
	  R2rate += rate;

	//cout<<event<<" "<<hit->at(j).x
	hXY->Fill(hit->at(j).x,hit->at(j).y);
	hXYrate->Fill(hit->at(j).x,hit->at(j).y,rate);

	eXYrate->Fill(hit->at(j).x,hit->at(j).y,rate*hit->at(j).edep);
      }
      eXYtot->Fill(hit->at(j).x,hit->at(j).y,rate*hit->at(j).edep);
    }
  }
  fin->Close();
  delete fin;
  return nEntries;
};
    

void writeOutput(){
  fout->cd();
  cout<<"R5 total rate "<<R5rate<<endl;
  cout<<"R2 total rate "<<R2rate<<endl;

  hXY->Write();

  hXYrate->Write();

  eXYtot->Write();

  eXYrate->Write();

  const double vol = 0.5*0.5*1; // cm3
  const double density = 2.32/1000;//kg/cm3
  const double MeV2J = 1.6e-13; 
  const double h2s = 3600;
  const double PAChours = 24 * (235 + 95 + 14);
  const double Gy2rad = 100;
  rXYtot->Add(eXYtot);
  rXYtot->Scale(MeV2J * PAChours * h2s /(vol*density)*Gy2rad);
  rXYtot->Write();

  rXYrate->Add(eXYrate);
  rXYrate->Scale(MeV2J * PAChours * h2s /(vol*density)*Gy2rad);
  rXYrate->Write();

  fout->Close();
}

  

