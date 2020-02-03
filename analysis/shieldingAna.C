// Run commands in this order:
//
// //Start reroot
// >build/reroot
//
// //Load in the script, and run it
// >.L analysis/shieldingAna.C
// > shieldingAna(<remoll output file>)


TFile *fout;

const int nSp=5;// [e/pi,e/pi E>1,gamma,neutron]
const string spTit[nSp]={"e/#pi","e/#pi E>1","#gamma","neutron","primary e E>1"};
const string spH[nSp]={"e","e1","g","n","eP1"};
map<int,int> spM {{11,1},{211,1},{22,3},{2112,4}};

const int nDet=3; //[26,27,28]
const string detH[nDet]={"det26","det27","det28"};
map<int,int> dtM {{26,1},{27,2},{28,3}};

TH1D *hRate[nSp];
TH1D *eRate[nSp][nDet];
TH1D *drate[nSp][nDet], *drRate[nSp][nDet], *drRateAsym[nSp][nDet], *dZ0[nSp][nDet];
TH1D *drRatePZL0[nSp][nDet],*drRatePZG0[nSp][nDet];
TH2D *dXY[nSp][nDet], *dXYrate[nSp][nDet], *dXYrateE[nSp][nDet];
TH2D *dZ0R0[nSp][nDet],*dZ0X0[nSp][nDet];

const int nSec=3; //  //0,1,2 == closed, transition, open
const string secH[nSec]={"closed","trans","open"};
TH1D *drRateS[nSp][nSec], *drRateAsymS[nSp][nSec];

string fin;
int nFiles(0);
long currentEvNr(0);

const double pi = acos(-1);

//log X axis histograms
void niceLogBins(TH1*);

void initHisto();
long processOne(string);
void process();
int findDetector(int &sector, double phi, double r);
void writeOutput();

void shieldingAna(const string& finName = "./remollout.root"){
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

  long nTotEv(0);
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
  string foutNm = Form("%s_shldAnaV4.root",fin.substr(0,fin.find(".")).c_str());

  fout = new TFile(foutNm.c_str(),"RECREATE");

  for(int j=0;j<nDet;j++){
    fout->mkdir(detH[j].c_str(),Form("%s plane",detH[j].c_str()));
    fout->cd(detH[j].c_str());
    for(int i=0;i<nSp;i++){

      if(j==2){
	hRate[i] = new TH1D(Form("hRate_%s",spH[i].c_str()),Form("Sums for all rings and sectors %s",spTit[i].c_str()),21,0,21);
	const string secNm[3]={"closed","transition","open"};
	for(int k=1;k<=21;k++)
	  hRate[i]->GetXaxis()->SetBinLabel(k,Form("R%d %s",(k-1-(k-1)%3)/3+1,secNm[(k-1)%3].c_str()));
	for(int k=0;k<nSec;k++){
	  drRateS[i][k]=new TH1D(Form("det28_rRate_S%d_%s",k,spH[i].c_str()),
				 Form("rate weighted %s for %s;r[mm]",
				      spTit[i].c_str(),secH[k].c_str()),
				 600,600,1200);
	  drRateAsymS[i][k]=new TH1D(Form("det28_rRateAsym_S%d_%s",k,spH[i].c_str()),
				     Form("rate*asym weighted %s for %s;r[mm]",
					  spTit[i].c_str(),secH[k].c_str()),
				     600,600,1200);
	  
	}
      }

      eRate[i][j]=new TH1D(Form("%seRate_%s",detH[j].c_str(),spH[i].c_str()),
			   Form("rate weighted for %s;E [MeV]",spTit[i].c_str()),
			   121,-8,4.1);
      niceLogBins(eRate[i][j]);
      
      drate[i][j]=new TH1D(Form("%srate_%s",detH[j].c_str(),spH[i].c_str()),
			   Form("log10 of rate for %s",spTit[i].c_str()),
			   100,0,20);

      drRate[i][j]=new TH1D(Form("%srRate_%s",detH[j].c_str(),spH[i].c_str()),
			    Form("rate weighted %s;r[mm]",spTit[i].c_str()),
			    500,0,2000);
      drRatePZL0[i][j]=new TH1D(Form("%srRatePZL0_%s",detH[j].c_str(),spH[i].c_str()),
				Form("rate weighted pz<=0 %s;r[mm]",spTit[i].c_str()),
				500,0,2000);
      drRatePZG0[i][j]=new TH1D(Form("%srRatePZG0_%s",detH[j].c_str(),spH[i].c_str()),
				Form("rate weighted pz>0 %s;r[mm]",spTit[i].c_str()),
				500,0,2000);
      drRateAsym[i][j]=new TH1D(Form("%srRateAsym_%s",detH[j].c_str(),spH[i].c_str()),
				Form("rate*Asym weighted %s;r[mm]",spTit[i].c_str()),
				500,0,2000);

      dZ0[i][j]=new TH1D(Form("%sZ0_%s",detH[j].c_str(),spH[i].c_str()),
			 Form("rate weighted %s;z0[mm]",spTit[i].c_str()),
			 2000,-2000,32000);

      dXY[i][j]=new TH2D(Form("%sXY_%s",detH[j].c_str(),spH[i].c_str()),
			 Form("hits for %s;x[mm];y[mm]",spTit[i].c_str()),
			 200,-2000,2000,
			 200,-2000,2000);
      dXYrate[i][j]=new TH2D(Form("%sXYrate_%s",detH[j].c_str(),spH[i].c_str()),
			     Form("hits*rate for %s;x[mm];y[mm]",spTit[i].c_str()),
			     200,-2000,2000,
			     200,-2000,2000);
      dXYrateE[i][j]=new TH2D(Form("%sXYrateE_%s",detH[j].c_str(),spH[i].c_str()),
			      Form("hits*rate*E for %s;x[mm];y[mm]",spTit[i].c_str()),
			      200,-2000,2000,
			      200,-2000,2000);

      dZ0R0[i][j]=new TH2D(Form("%sZ0R0_%s",detH[j].c_str(),spH[i].c_str()),
			   Form("hits*rate for %s;z0[mm];r0[mm]",spTit[i].c_str()),
			   2000,-2000,32000,
			   200,0,3000);
      dZ0X0[i][j]=new TH2D(Form("%sZ0X0_%s",detH[j].c_str(),spH[i].c_str()),
			   Form("hits*rate for %s;z0[mm];x0[mm]",spTit[i].c_str()),
			   2000,-2000,32000,
			   200,-3000,3000);
    }
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
  double pi = acos(-1);

  //for (Long64_t event = 0; event < 5; t->GetEntry(event++)) {
  for (Long64_t event = 0; event < nEntries; t->GetEntry(event++)) {
    currentEvNr++;
    if( float(event+1)/nEntries*100 > currentProc){
      cout<<"at tree entry\t"<<event<<"\t"<< float(event+1)/nEntries*100<<endl;
      currentProc+=procStep;
    }

    double asym = abs(ev->A);
    for(int j=0;j<hit->size();j++){

      if(std::isnan(rate) || std::isinf(rate)) continue;
      if(rate==0) {rate=1;asym=0;}
      double lgRate=log10(rate);

      int sp = spM[int(abs(hit->at(j).pid))]-1;
      if(sp==-1) continue;

      int dt = dtM[int(hit->at(j).det)]-1;
      if(dt==-1) continue;

      eRate[sp][dt]->Fill(hit->at(j).p,rate);
      drate[sp][dt]->Fill(lgRate);
      drRate[sp][dt]->Fill(hit->at(j).r,rate);
      if(hit->at(j).pz<=0)
	drRatePZL0[sp][dt]->Fill(hit->at(j).r,rate);
      else
	drRatePZG0[sp][dt]->Fill(hit->at(j).r,rate);
      drRateAsym[sp][dt]->Fill(hit->at(j).r,rate*asym);

      dZ0[sp][dt]->Fill(hit->at(j).vz,rate);

      dXY[sp][dt]->Fill(hit->at(j).x,hit->at(j).y);
      dXYrate[sp][dt]->Fill(hit->at(j).x,hit->at(j).y,rate);
      dXYrateE[sp][dt]->Fill(hit->at(j).x,hit->at(j).y,rate*hit->at(j).e);

      double r0=sqrt(pow(hit->at(j).vx,2)+pow(hit->at(j).vy,2));
      dZ0R0[sp][dt]->Fill(hit->at(j).vz,r0,rate);
      dZ0X0[sp][dt]->Fill(hit->at(j).vz,hit->at(j).vx,rate);

      double phi = atan2(hit->at(j).y,hit->at(j).x);
      if(phi<0) phi+=2*pi;
      int foundRing = findDetector(sector, phi, hit->at(j).r);
      if(foundRing==-1) continue;

      if(hit->at(j).e>1 && (abs(hit->at(j).pid)==11 || abs(hit->at(j).pid)==211)){
	eRate[1][dt]->Fill(hit->at(j).p,rate);
	drate[1][dt]->Fill(lgRate);
	drRate[1][dt]->Fill(hit->at(j).r,rate);
	if(hit->at(j).pz<=0)
	  drRatePZL0[1][dt]->Fill(hit->at(j).r,rate);
	else
	  drRatePZG0[1][dt]->Fill(hit->at(j).r,rate);
	drRateAsym[1][dt]->Fill(hit->at(j).r,rate*asym);
	dZ0[1][dt]->Fill(hit->at(j).vz,rate);
	dXY[1][dt]->Fill(hit->at(j).x,hit->at(j).y);
	dXYrate[1][dt]->Fill(hit->at(j).x,hit->at(j).y,rate);
	dXYrateE[1][dt]->Fill(hit->at(j).x,hit->at(j).y,rate*hit->at(j).e);
	dZ0R0[1][dt]->Fill(hit->at(j).vz,r0,rate);
	dZ0X0[1][dt]->Fill(hit->at(j).vz,hit->at(j).vx,rate);

	if(dt==2){
	  hRate[1]->SetBinContent(foundRing*3+sector+1,
				  rate + hRate[1]->GetBinContent(foundRing*3+sector+1));
	  drRateS[1][sector]->Fill(hit->at(j).r,rate);
	  drRateAsymS[1][sector]->Fill(hit->at(j).r,rate*asym);
	}

	if(hit->at(j).trid==1 || hit->at(j).trid==2){
	  eRate[4][dt]->Fill(hit->at(j).p,rate);
	  drate[4][dt]->Fill(lgRate);
	  drRate[4][dt]->Fill(hit->at(j).r,rate);
	  if(hit->at(j).pz<=0)
	    drRatePZL0[4][dt]->Fill(hit->at(j).r,rate);
	  else
	    drRatePZG0[4][dt]->Fill(hit->at(j).r,rate);
	  drRateAsym[4][dt]->Fill(hit->at(j).r,rate*asym);
	  dZ0[4][dt]->Fill(hit->at(j).vz,rate);
	  dXY[4][dt]->Fill(hit->at(j).x,hit->at(j).y);
	  dXYrate[4][dt]->Fill(hit->at(j).x,hit->at(j).y,rate);
	  dXYrateE[4][dt]->Fill(hit->at(j).x,hit->at(j).y,rate*hit->at(j).e);
	  dZ0R0[4][dt]->Fill(hit->at(j).vz,r0,rate);
	  dZ0X0[4][dt]->Fill(hit->at(j).vz,hit->at(j).vx,rate);
	  if(dt==2){
	    hRate[4]->SetBinContent(foundRing*3+sector+1,
				    rate + hRate[4]->GetBinContent(foundRing*3+sector+1));
	    drRateS[4][sector]->Fill(hit->at(j).r,rate);
	    drRateAsymS[4][sector]->Fill(hit->at(j).r,rate*asym);
	  }

	}
      }

      if(dt==2){
	hRate[sp]->SetBinContent(foundRing*3+sector+1,
				 rate + hRate[sp]->GetBinContent(foundRing*3+sector+1));
	drRateS[sp][sector]->Fill(hit->at(j).r,rate);
	drRateAsymS[sp][sector]->Fill(hit->at(j).r,rate*asym);
      }
    }
  }
  fin->Close();
  delete fin;
  return nEntries;
};

void writeOutput(){
  fout->cd();
  for(int j=0;j<nDet;j++){
    fout->cd(detH[j].c_str());

    for(int i=0;i<nSp;i++){
      if(j==2){
	hRate[i]->Scale(1./nFiles);
	hRate[i]->Write();
	for(int k=0;k<3;k++){
	  drRateS[i][k]->Scale(1./nFiles);
	  drRateS[i][k]->Write();
	  drRateAsymS[i][k]->Scale(1./nFiles);
	  drRateAsymS[i][k]->Write();
	}
      }

      drate[i][j]->Write();

      eRate[i][j]->Scale(1./nFiles);
      eRate[i][j]->Write();

      drRate[i][j]->Scale(1./nFiles);
      drRate[i][j]->Write();

      drRatePZL0[i][j]->Scale(1./nFiles);
      drRatePZL0[i][j]->Write();

      drRatePZG0[i][j]->Scale(1./nFiles);
      drRatePZG0[i][j]->Write();

      drRateAsym[i][j]->Scale(1./nFiles);
      drRateAsym[i][j]->Write();

      dZ0[i][j]->Scale(1./nFiles);
      dZ0[i][j]->Write();

      dXY[i][j]->Write();

      dXYrate[i][j]->Scale(1./nFiles);
      dXYrate[i][j]->Write();

      dXYrateE[i][j]->Scale(1./nFiles);
      dXYrateE[i][j]->Write();

      dZ0R0[i][j]->Scale(1./nFiles);
      dZ0R0[i][j]->Write();

      dZ0X0[i][j]->Scale(1./nFiles);
      dZ0X0[i][j]->Write();
    }
  }

  fout->Close();
}

int findDetector(int &sector, double phi, double r){

  //turn off for regular analysis
  // double dPhiOffset = 2 * atan2( offsetPhi , 2*(r * 1000) );
  // phi -= dPhiOffset;// "-" because we are moving the hit; i.e. the detector moves the other way

  const double secPhi = fmod(phi, 2*pi/7);

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

  // this will jsut pick up the first hit and ignore the rest
  const int nRings = 7;
  const int nSec = 3;
  // segmented from Sakib 200131
  const double rMin[nRings][nSec]={
				   { 640.0,  640.0,  640.0},
				   { 680.0,  680.0,  680.0},
				   { 750.0,  750.0,  750.0},
				   { 855.0,  847.5,  825.0},
				   { 935.0,  920.0,  875.0},
				   {1075.0, 1080.0, 1090.0},
				   {1200.0, 1200.0, 1200.0}
  };
  const double rMax[nRings][nSec]={
				   { 680.0,  680.0,  680.0},
				   { 750.0,  750.0,  750.0},
				   { 855.0,  847.5,  825.0},
				   { 935.0,  920.0,  875.0},
				   {1075.0, 1060.0, 1055.0},
				   {1190.0, 1190.0, 1190.0},
				   {1500.0, 1500.0, 1500.0}
  };

  // hybrid rDef 200202
  // const double rMin[nRings][nSec]={
  // 				   { 640.0,  640.0,  640.0},
  // 				   { 680.0,  680.0,  680.0},
  // 				   { 730.0,  730.0,  730.0},
  // 				   { 805.0,  827.5,  835.0},
  // 				   { 855.0,  900.0,  915.0},
  // 				   {1070.0, 1060.0, 1055.0},
  // 				   {1200.0, 1200.0, 1200.0}
  // };
  // const double rMax[nRings][nSec]={
  // 				   { 680.0,  680.0,  680.0},
  // 				   { 730.0,  730.0,  730.0},
  // 				   { 805.0,  827.5,  835.0},
  // 				   { 855.0,  900.0,  915.0},
  // 				   {1070.0, 1060.0, 1055.0},
  // 				   {1170.0, 1170.0, 1170.0},
  // 				   {1500.0, 1500.0, 1500.0}
  // };

  for(int i=0;i<nRings;i++)
    if(r >= rMin[i][sector] && r <= rMax[i][sector])
      return i;

  return -1;
}

void niceLogBins(TH1*h)
{
  TAxis *axis = h->GetXaxis();
  int bins = axis->GetNbins();

  double from = axis->GetXmin();
  double to = axis->GetXmax();
  double width = (to - from) / bins;
  double *new_bins = new double[bins + 1];

  for (int i = 0; i <= bins; i++) {
    new_bins[i] = pow(10, from + i * width);
  }
  axis->Set(bins, new_bins);
  delete[] new_bins;
}
