// Run commands in this order:
//
// //Start reroot
// >build/reroot
//
// //Load in the script, and run it
// >.L analysis/processDeconv.C
// > processDeconv(<remoll output file>,<0 fot std analysis, 1 for 3 separate W2 regions>)


TFile *fout;
vector<vector<TH1D*>> hAsym_e1,hAsym_eP1;
vector<vector<vector<TH1D*>>> hAsymW_e1,hAsymW_eP1;
vector<vector<TH1D*>> hRateW;//rate for each W region for epInelastic
int separateW=0;

const int nSp=5;// [e/pi,e/pi E>1,gamma,neutron]
const string spTit[nSp]={"e/#pi","e/#pi E>1","#gamma","neutron","primary e E>1"};
const string spH[nSp]={"e","e1","g","n","eP1"};
map<int,int> spM {{11,1},{211,1},{22,3},{2112,4}};

const int nDet=1; //[28 - MD plane]
const string detH[nDet]={"det28"};
map<int,int> dtM {{28,1}};

const int nSecDet = 21; // 7(ring, including pmts) x 3 (sectors)
TH1D *hRate[nSp], *hRateAsym[nSp];
TH1D *hQ2[nSp][nSecDet],*hW2[nSp][nSecDet];
TH2D *hQ2W2[nSp][nSecDet];

TH1D *eRate[nSp][nDet];
TH1D *eRtPMT[nSp];
TH1D *drate[nSp][nDet], *drRate[nSp][nDet], *drRateAsym[nSp][nDet], *dZ0[nSp][nDet];
TH1D *drRatePZL0[nSp][nDet],*drRatePZG0[nSp][nDet];
TH2D *dXY[nSp][nDet], *dXYrate[nSp][nDet], *dXYrateE[nSp][nDet];
TH2D *dZ0R0[nSp][nDet],*dZ0X0[nSp][nDet];

const int nSec=3; //  //0,1,2 == closed, transition, open
const string secH[nSec]={"closed","trans","open"};
TH1D *drRateS[nSp][nSec], *drRateAsymS[nSp][nSec];
const int nW=3; //3 W2 regions
TH1D *drRateW[nW][nSec], *drRateAsymW[nW][nSec];

string fin;
int nFiles(0);
long currentEvNr(0);

const double pi = acos(-1);

//log X axis histograms
void niceLogBins(TH1*);

void initHisto(int);
long processOne(string);
void process();
int findDetector(int &sector, double phi, double r);
void writeOutput();

const int nRings = 7; // 6MD rings plus PMT region
double rMin[nRings][nSec],rMax[nRings][nSec];
int setSegmentation(int tileConf);

void processDeconv(const string& finName = "./remollout.root", int wRegions=0, int tiling=3){
  if(!setSegmentation(tiling))
    return;
  separateW = wRegions;
  fin = finName;
  initHisto(tiling);
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

void initHisto(int tileConf){
  string foutNm = Form("%s_tileConf%d_procDeconvV1.root",fin.substr(0,fin.find(".")).c_str(),tileConf);

  fout = new TFile(foutNm.c_str(),"RECREATE");

  fout->mkdir("deconvolution","histos for deconvolution analysis");
  fout->cd("deconvolution");

  const string secNm[3]={"closed","transition","open"};
  for(int i=0;i<6;i++){
    vector<TH1D*> dt1,dt2;
    for(int j=0;j<3;j++){
      dt1.push_back(new TH1D(Form("hAsym_e1_R%d_S%d",i+1,j),
			     Form("rate weighted Asyms for Ring %d Sector %s;asymmetry [ppb]",i+1,secNm[j].c_str()),
			     100,-1000000,1000000));
      dt2.push_back(new TH1D(Form("hAsym_eP1_R%d_S%d",i+1,j),
			     Form("rate weighted Asyms for Ring %d Sector %s;asymmetry [ppb]",i+1,secNm[j].c_str()),
			     100,-1000000,1000000));
    }
    hAsym_e1.push_back(dt1);
    hAsym_eP1.push_back(dt2);
  }

  const double wReg[3][2]={{1,1.4},{1.4,2.5},{2.5,6.0}};
  if(separateW)
    for(int k=0;k<3;k++){
      vector<TH1D*> dtt1;
      for(int i=0;i<nSp;i++){
	dtt1.push_back(new TH1D(Form("hRateW%d_%s",k+1,spH[i].c_str()),
				Form("Sums for all rings and sectors %s %4.2f<W<%4.2f",spTit[i].c_str(),wReg[k][0],wReg[k][1]),
				nSecDet,0,nSecDet));
	for(int k=1;k<=nSecDet;k++){
	  int ring= (k-1-(k-1)%3)/3+1;
	  int sector = (k-1)%3;
	  dtt1[i]->GetXaxis()->SetBinLabel(k,Form("R%d %s",ring,secNm[sector].c_str()));
	}
      }
      hRateW.push_back(dtt1);
    }
		       
  for(int k=0;k<3;k++){
    vector<vector<TH1D*>> dtt1,dtt2;
    for(int i=0;i<6;i++){
      vector<TH1D*> dt1,dt2;
      for(int j=0;j<3;j++){
	dt1.push_back(new TH1D(Form("hAsym_W%d_e1_R%d_S%d",k+1,i+1,j),
			       Form("rate weighted Asyms for Ring %d Sector %s W reg %d;asymmetry [ppb]",i+1,secNm[j].c_str(),k+1),
			       100,-1000000,1000000));
	dt2.push_back(new TH1D(Form("hAsym_W%d_eP1_R%d_S%d",k+1,i+1,j),
			       Form("rate weighted Asyms for Ring %d Sector %s W reg %d;asymmetry [ppb]",i+1,secNm[j].c_str(),k+1),
			       100,-1000000,1000000));
      }
      dtt1.push_back(dt1);
      dtt2.push_back(dt2);
    }
    hAsymW_e1.push_back(dtt1);
    hAsymW_eP1.push_back(dtt2);
  }
  
  for(int j=0;j<nDet;j++){
    fout->mkdir(detH[j].c_str(),Form("%s plane",detH[j].c_str()));
    fout->cd(detH[j].c_str());
    for(int i=0;i<nSp;i++){

      if(detH[j]=="det28"){
	eRtPMT[i]=new TH1D(Form("eRtPMT_%s",spH[i].c_str()),
			   Form("rate weighted for %s;E [MeV]",spTit[i].c_str()),
			   121,-8,4.1);
	niceLogBins(eRtPMT[i]);

	hRate[i] = new TH1D(Form("hRate_%s",spH[i].c_str()),Form("Sums for all rings and sectors %s",spTit[i].c_str()),nSecDet,0,nSecDet);
	hRateAsym[i] = new TH1D(Form("hRateAsym_%s",spH[i].c_str()),
				Form("sum(rate*Asym) for all rings and sectors %s",spTit[i].c_str()),nSecDet,0,nSecDet);
	const string secNm[3]={"closed","transition","open"};
	for(int k=1;k<=nSecDet;k++){
	  int ring= (k-1-(k-1)%3)/3+1;
	  int sector = (k-1)%3;
	  hRate[i]->GetXaxis()->SetBinLabel(k,Form("R%d %s",ring,secNm[sector].c_str()));
	  hRateAsym[i]->GetXaxis()->SetBinLabel(k,Form("R%d %s",ring,secNm[sector].c_str()));
	  hQ2[i][k-1] = new TH1D(Form("hQ2_R%d_S%d_%s",ring,sector,spH[i].c_str()),
				 Form("Q2 R%d S%s for %s",ring,secNm[sector].c_str(),spTit[i].c_str()),
				 200,0,150000);
	  hW2[i][k-1] = new TH1D(Form("hW2_R%d_S%d_%s",ring,sector,spH[i].c_str()),
				 Form("W2 R%d S%s for %s",ring,secNm[sector].c_str(),spTit[i].c_str()),
				 200,0,22000000);
	  hQ2W2[i][k-1] = new TH2D(Form("hQ2W2_R%d_S%d_%s",ring,sector,spH[i].c_str()),
				   Form("Q2xW2 R%d S%s for %s",ring,secNm[sector].c_str(),spTit[i].c_str()),
				   200,0,150000,
				   200,0,22000000);
	}
	for(int k=0;k<nSec;k++){
	  drRateS[i][k]=new TH1D(Form("det28_rRate_S%d_%s",k,spH[i].c_str()),
				 Form("rate weighted %s for %s;r[mm]",
				      spTit[i].c_str(),secH[k].c_str()),
				 600,600,1200);
	  drRateAsymS[i][k]=new TH1D(Form("det28_rRateAsym_S%d_%s",k,spH[i].c_str()),
				     Form("rate*asym weighted %s for %s;r[mm]",
					  spTit[i].c_str(),secH[k].c_str()),
				     600,600,1200);
	  for(int l=0;l<nW && separateW && i==0; l++){
	    //cout<<i<<" "<<k<<" "<<l<<" ";
	    drRateW[l][k]=new TH1D(Form("det28_rRate_S%d_W%d",k,l),
				   Form("rate weighted e1 for %s W%d;r[mm]",
					secH[k].c_str(),l),
				   600,600,1200);
	    drRateAsymW[l][k]=new TH1D(Form("det28_rRateAsym_S%d_W%d",k,l),
				       Form("rate*asym weighted e1 for %s W%d;r[mm]",
					    secH[k].c_str(),l),
				       600,600,1200);
	    //cout<<drRateAsymW[l][k]->GetName()<<endl;
	  }
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
			     800,-2000,2000,
			     800,-2000,2000);
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
  float currentProc=1,procStep=60;
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

      //if(hit->at(j).vz>18500 && hit->at(j).vz<18835) continue;//remove DS vac window
      //if(hit->at(j).vz>18835 && hit->at(j).vz<19020) continue;//remove collar2
      //if(hit->at(j).vz>19020 && hit->at(j).vz<22000) continue;//remove air
      
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

      int wRegion=-1;
      if(separateW){
	double wVal = sqrt(ev->W2)/1000;//MeV to GeV
	if( wVal >= 1 && wVal < 1.4)
	  wRegion = 0;
	else if(wVal >= 1.4 && wVal <2.5)
	  wRegion = 1;
	else if(wVal >= 2.5 && wVal <6.0)
	  wRegion = 2;
      }

      if(foundRing==6){
	eRtPMT[sp]->Fill(hit->at(j).p,rate);
      }

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

	if(dt==0){
	  //cout<<foundRing<<" "<<sector<<endl;
	  //cout<<hAsym_e1.size()<<endl;
	  if(foundRing<6){
	    hAsym_e1[foundRing][sector]->Fill(asym,rate);//for primary+secondaries
	    if(wRegion!=-1){
	      hAsymW_e1[wRegion][foundRing][sector]->Fill(asym,rate);//for primary+secondaries
	    }
	  }else if(foundRing==6){
	    eRtPMT[1]->Fill(hit->at(j).p,rate);
	  }

	  if(wRegion!=-1){
	    hRateW[wRegion][1]->SetBinContent(foundRing*3+sector+1,
					      rate + hRateW[wRegion][1]->GetBinContent(foundRing*3+sector+1));
	  }
	  hRate[1]->SetBinContent(foundRing*3+sector+1,
				  rate + hRate[1]->GetBinContent(foundRing*3+sector+1));
	  hRateAsym[1]->SetBinContent(foundRing*3+sector+1,
				      rate*abs(asym) + hRateAsym[1]->GetBinContent(foundRing*3+sector+1));
	  hQ2[1][foundRing]->Fill(ev->Q2,rate);
	  hW2[1][foundRing]->Fill(ev->W2,rate);
	  hQ2W2[1][foundRing]->Fill(ev->Q2,ev->W2,rate);
	  drRateS[1][sector]->Fill(hit->at(j).r,rate);
	  drRateAsymS[1][sector]->Fill(hit->at(j).r,rate*asym);
	  if(wRegion!=-1){
	    //cout<<wRegion<<" "<<sector<<endl;
	    drRateW[wRegion][sector]->Fill(hit->at(j).r,rate);
	    drRateAsymW[wRegion][sector]->Fill(hit->at(j).r,rate*asym);
	  }
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
	  if(dt==0){
	    if(foundRing<6){
	      hAsym_eP1[foundRing][sector]->Fill(asym,rate);//for primary only
	      if(wRegion!=-1){
		hAsymW_eP1[wRegion][foundRing][sector]->Fill(asym,rate);//for primary+secondaries
	      }
	    }else if(foundRing==6){
	      eRtPMT[4]->Fill(hit->at(j).p,rate);
	    }
	    if(wRegion!=-1){
	      hRateW[wRegion][4]->SetBinContent(foundRing*3+sector+1,
						rate + hRateW[wRegion][4]->GetBinContent(foundRing*3+sector+1));
	      
	    }
	    hRate[4]->SetBinContent(foundRing*3+sector+1,
				    rate + hRate[4]->GetBinContent(foundRing*3+sector+1));
	    hRateAsym[4]->SetBinContent(foundRing*3+sector+1,
					rate*abs(asym) + hRateAsym[4]->GetBinContent(foundRing*3+sector+1));
	    hQ2[4][foundRing]->Fill(ev->Q2,rate);
	    hW2[4][foundRing]->Fill(ev->W2,rate);
	    hQ2W2[4][foundRing]->Fill(ev->Q2,ev->W2,rate);
	    drRateS[4][sector]->Fill(hit->at(j).r,rate);
	    drRateAsymS[4][sector]->Fill(hit->at(j).r,rate*asym);
	  }

	}
      }

      if(dt==0){
	if(wRegion!=-1){
	  hRateW[wRegion][sp]->SetBinContent(foundRing*3+sector+1,
					     rate + hRateW[wRegion][sp]->GetBinContent(foundRing*3+sector+1));
	}
	hRate[sp]->SetBinContent(foundRing*3+sector+1,
				 rate + hRate[sp]->GetBinContent(foundRing*3+sector+1));
	hRateAsym[sp]->SetBinContent(foundRing*3+sector+1,
				     rate*abs(asym) + hRateAsym[sp]->GetBinContent(foundRing*3+sector+1));
	hQ2[sp][foundRing]->Fill(ev->Q2,rate);
	hW2[sp][foundRing]->Fill(ev->W2,rate);
	hQ2W2[sp][foundRing]->Fill(ev->Q2,ev->W2,rate);
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
  fout->cd("deconvolution");
  for(int i=0;i<6;i++)
    for(int j=0;j<3;j++){
      hAsym_e1[i][j]->Scale(1./nFiles);
      hAsym_e1[i][j]->Write();
      hAsym_eP1[i][j]->Scale(1./nFiles);
      hAsym_eP1[i][j]->Write();
    }

  if(separateW){
    for(int k=0;k<3;k++){
      for(int i=0;i<nSp;i++){
	hRateW[k][i]->Scale(1./nFiles);
	hRateW[k][i]->Write();
      }
      for(int i=0;i<6;i++)
	for(int j=0;j<3;j++){
	  hAsymW_e1[k][i][j]->Scale(1./nFiles);
	  hAsymW_e1[k][i][j]->Write();
	  hAsymW_eP1[k][i][j]->Scale(1./nFiles);
	  hAsymW_eP1[k][i][j]->Write();
	}
    }
  }
  
  for(int j=0;j<nDet;j++){
    fout->cd(detH[j].c_str());

    for(int i=0;i<nSp;i++){
      if(detH[j]=="det28"){
	eRtPMT[i]->Scale(1./nFiles);
	eRtPMT[i]->Write();
	hRate[i]->Scale(1./nFiles);
	hRate[i]->Write();
	hRateAsym[i]->Scale(1./nFiles);
	hRateAsym[i]->Write();
	for(int k=0;k<nSecDet;k++){
	  hQ2[i][k]->Scale(1./nFiles);
	  hQ2[i][k]->Write();
	  hW2[i][k]->Scale(1./nFiles);
	  hW2[i][k]->Write();
	  hQ2W2[i][k]->Scale(1./nFiles);
	  hQ2W2[i][k]->Write();
	}
	for(int k=0;k<3;k++){
	  drRateS[i][k]->Scale(1./nFiles);
	  drRateS[i][k]->Write();
	  drRateAsymS[i][k]->Scale(1./nFiles);
	  drRateAsymS[i][k]->Write();

	  for(int l=0;l<nW && separateW && i==0; l++){
	    drRateW[l][k]->Scale(1./nFiles);
	    drRateW[l][k]->Write();
	    drRateAsymW[l][k]->Scale(1./nFiles);
	    drRateAsymW[l][k]->Write();
	  }
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

  // this will just pick up the first hit and ignore the rest if there is overlap in tiling
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


int setSegmentation(int tileConf){

  switch(tileConf){
  case 3:{
    // segmented from Sakib 210501
    double rm3[nRings][nSec]={
      { 650.0,  650.0,  650.0},
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 790.0,  790.0,  790.0},
      { 900.0,  900.0,  900.0},
      {1060.0, 1060.0, 1060.0},
      {1200.0, 1200.0, 1200.0}
    };
    double rM3[nRings][nSec]={
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 790.0,  790.0,  790.0},
      { 900.0,  900.0,  900.0},
      {1060.0, 1060.0, 1060.0},
      {1160.0, 1160.0, 1160.0},
      {1500.0, 1500.0, 1500.0}
    };
    cout<<"using tiling configuration from 2021-05-01 for parallel segmented magnets; uniform R5=16cm"<<endl;
    for(int i=0;i<nRings;i++)
      for(int j=0;j<nSec;j++){
	rMin[i][j]=rm3[i][j];
	rMax[i][j]=rM3[i][j];
      }
    return 1;
  }case 17:{
    // segmented from Sakib 210501
    double rm3[nRings][nSec]={
      { 650.0,  650.0,  650.0},
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 790.0,  790.0,  790.0},
      { 940.0,  940.0,  940.0},
      {1060.0, 1060.0, 1060.0},
      {1200.0, 1200.0, 1200.0}
    };
    double rM3[nRings][nSec]={
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 790.0,  790.0,  790.0},
      { 940.0,  940.0,  940.0},
      {1060.0, 1060.0, 1060.0},
      {1160.0, 1160.0, 1160.0},
      {1500.0, 1500.0, 1500.0}
    };
    cout<<"using tiling configuration from 2021-05-01 for parallel segmented magnets; uniform R5=16cm"<<endl;
    for(int i=0;i<nRings;i++)
      for(int j=0;j<nSec;j++){
	rMin[i][j]=rm3[i][j];
	rMax[i][j]=rM3[i][j];
      }
    return 1;
  }case 16:{
    double rm3[nRings][nSec]={
      { 650.0,  650.0,  650.0},
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 800.0,  800.0,  800.0},
      { 920.0,  920.0,  920.0},
      {1060.0, 1060.0, 1060.0},
      {1200.0, 1200.0, 1200.0}
    };
    double rM3[nRings][nSec]={
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 800.0,  800.0,  800.0},
      { 920.0,  920.0,  920.0},
      {1060.0, 1060.0, 1060.0},
      {1160.0, 1160.0, 1160.0},
      {1500.0, 1500.0, 1500.0}
    };
    cout<<"using tiling configuration from 2021-05-01 for parallel segmented magnets; uniform R5=16cm"<<endl;
    for(int i=0;i<nRings;i++)
      for(int j=0;j<nSec;j++){
	rMin[i][j]=rm3[i][j];
	rMax[i][j]=rM3[i][j];
      }
    return 1;
  }case 21:{
    double rm3[nRings][nSec]={
      { 650.0,  650.0,  650.0},
      { 690.0,  690.0,  690.0},
      { 740.0,  740.0,  740.0},
      { 800.0,  800.0,  800.0},
      { 920.0,  920.0,  920.0},
      {1060.0, 1060.0, 1060.0},
      {1200.0, 1200.0, 1200.0}
    };
    double rM3[nRings][nSec]={
      { 690.0,  690.0,  690.0},
      { 740.0,  740.0,  740.0},
      { 800.0,  800.0,  800.0},
      { 920.0,  920.0,  920.0},
      {1060.0, 1060.0, 1060.0},
      {1160.0, 1160.0, 1160.0},
      {1500.0, 1500.0, 1500.0}
    };
    cout<<"using tiling configuration from 2021-05-01 for parallel segmented magnets; uniform R5=16cm"<<endl;
    for(int i=0;i<nRings;i++)
      for(int j=0;j<nSec;j++){
	rMin[i][j]=rm3[i][j];
	rMax[i][j]=rM3[i][j];
      }
    return 1;
  }case 22:{
    double rm3[nRings][nSec]={
      { 650.0,  650.0,  650.0},
      { 680.0,  680.0,  680.0},
      { 740.0,  740.0,  740.0},
      { 800.0,  800.0,  800.0},
      { 920.0,  920.0,  920.0},
      {1060.0, 1060.0, 1060.0},
      {1200.0, 1200.0, 1200.0}
    };
    double rM3[nRings][nSec]={
      { 680.0,  680.0,  680.0},
      { 740.0,  740.0,  740.0},
      { 800.0,  800.0,  800.0},
      { 920.0,  920.0,  920.0},
      {1060.0, 1060.0, 1060.0},
      {1160.0, 1160.0, 1160.0},
      {1500.0, 1500.0, 1500.0}
    };
    cout<<"using tiling configuration from 2021-05-01 for parallel segmented magnets; uniform R5=16cm"<<endl;
    for(int i=0;i<nRings;i++)
      for(int j=0;j<nSec;j++){
	rMin[i][j]=rm3[i][j];
	rMax[i][j]=rM3[i][j];
      }
    return 1;
  }case 26:{
    double rm3[nRings][nSec]={
      { 650.0,  650.0,  650.0},
      { 680.0,  680.0,  680.0},
      { 740.0,  740.0,  740.0},
      { 800.0,  800.0,  800.0},
      { 920.0,  920.0,  920.0},
      {1070.0, 1070.0, 1070.0},
      {1200.0, 1200.0, 1200.0}
    };
    double rM3[nRings][nSec]={
      { 680.0,  680.0,  680.0},
      { 740.0,  740.0,  740.0},
      { 800.0,  800.0,  800.0},
      { 920.0,  920.0,  920.0},
      {1070.0, 1070.0, 1070.0},
      {1160.0, 1160.0, 1160.0},
      {1500.0, 1500.0, 1500.0}
    };
    cout<<"using tiling configuration from 2021-05-01 for parallel segmented magnets; uniform R5=16cm"<<endl;
    for(int i=0;i<nRings;i++)
      for(int j=0;j<nSec;j++){
	rMin[i][j]=rm3[i][j];
	rMax[i][j]=rM3[i][j];
      }
    return 1;
  }case 25:{
    double rm3[nRings][nSec]={
      { 650.0,  650.0,  650.0},
      { 680.0,  680.0,  680.0},
      { 740.0,  740.0,  740.0},
      { 800.0,  800.0,  800.0},
      { 920.0,  920.0,  920.0},
      {1050.0, 1050.0, 1050.0},
      {1200.0, 1200.0, 1200.0}
    };
    double rM3[nRings][nSec]={
      { 680.0,  680.0,  680.0},
      { 740.0,  740.0,  740.0},
      { 800.0,  800.0,  800.0},
      { 920.0,  920.0,  920.0},
      {1050.0, 1050.0, 1050.0},
      {1160.0, 1160.0, 1160.0},
      {1500.0, 1500.0, 1500.0}
    };
    cout<<"using tiling configuration from 2021-05-01 for parallel segmented magnets; uniform R5=16cm"<<endl;
    for(int i=0;i<nRings;i++)
      for(int j=0;j<nSec;j++){
	rMin[i][j]=rm3[i][j];
	rMax[i][j]=rM3[i][j];
      }
    return 1;
  }case 24:{
    double rm3[nRings][nSec]={
      { 650.0,  650.0,  650.0},
      { 680.0,  680.0,  680.0},
      { 740.0,  740.0,  740.0},
      { 800.0,  800.0,  800.0},
      { 920.0,  920.0,  920.0},
      {1040.0, 1040.0, 1040.0},
      {1200.0, 1200.0, 1200.0}
    };
    double rM3[nRings][nSec]={
      { 680.0,  680.0,  680.0},
      { 740.0,  740.0,  740.0},
      { 800.0,  800.0,  800.0},
      { 920.0,  920.0,  920.0},
      {1040.0, 1040.0, 1040.0},
      {1160.0, 1160.0, 1160.0},
      {1500.0, 1500.0, 1500.0}
    };
    cout<<"using tiling configuration from 2021-05-01 for parallel segmented magnets; uniform R5=16cm"<<endl;
    for(int i=0;i<nRings;i++)
      for(int j=0;j<nSec;j++){
	rMin[i][j]=rm3[i][j];
	rMax[i][j]=rM3[i][j];
      }
    return 1;
  }case 23:{
    double rm3[nRings][nSec]={
      { 650.0,  650.0,  650.0},
      { 680.0,  680.0,  680.0},
      { 740.0,  740.0,  740.0},
      { 800.0,  800.0,  800.0},
      { 920.0,  920.0,  920.0},
      {1030.0, 1030.0, 1030.0},
      {1200.0, 1200.0, 1200.0}
    };
    double rM3[nRings][nSec]={
      { 680.0,  680.0,  680.0},
      { 740.0,  740.0,  740.0},
      { 800.0,  800.0,  800.0},
      { 920.0,  920.0,  920.0},
      {1030.0, 1030.0, 1030.0},
      {1160.0, 1160.0, 1160.0},
      {1500.0, 1500.0, 1500.0}
    };
    cout<<"using tiling configuration from 2021-05-01 for parallel segmented magnets; uniform R5=16cm"<<endl;
    for(int i=0;i<nRings;i++)
      for(int j=0;j<nSec;j++){
	rMin[i][j]=rm3[i][j];
	rMax[i][j]=rM3[i][j];
      }
    return 1;
  }case 15:{
    // segmented from Sakib 210501
    double rm3[nRings][nSec]={
      { 650.0,  650.0,  650.0},
      { 680.0,  680.0,  680.0},
      { 735.0,  735.0,  735.0},
      { 790.0,  790.0,  790.0},
      { 900.0,  900.0,  900.0},
      {1060.0, 1060.0, 1060.0},
      {1200.0, 1200.0, 1200.0}
    };
    double rM3[nRings][nSec]={
      { 680.0,  680.0,  680.0},
      { 735.0,  735.0,  735.0},
      { 790.0,  790.0,  790.0},
      { 900.0,  900.0,  900.0},
      {1060.0, 1060.0, 1060.0},
      {1160.0, 1160.0, 1160.0},
      {1500.0, 1500.0, 1500.0}
    };
    cout<<"using tiling configuration from 2021-05-01 for parallel segmented magnets; uniform R5=16cm"<<endl;
    for(int i=0;i<nRings;i++)
      for(int j=0;j<nSec;j++){
	rMin[i][j]=rm3[i][j];
	rMax[i][j]=rM3[i][j];
      }
    return 1;
  }case 14:{
    // segmented from Sakib 210501
    double rm3[nRings][nSec]={
      { 650.0,  650.0,  650.0},
      { 690.0,  690.0,  690.0},
      { 745.0,  745.0,  745.0},
      { 790.0,  790.0,  790.0},
      { 900.0,  900.0,  900.0},
      {1060.0, 1060.0, 1060.0},
      {1200.0, 1200.0, 1200.0}
    };
    double rM3[nRings][nSec]={
      { 690.0,  690.0,  690.0},
      { 745.0,  745.0,  745.0},
      { 790.0,  790.0,  790.0},
      { 900.0,  900.0,  900.0},
      {1060.0, 1060.0, 1060.0},
      {1160.0, 1160.0, 1160.0},
      {1500.0, 1500.0, 1500.0}
    };
    cout<<"using tiling configuration from 2021-05-01 for parallel segmented magnets; uniform R5=16cm"<<endl;
    for(int i=0;i<nRings;i++)
      for(int j=0;j<nSec;j++){
	rMin[i][j]=rm3[i][j];
	rMax[i][j]=rM3[i][j];
      }
    return 1;
  }case 13:{
    // segmented from Sakib 210501
    double rm3[nRings][nSec]={
      { 650.0,  650.0,  650.0},
      { 690.0,  690.0,  690.0},
      { 740.0,  740.0,  740.0},
      { 790.0,  790.0,  790.0},
      { 900.0,  900.0,  900.0},
      {1060.0, 1060.0, 1060.0},
      {1200.0, 1200.0, 1200.0}
    };
    double rM3[nRings][nSec]={
      { 690.0,  690.0,  690.0},
      { 740.0,  740.0,  740.0},
      { 790.0,  790.0,  790.0},
      { 900.0,  900.0,  900.0},
      {1060.0, 1060.0, 1060.0},
      {1160.0, 1160.0, 1160.0},
      {1500.0, 1500.0, 1500.0}
    };
    cout<<"using tiling configuration from 2021-05-01 for parallel segmented magnets; uniform R5=16cm"<<endl;
    for(int i=0;i<nRings;i++)
      for(int j=0;j<nSec;j++){
	rMin[i][j]=rm3[i][j];
	rMax[i][j]=rM3[i][j];
      }
    return 1;
  }case 11:{
    // segmented from Sakib 210501
    double rm3[nRings][nSec]={
      { 650.0,  650.0,  650.0},
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 800.0,  800.0,  800.0},
      { 900.0,  900.0,  900.0},
      {1060.0, 1060.0, 1060.0},
      {1200.0, 1200.0, 1200.0}
    };
    double rM3[nRings][nSec]={
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 800.0,  800.0,  800.0},
      { 900.0,  900.0,  900.0},
      {1060.0, 1060.0, 1060.0},
      {1160.0, 1160.0, 1160.0},
      {1500.0, 1500.0, 1500.0}
    };
    cout<<"using tiling configuration from 2021-05-01 for parallel segmented magnets; uniform R5=16cm"<<endl;
    for(int i=0;i<nRings;i++)
      for(int j=0;j<nSec;j++){
	rMin[i][j]=rm3[i][j];
	rMax[i][j]=rM3[i][j];
      }
    return 1;
  }case 12:{
    // segmented from Sakib 210501
    double rm3[nRings][nSec]={
      { 650.0,  650.0,  650.0},
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 780.0,  780.0,  780.0},
      { 900.0,  900.0,  900.0},
      {1060.0, 1060.0, 1060.0},
      {1200.0, 1200.0, 1200.0}
    };
    double rM3[nRings][nSec]={
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 780.0,  780.0,  780.0},
      { 900.0,  900.0,  900.0},
      {1060.0, 1060.0, 1060.0},
      {1160.0, 1160.0, 1160.0},
      {1500.0, 1500.0, 1500.0}
    };
    cout<<"using tiling configuration from 2021-05-01 for parallel segmented magnets; uniform R5=16cm"<<endl;
    for(int i=0;i<nRings;i++)
      for(int j=0;j<nSec;j++){
	rMin[i][j]=rm3[i][j];
	rMax[i][j]=rM3[i][j];
      }
    return 1;
  }case 10:{
    // segmented from Sakib 210501
    double rm3[nRings][nSec]={
      { 650.0,  650.0,  650.0},
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 790.0,  790.0,  790.0},
      { 920.0,  920.0,  920.0},
      {1060.0, 1060.0, 1060.0},
      {1200.0, 1200.0, 1200.0}
    };
    double rM3[nRings][nSec]={
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 790.0,  790.0,  790.0},
      { 920.0,  920.0,  920.0},
      {1060.0, 1060.0, 1060.0},
      {1160.0, 1160.0, 1160.0},
      {1500.0, 1500.0, 1500.0}
    };
    cout<<"using tiling configuration from 2021-05-01 for parallel segmented magnets; uniform R5=16cm"<<endl;
    for(int i=0;i<nRings;i++)
      for(int j=0;j<nSec;j++){
	rMin[i][j]=rm3[i][j];
	rMax[i][j]=rM3[i][j];
      }
    return 1;
  }case 7:{
    double rm3[nRings][nSec]={
      { 650.0,  650.0,  650.0},
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 790.0,  790.0,  790.0},
      { 900.0,  900.0,  900.0},
      {1040.0, 1040.0, 1040.0},
      {1200.0, 1200.0, 1200.0}
    };
    double rM3[nRings][nSec]={
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 790.0,  790.0,  790.0},
      { 900.0,  900.0,  900.0},
      {1040.0, 1040.0, 1040.0},
      {1160.0, 1160.0, 1160.0},
      {1500.0, 1500.0, 1500.0}
    };
    cout<<"using tiling configuration from 2021-05-01 for parallel segmented magnets; uniform R5=20cm"<<endl;
    for(int i=0;i<nRings;i++)
      for(int j=0;j<nSec;j++){
	rMin[i][j]=rm3[i][j];
	rMax[i][j]=rM3[i][j];
      }
    return 1;
   }case 9:{
    double rm3[nRings][nSec]={
      { 650.0,  650.0,  650.0},
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 790.0,  790.0,  790.0},
      { 910.0,  910.0,  910.0},
      {1060.0, 1060.0, 1060.0},
      {1200.0, 1200.0, 1200.0}
    };
    double rM3[nRings][nSec]={
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 790.0,  790.0,  790.0},
      { 910.0,  910.0,  910.0},
      {1060.0, 1060.0, 1060.0},
      {1160.0, 1160.0, 1160.0},
      {1500.0, 1500.0, 1500.0}
    };
    cout<<"using tiling configuration from 2021-05-01 for parallel segmented magnets; uniform R5=20cm"<<endl;
    for(int i=0;i<nRings;i++)
      for(int j=0;j<nSec;j++){
	rMin[i][j]=rm3[i][j];
	rMax[i][j]=rM3[i][j];
      }
    return 1;
  }case 5:{
    double rm3[nRings][nSec]={
      { 650.0,  650.0,  650.0},
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 790.0,  790.0,  790.0},
      { 900.0,  900.0,  900.0},
      {1080.0, 1080.0, 1080.0},
      {1200.0, 1200.0, 1200.0}
    };
    double rM3[nRings][nSec]={
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 790.0,  790.0,  790.0},
      { 900.0,  900.0,  900.0},
      {1080.0, 1080.0, 1080.0},
      {1160.0, 1160.0, 1160.0},
      {1500.0, 1500.0, 1500.0}
    };
    cout<<"using tiling configuration from 2021-05-01 for parallel segmented magnets; uniform R5=18cm"<<endl;
    for(int i=0;i<nRings;i++)
      for(int j=0;j<nSec;j++){
	rMin[i][j]=rm3[i][j];
	rMax[i][j]=rM3[i][j];
      }
    return 1;
  }case 2:{
     // // segmented from Sakib 200131
     double rm2[nRings][nSec]={
       { 640.0,  640.0,  640.0},
       { 680.0,  680.0,  680.0},
       { 750.0,  750.0,  750.0},
       { 855.0,  847.5,  825.0},
       { 935.0,  920.0,  875.0},
       {1075.0, 1080.0, 1090.0},
       {1200.0, 1200.0, 1200.0}
     };
     double rM2[nRings][nSec]={
       { 680.0,  680.0,  680.0},
       { 750.0,  750.0,  750.0},
       { 855.0,  847.5,  825.0},
       { 935.0,  920.0,  875.0},
       {1075.0, 1060.0, 1055.0},
       {1190.0, 1190.0, 1190.0},
       {1500.0, 1500.0, 1500.0}
     };
     cout<<"using tiling configuration from 2020-01-31 segmented magnets"<<endl;
     for(int i=0;i<nRings;i++)
       for(int j=0;j<nSec;j++){
	 rMin[i][j]=rm2[i][j];
	 rMax[i][j]=rM2[i][j];
       }
     return 1;
   }case 4:{
     double rm2[nRings][nSec]={
       { 640.0,  640.0,  640.0},
       { 680.0,  680.0,  680.0},
       { 750.0,  750.0,  750.0},
       { 855.0,  847.5,  825.0},
       { 935.0,  920.0,  875.0},
       {1075.0, 1080.0, 1090.0},
       {1200.0, 1200.0, 1200.0}
     };
     double rM2[nRings][nSec]={
       { 680.0,  680.0,  680.0},
       { 750.0,  750.0,  750.0},
       { 855.0,  847.5,  825.0},
       { 935.0,  920.0,  875.0},
       {1075.0, 1080.0, 1090.0},
       {1190.0, 1190.0, 1190.0},
       {1500.0, 1500.0, 1500.0}
     };
     cout<<"using tiling configuration from 2020-01-31 segmented magnets; with no gaps!"<<endl;
     for(int i=0;i<nRings;i++)
       for(int j=0;j<nSec;j++){
	 rMin[i][j]=rm2[i][j];
	 rMax[i][j]=rM2[i][j];
       }
     return 1;
   }case 1:{
      // hybrid rDef 200202
      double rm1[nRings][nSec]={
	{ 640.0,  640.0,  640.0},
	{ 680.0,  680.0,  680.0},
	{ 730.0,  730.0,  730.0},
	{ 805.0,  827.5,  835.0},
	{ 855.0,  900.0,  915.0},
	{1070.0, 1060.0, 1055.0},
	{1200.0, 1200.0, 1200.0}
      };
      double rM1[nRings][nSec]={
	{ 680.0,  680.0,  680.0},
	{ 730.0,  730.0,  730.0},
	{ 805.0,  827.5,  835.0},
	{ 855.0,  900.0,  915.0},
	{1070.0, 1060.0, 1055.0},
	{1170.0, 1170.0, 1170.0},
	{1500.0, 1500.0, 1500.0}
      };
      cout<<"using tiling configuration from 2020-02-02 for hybrid magnets"<<endl;
      for(int i=0;i<nRings;i++)
	for(int j=0;j<nSec;j++){
	  rMin[i][j]=rm1[i][j];
	  rMax[i][j]=rM1[i][j];
	}
      return 1;
    } default:{
	cout<<"tiling setting unknown: quitting!"<<endl;
	return 0;
      }

  }
}
