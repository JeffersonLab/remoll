// Run commands in this order:
// 
// //Start reroot
// >build/reroot
//
// //Load in the script, and run it
// >.L analysis/basicAna.C
// > basicAna(<remoll output file>)


TFile *fout;
vector<vector<TH1D*>> hAsym;
TH1D *hScatAngP1,*hScatAngP2;
TH1D *hRate,*rRate, *rRateAsym,*r,*sourceZ,*eRateAsym;
TH2D *hXY,*hXYrate, *hXYrateAsym;
TH2D *hVtxAngR, *hAfterColl2AngR;
TH2D *hVtxAngRrate, *hAfterColl2AngRrate;
TH2D *hVtxAngE, *hAfterColl2AngE;
TH2D *hVtxAngErate, *hAfterColl2AngErate;
TH2D *hVtxER, *hAfterColl2ER;
TH2D *hVtxERrate, *hAfterColl2ERrate;
string fin;
//raster
TH2D *beamRaster,*rBeamRaster;
TH1D *xRate;
TH1D *xRateAll;
TH1D *leftRate,*rightRate;
TH1D *lR,*rR;

int nFiles(0);

long currentEvNr(0);

float offsetR(0),offsetPhi(0);
const double pi = acos(-1);

void initHisto();
long processOne(string);
void process();
int findDetector(int &sector, double phi, double r);
void writeOutput();

void basicAna(const string& finName = "./remollout.root"){
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
  string foutNm = Form("%s_bkgAnaV4.root",fin.substr(0,fin.find(".")).c_str());

  fout = new TFile(foutNm.c_str(),"RECREATE");

  hRate = new TH1D("hRate","Sums for all rings and sectors",18,0,18);
  const string secNm[3]={"closed","transition","open"};
  for(int i=1;i<=18;i++)
    hRate->GetXaxis()->SetBinLabel(i,Form("R%d %s",(i-1-(i-1)%3)/3+1,secNm[(i-1)%3].c_str()));

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

  fout->mkdir("QA","quality assurance plots");
  fout->cd("QA");
  r = new TH1D("r","radial distribution;r[m]",200,500,1500);
  rRate = new TH1D("rRate","rate weighted distribution;r[m]",200,500,1500);
  rRateAsym = new TH1D("rRateAsym","rate*Asym weighted distribution;r[m]",200,500,1500);
  eRateAsym = new TH1D("eRateAsym","rate*Asym weighted distribution ring 5;E[MeV]",200,0,13000);
  hVtxAngR = new TH2D("hVtxAngR","At vertex; r on Main detector [m]; scattering angle[deg]",100,600,1200,200,0,1.5);
  hVtxAngRrate = new TH2D("hVtxAngRrate","At vertex rate weighted; r on Main detector [m]; scattering angle[deg]",100,600,1200,200,0,1.5);
  hAfterColl2AngR = new TH2D("hAfterColl2AngR","After coll 2;  r on Main detector [m]; scattering angle[deg]",100,600,1200,200,0,1.5);
  hAfterColl2AngRrate = new TH2D("hAfterColl2AngRrate","After coll 2 rate weighted; r on Main detector [m]; scattering angle[deg]",100,600,1200,200,0,1.5);
  hScatAngP1 = new TH1D("hScatAngP1","scattering angle at the vertex",720,0,30);
  hScatAngP2 = new TH1D("hScatAngP2","scattering angle at the vertex",720,0,30);
  
  hVtxER = new TH2D("hVtxER","At vertex; r on Main detector [m]; total momentum [GeV]",100,600,1200,200,0,12000);
  hVtxERrate = new TH2D("hVtxERrate","At vertex rate weighted; r on Main det [m]; scattering angle[deg]",100,600,1200,200,0,12000);
  hAfterColl2ER = new TH2D("hAfterColl2ER","After coll 2; r on Main det [m]; scattering angle[deg]",100,600,1200,200,0,12000);
  hAfterColl2ERrate = new TH2D("hAfterColl2ERrate","After coll 2 rate weighted; r on Main det [m]; scattering angle[deg]",100,600,1200,200,0,12000);

  hVtxAngE = new TH2D("hVtxAngE","At vertex (selection on ring 5 hit);scattering angle[deg]; total momentum [GeV]",200,0,1.5,200,0,12000);
  hVtxAngErate = new TH2D("hVtxAngErate","At vertex rate weighted (selection on ring 5 hit);scattering angle[deg]; total momentum [GeV]",200,0,1.5,200,0,12000);
  hAfterColl2AngE = new TH2D("hAfterColl2AngE","After coll 2 (select on ring5 hit); scattering angle[deg]; total momentum [GeV]",200,0,1.5,200,0,12000);
  hAfterColl2AngErate = new TH2D("hAfterColl2AngErate","After coll 2 rate weight (select on ring5 hit); scattering angle[deg]; total momentum [GeV]",200,0,1.5,200,0,12000);

  hXY = new TH2D("hXY","2D hit ditribution;x [m];y [m]",200,-2100,2100,200,-2100,2100);
  hXYrate = new TH2D("hXYrate","rate weighted 2D hit ditribution;x [m];y [m]",200,-2100,2100,200,-2100,2100);
  hXYrateAsym = new TH2D("hXYrateAsym","rate*asym weighted 2D hit ditribution;x [m];y [m]",200,-2100,2100,200,-2100,2100);
  sourceZ = new TH1D("sourceZ","initial vertex for hit ;z position [m]",5000,-5500,-3500);
  beamRaster = new TH2D("beamRaster","sampling for ring 5 ;x raster [mm]; y raster [mm]",200,-8,8,200,-8,8);
  rBeamRaster = new TH2D("rBeamRaster","rate weighted ;x raster [mm]; y raster [mm]",200,-8,8,200,-8,8);

  xRate = new TH1D("xRate","rate weighted raster position",200,-15,15);
  xRateAll = new TH1D("xRateAll","rate weighted raster position",200,-15,15);
  leftRate = new TH1D("leftRate","rate for beam raster -2.5 to -0.5 per 10000 events; rate [GHz]",500,0,50);
  rightRate = new TH1D("rightRate","rate for beam raster 0.5 to 2.5 per 10000 events; rate [GHz]",500,0,50);

  lR = new TH1D("lR","rate for beam raster -2.5 to -0.5 per event; rate [GHz]",500,0,1);
  rR = new TH1D("rR","rate for beam raster 0.5 to 2.5 per event; rate [GHz]",500,0,1);

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
  float currentProc=1,procStep=10;
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

    if(currentEvNr % 10000 == 1 && currentEvNr>2){
      cout<<"avg "<<currentEvNr<<" " <<event<<endl;
      leftRate->Fill(lR->GetMean());
      rightRate->Fill(rR->GetMean());
      lR->Reset();
      rR->Reset();
    }

    double asym = ev->A;

    double scatAng[2]={-1,-1};
    double scatP[2]={-1,-1};

    if(part){
      for(int j=0;j<part->size();j++){
	if(j>2){
	  cout<<"Event npart >2 skipping this event:\n"
	      <<part->at(j).p<<"\t"<<part->at(j).pz<<"\t"<<part->at(j).pid<<endl;
	  break;
	}
	if(part->at(j).pid!=11) continue;
	scatAng[j] = acos(part->at(j).pz/part->at(j).p) * 180/pi;
	scatP[j] = part->at(j).p;
      }
    }
    if(scatAng[0]!=-1) hScatAngP1->Fill(scatAng[0]);
    if(scatAng[1]!=-1) hScatAngP2->Fill(scatAng[1]);

    procID.clear();
    double afterColl2Ang[2]={-1,-1};
    double afterColl2P[2]={-1,-1};

    for(int j=0;j<hit->size();j++){

      if(hit->at(j).det==200 && (hit->at(j).trid==1 || hit->at(j).trid==2) && abs(hit->at(j).pid)==11){
	afterColl2Ang[hit->at(j).trid-1] = acos(hit->at(j).pz/hit->at(j).p) * 180/pi;
	afterColl2P[hit->at(j).trid-1] = hit->at(j).p;
      }

      if(hit->at(j).z <= 21999 || hit->at(j).z>22001) continue;

      if(rate>1e10) continue;//this cut is not understandable ... there is some difference between YZ output and mine where rates >1e7 screw up the results

      //select only e- and pi-
      if(hit->at(j).pid!=11 && hit->at(j).pid!=-211) continue;

      //make sure this is the detector you want
      if(hit->at(j).det != 28) continue;

      //this will ensure that we process only one hit from a particular track per event
      if( find(procID.begin(),procID.end(), hit->at(j).trid) != procID.end() ) continue;
      procID.push_back(hit->at(j).trid);

      if(isnan(rate) || isinf(rate)) continue;
      
      if(hit->at(j).r < 500) continue;

      double phi = atan2(hit->at(j).y,hit->at(j).x);
      if(phi<0) phi+=2*pi;
      int foundRing = findDetector(sector, phi, hit->at(j).r);
      if(foundRing==-1) continue;

      ///HACK Cut to select one septant .. WARNING!! do not use in regular analysis
      //if(!(phi>=2*pi/7*6 && phi<2*pi/7*7)) continue;

      double xx = bm->x;
      double yy = bm->y;
      double gRate = rate/1e9;
      xRateAll -> Fill(xx, gRate);
      if(foundRing == 4){
	beamRaster -> Fill(xx,yy);
	rBeamRaster -> Fill(xx,yy,gRate);
	xRate -> Fill(xx, gRate);

	if( xx > -2.5 && xx < -2.0)
	  lR->Fill(gRate);
	else if(xx > 2.0 && xx < 2.5)
	  rR->Fill(gRate);
      }


      r->Fill(hit->at(j).r);
      rRate->Fill(hit->at(j).r,rate);
      rRateAsym->Fill(hit->at(j).r,rate*asym);
      if(foundRing == 4)
	eRateAsym->Fill(hit->at(j).e,rate*asym);
      sourceZ->Fill(hit->at(j).vz);
      hXY->Fill(hit->at(j).x,hit->at(j).y);
      hXYrate->Fill(hit->at(j).x,hit->at(j).y,rate);
      hXYrateAsym->Fill(hit->at(j).x,hit->at(j).y,rate*asym);

      int hitRing5=0;
      if(foundRing == 4) hitRing5=1;

      hAsym[foundRing][sector]->Fill(asym,rate);
      hRate->SetBinContent(foundRing*3+sector+1,
			   rate + hRate->GetBinContent(foundRing*3+sector+1));
      
      if(hit->at(j).trid==1 && scatAng[0]!=-1){
	hVtxAngR -> Fill(hit->at(j).r,scatAng[0]);
	hVtxAngRrate -> Fill(hit->at(j).r,scatAng[0],rate);
	hVtxER -> Fill(hit->at(j).r,scatP[0]);
	hVtxERrate -> Fill(hit->at(j).r,scatP[0],rate);
	if(hitRing5){
	  hVtxAngE -> Fill(scatAng[0],scatP[0]);
	  hVtxAngErate -> Fill(scatAng[0],scatP[0],rate);
	}
      }else if(hit->at(j).trid==2 && scatAng[1]!=-1){
	hVtxAngR -> Fill(hit->at(j).r,scatAng[1]);
	hVtxAngRrate -> Fill(hit->at(j).r,scatAng[1],rate);
	hVtxER -> Fill(hit->at(j).r,scatP[1]);
	hVtxERrate -> Fill(hit->at(j).r,scatP[1],rate);
	if(hitRing5){
	  hVtxAngE -> Fill(scatAng[1],scatP[1]);
	  hVtxAngErate -> Fill(scatAng[1],scatP[1],rate);
	}
      }

      if(hit->at(j).trid==1 && afterColl2Ang[0]!=-1){
	hAfterColl2AngR -> Fill(hit->at(j).r,afterColl2Ang[0]);
	hAfterColl2AngRrate -> Fill(hit->at(j).r,afterColl2Ang[0],rate);
	hAfterColl2ER -> Fill(hit->at(j).r,afterColl2P[0]);
	hAfterColl2ERrate -> Fill(hit->at(j).r,afterColl2P[0],rate);
	if(hitRing5){
	  hAfterColl2AngE -> Fill(scatAng[0],afterColl2P[0]);
	  hAfterColl2AngErate -> Fill(scatAng[0],afterColl2P[0],rate);
	}
      }else if(hit->at(j).trid==2 && afterColl2Ang[1]!=-1){
	hAfterColl2AngR -> Fill(hit->at(j).r,afterColl2Ang[1]);
	hAfterColl2AngRrate -> Fill(hit->at(j).r,afterColl2Ang[1],rate);
	hAfterColl2ER -> Fill(hit->at(j).r,afterColl2P[1]);
	hAfterColl2ERrate -> Fill(hit->at(j).r,afterColl2P[1],rate);
	if(hitRing5){
	  hAfterColl2AngE -> Fill(scatAng[1],afterColl2P[1]);
	  hAfterColl2AngErate -> Fill(scatAng[1],afterColl2P[1],rate);
	}
      }
	
    }
  }
  fin->Close();
  delete fin;
  return nEntries;
};
    

void writeOutput(){
  fout->cd();
  for(int i=0;i<6;i++)
    for(int j=0;j<3;j++){
      hAsym[i][j]->Scale(1./nFiles);
      hAsym[i][j]->Write();
    }
  hRate->Scale(1./nFiles);
  hRate->Write();

  fout->cd("QA");
  r->Write();
  hScatAngP1->Write();
  hScatAngP2->Write();

  rRate->Scale(1./nFiles);
  rRateAsym->Scale(1./nFiles);
  eRateAsym->Scale(1./nFiles);
  rRate->Write();
  rRateAsym->Write();
  eRateAsym->Write();

  hVtxAngR->Write();

  hVtxAngRrate->Scale(1./nFiles);
  hVtxAngRrate->Write();
  
  hAfterColl2AngR->Write();

  hAfterColl2AngRrate->Scale(1./nFiles);
  hAfterColl2AngRrate->Write();

  hVtxER->Write();

  hVtxERrate->Scale(1./nFiles);
  hVtxERrate->Write();

  hAfterColl2ER->Write();

  hAfterColl2ERrate->Scale(1./nFiles);
  hAfterColl2ERrate->Write();

  hVtxAngE->Write();

  hVtxAngErate->Scale(1./nFiles);
  hVtxAngErate->Write();

  hAfterColl2AngE->Write();

  hAfterColl2AngErate->Scale(1./nFiles);
  hAfterColl2AngErate->Write();

  hXY->Write();

  hXYrate->Scale(1./nFiles);
  hXYrate->Write();

  hXYrateAsym->Scale(1./nFiles);
  hXYrateAsym->Write();

  sourceZ->Write();

  beamRaster->Write();

  rBeamRaster->Scale(1./nFiles);
  rBeamRaster->Write();

  xRate->Scale(1./nFiles);
  xRate->Write();

  xRateAll->Scale(1./nFiles);
  xRateAll->Write();

  leftRate->Write();
  rightRate->Write();
  
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
  const int nRings = 6;
  const int nSec = 3;
  const double rMin[nRings][nSec]={
    { 640.0,  640.0,  640.0},
    { 680.0,  680.0,  680.0},
    { 730.0,  730.0,  730.0},
    { 805.0,  827.5,  835.0},
    { 855.0,  900.0,  915.0},
    {1070.0, 1060.0, 1055.0}
  };
  const double rMax[nRings][nSec]={
    { 680.0,  680.0,  680.0},
    { 730.0,  730.0,  730.0},
    { 805.0,  827.5,  835.0},
    { 855.0,  900.0,  915.0},
    {1070.0, 1060.0, 1055.0},
    {1170.0, 1170.0, 1170.0}
  };

  // if ever you want to recover this it has to be reorganized
  // const double rMin[8]={690, 730, 780, 855,  900,  855,  915, 1070};//initial CG estimation
  // const double rMax[8]={730, 780, 855, 930, 1060, 1070, 1055, 1200};
  // const double rMin[8]={0.690, 0.730, 0.780, 0.855, 0.935, 0.960, 0.960, 1.100};
  // const double rMax[8]={0.730, 0.780, 0.855, 0.930, 1.040, 1.075, 1.100, 1.200};
  //const int region2ring[8]={0,1,2,3,4,4,4,5};

  for(int i=0;i<nRings;i++)
    if( r >= rMin[i][sector] + offsetR && r <= rMax[i][sector] + offsetR)
      return i;
  
  return -1;
}

