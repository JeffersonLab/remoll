// Run commands in this order:
// 
// //Start reroot
// >build/reroot
//
// //Load in the script, and run it
// >.L analysis/basicAna_carynbellows.C
// > basicAna(<remoll output file>)
//OR to auto loop through list in particular directory below...
// autoloop for particular detector 28,70,71,72,73,74,75,76
// > basicAna("",70)


TFile *fout;
vector<vector<TH1D*>> hAsym;
TH1D *hRate,*rRate, *rRateAsym,*r,*sourceZ,*eRateAsym;
TH2D *hXY,*hXYrate, *hXYrateAsym, *hetheta, *hethetaRate,*hetheta_cut15, *hethetaRate_cut15;
TH1D *hE,*hERate, *hradius, *hradiusRate, *hphi, *hphiRate, *hthetaP, *hthetaPRate;
TH2D *hzfront, *hzback, *hrin, *hrout; 
TH2D *hzfrontRate, *hzbackRate, *hrinRate, *hroutRate; 
TH2D *hethetaVSz,*hethetaVSz_cut15, *heVSz, *heVSr;

TH1D *hRate_ecut,*rRate_ecut, *rRateAsym_ecut,*r_ecut,*eRateAsym_ecut;
TH2D *hXY_ecut,*hXYrate_ecut, *hXYrateAsym_ecut, *hetheta_ecut, *hethetaRate_ecut,*hetheta_cut15ecut, *hethetaRate_cut15ecut;
TH1D *hE_ecut,*hERate_ecut, *hradius_ecut, *hradiusRate_ecut, *hphi_ecut, *hphiRate_ecut, *hthetaP_ecut, *hthetaPRate_ecut;
TH2D *hzfront_ecut, *hzback_ecut, *hrin_ecut, *hrout_ecut; 
TH2D *hzfrontRate_ecut, *hzbackRate_ecut, *hrinRate_ecut, *hroutRate_ecut; 
TH2D *hethetaVSz_ecut,*hethetaVSz_cut15ecut, *heVSz_ecut, *heVSr_ecut;

string fin;
int detnum;

double z1,z2,r1,r2,Emax;
//      double z1=-2972.48;
//      double z2=-2647.6;
//      double r1=100;
//      double r2=149;


int nFiles(0);

long currentEvNr(0);

float offsetR(0),offsetPhi(0);
const double pi = acos(-1);

void initHisto();
long processOne(string);
void process();
int findDetector(int &sector, double phi, double r);
void writeOutput();

void basicAna(const string& finName = "./remollout.root", int detectornumber = 70){
  fin = finName;
  detnum = detectornumber;
  if(detnum==28){
    z1=22000;
    z2=22000;
    r1=0.0;
    r2=1900-1;
    Emax=11000;
  }
  if(detnum==70){//b1
    z1=-2972.48; //1690.00-4500.0 -325/2 -0.48
    z2=-2647.6;  //1690.00-4500.0 +325/2 +0.4
    r1=100;
    r2=149;
    Emax=6000;
  }
  if(detnum==71){//b2
    z1=4087.00-4500.0-380/2+1;  //<z1 front face
    z2=4087.00-4500.0+380/2-1;  //>z2 back face
    r1=9.8*25.4/2+1;//<r1 inner ring
    r2=9.8*25.4/2+50-1;//>r2 outer ring
    Emax=1500;
  }
  if(detnum==72){//b3
    z1=8946.00-4500.0-508/2+1;//<z1 front face
    z2=8946.00-4500.0+508/2-1;//>z2 back face
    r1=25.8*25.4/2+1;//<r1 inner ring
    r2=25.8*25.4/2+50-1;//>r2 outer ring
  }
  if(detnum==73){//b4
    z1=16900.00-4500.0-150/2+1;//<z1 front face
    z2=16900.00-4500.0+150/2-1;//>z2 back face
    r1=51*25.4/2+1;//<r1 inner ring
    r2=51*25.4/2+50-1;//>r2 outer ring
    Emax=1500;
  }
  if(detnum==74){//b5
    z1=23430.00-4500.0-120/2+1;//<z1 front face
    z2=23430.00-4500.0+120/2-1;//>z2 back face
    r1=39.8*25.4/2+1;//<r1 inner ring
    r2=39.8*25.4/2+50-1;//>r2 outer ring
    Emax=1500;
  }
  if(detnum==75){//b6
    z1=24280.00-4500.0-120/2+1;//<z1 front face
    z2=24280.00-4500.0+120/2-1;//>z2 back face
    r1=41.6*25.4/2+1;//<r1 inner ring
    r2=41.6*25.4/2+50-1;//>r2 outer ring
    Emax=1500;
  }
  if(detnum==76){///b7
    z1=30160.00-4500.0-570/2+1;//<z1 front face
    z2=30160.00-4500.0+570/2-1;//>z2 back face
    r1=16*25.4/2+1;//<r1 inner ring
    r2=16*25.4/2+50-1;//>r2 outer ring
    Emax=1500;
  }
  initHisto();
  process();
  writeOutput();
}

void process(){

  long nTotEv(0);
  if(fin==""){
    // cout<<"\t did not find input file. Quitting!"<<endl;
    //    return 2;
     string data;
     for(int ii=1; ii<1001;ii++){
       if(ii<10){
	 data = Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_100M/remoll_bellows_beam_100M_100kEv_00%d/remollout.root",ii);
	 //	 data = "copyremollout.root";
       }
       if(ii>9&&ii<100){
      	 data = Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_100M/remoll_bellows_beam_100M_100kEv_0%d/remollout.root",ii);
	 //	 data = "copyremollout.root";
       }
       if(ii>99){
	  data = Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_100M/remoll_bellows_beam_100M_100kEv_%d/remollout.root",ii);
	  //data = "copyremollout.root";
       }
          cout<<" processing: "<<data<<endl;
      	  nTotEv+=processOne(data);
	  nFiles++;
     }
  }

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
  string foutNm = Form("det%d_%s_carynplots.root",detnum,fin.substr(0,fin.find(".")).c_str());

  fout = new TFile(foutNm.c_str(),"RECREATE");


  fout->mkdir("QA","quality assurance plots");
  fout->cd("QA");
  r = new TH1D("r","radial distribution;r[m]",200,r1-10,r2+10);
  rRate = new TH1D("rRate","rate weighted distribution;r[m]",200,r1-10,r2+10);
  rRateAsym = new TH1D("rRateAsym","rate*Asym weighted distribution;r[m]",200,r1-10,r2+10);
  hXY = new TH2D("hXY","2D hit ditribution;x [m];y [m]",200,-2100,2100,200,-2100,2100);
  hXYrate = new TH2D("hXYrate","rate weighted 2D hit ditribution;x [m];y [m]",200,-2100,2100,200,-2100,2100);
    hE = new TH1D("hE","Energy distribution;E",Emax,0,Emax);
    hERate = new TH1D("hERate","rate weighteed Energy distribution;E",Emax,0,Emax);
    hphi = new TH1D("hphi","phi distribution;phi[deg]",1000,-180,180);
    hphiRate = new TH1D("hphiRate","rate weighted phi distribution;phi[deg]",1000,-180,180);
    hthetaP = new TH1D("hthetaP","scat angle theta distribution;theta[deg]",1000,-180,180);
    hthetaPRate = new TH1D("hthetaPRate","rate weighted scat angle theta distribution;theta[deg]",1000,-180,180);
    hradius = new TH1D("hradius","radius distribution;r[m]",1000,r1-10,r2+10);
    hradiusRate = new TH1D("hradiusRate","rate weighted radius distribution;r[m]",1000,r1-10,r2+10);
    //  hetheta = new TH2D("hetheta","2D hit ditribution;x [m];y [m]",200,-2100,2100,200,-2100,2100);
    //  hethetaRate = new TH2D("hethetaRate","rate weighted 2D hit ditribution;x [m];y [m]",200,-2100,2100,200,-2100,2100);
    hetheta = new TH2D("hetheta","2D energy-scat angle theta ditribution;E ;theta [deg]",200,0,Emax,200,-180,180);
    hethetaRate = new TH2D("hethetaRate","rate weighted 2D energy-scat angle theta  ditribution;E ;theta [deg]",200,0,Emax,200,-180,180);
    hetheta_cut15 = new TH2D("hetheta_cut15","2D energy-scat angle theta ditribution;E ;theta [deg]",200,0,Emax,200,0,15);
    hethetaRate_cut15 = new TH2D("hethetaRate_cut15","rate weighted 2D energy-scat angle theta  ditribution;E ;theta [deg]",200,0,Emax,200,0,15);


    hzfront=new TH2D("hzfront","frontface distribution;x[m];y[m]",200,-r2-10,r2+10,200,-r2-10,r2+10);
    hzback=new TH2D("hzback","backface distribution;x[m];y[m]",200,-r2-10,r2+10,200,-r2-10,r2+10);
    hrin=new TH2D("hrin","inner radius distribution;z[m];phi[deg]",200,z1,z2,200,-180,180);
    hrout=new TH2D("hrout","outer radius distribution;z[m];phi[deg]",200,z1,z2,200,-180,180);

    hzfrontRate=new TH2D("hzfrontRate","rate weighted frontface distribution;x[m];y[m]",200,-r2-10,r2+10,200,-r2-10,r2+10);
    hzbackRate=new TH2D("hzbackRate","rate weighted backface distribution;x[m];y[m]",200,-r2-10,r2+10,200,-r2-10,r2+10);
    hrinRate=new TH2D("hrinRate","rate weighted inner radius distribution;z[m];phi[deg]",200,z1,z2,200,-180,180);
    hroutRate=new TH2D("hroutRate","rate weighted outer radius distribution;z[m];phi[deg]",200,z1,z2,200,-180,180);

    hethetaVSz = new TH2D("hethetaVSz","2D z-scat angle theta ditribution vs z;z[m] ;theta [deg]",200,z1-10,z2+10,200,-180,180);
    hethetaVSz_cut15 = new TH2D("hethetaVSz_cut15","2D z-scat angle theta ditribution vs z;z[m] ;theta [deg]",200,z1-10,z2+10,200,0,15);

    heVSz = new TH2D("heVSz","2D energy ditribution vs z;z[m] ;E",200,z1-10,z2+10,200,0,Emax);
    heVSr = new TH2D("heVSr","2D energy ditribution vs r;r[m] ;E",200,r1-10,r2+10,200,0,Emax);



    //cut on E>10MeV
  r_ecut = new TH1D("r_ecut","radial distribution >10MeV;r[m]",200,r1-10,r2+10);
  rRate_ecut = new TH1D("rRate_ecut","rate weighted distribution >10MeV;r[m]",200,r1-10,r2+10);
  rRateAsym_ecut = new TH1D("rRateAsym_ecut","rate*Asym weighted distribution >10MeV;r[m]",200,r1-10,r2+10);
  hXY_ecut = new TH2D("hXY_ecut","2D hit ditribution >10MeV;x [m];y [m]",200,-2100,2100,200,-2100,2100);
  hXYrate_ecut = new TH2D("hXYrate_ecut","rate weighted 2D hit ditribution >10MeV;x [m];y [m]",200,-2100,2100,200,-2100,2100);
    hE_ecut = new TH1D("hE_ecut","Energy distribution >10MeV;E",Emax,10,Emax);
    hERate_ecut = new TH1D("hERate_ecut","rate weighteed Energy distribution >10MeV;E",Emax,10,Emax);
    hphi_ecut = new TH1D("hphi_ecut","phi distribution >10MeV;phi[deg]",1000,-180,180);
    hphiRate_ecut = new TH1D("hphiRate_ecut","rate weighted phi distribution >10MeV;phi[deg]",1000,-180,180);
    hthetaP_ecut = new TH1D("hthetaP_ecut","scat angle theta distribution >10MeV;theta[deg]",1000,-180,180);
    hthetaPRate_ecut = new TH1D("hthetaPRate_ecut","rate weighted scat angle theta distribution >10MeV;theta[deg]",1000,-180,180);
    hradius_ecut = new TH1D("hradius_ecut","radius distribution >10MeV;r[m]",1000,r1-10,r2+10);
    hradiusRate_ecut = new TH1D("hradiusRate_ecut","rate weighted radius distribution >10MeV;r[m]",1000,r1-10,r2+10);
    hetheta_ecut = new TH2D("hetheta_ecut","2D energy-scat angle theta ditribution >10MeV;E ;theta [deg]",200,10,Emax,200,-180,180);
    hethetaRate_ecut = new TH2D("hethetaRate_ecut","rate weighted 2D energy-scat angle theta  ditribution >10MeV;E ;theta [deg]",200,10,Emax,200,-180,180);
    hetheta_cut15ecut = new TH2D("hetheta_cut15ecut","2D energy-scat angle theta ditribution >10MeV;E ;theta [deg]",200,10,Emax,200,0,15);
    hethetaRate_cut15ecut = new TH2D("hethetaRate_cut15ecut","rate weighted 2D energy-scat angle theta  ditribution >10MeV;E ;theta [deg]",200,10,Emax,200,0,15);


    hzfront_ecut=new TH2D("hzfront_ecut","frontface distribution >10MeV;x[m];y[m]",200,-r2-10,r2+10,200,-r2-10,r2+10);
    hzback_ecut=new TH2D("hzback_ecut","backface distribution >10MeV;x[m];y[m]",200,-r2-10,r2+10,200,-r2-10,r2+10);
    hrin_ecut=new TH2D("hrin_ecut","inner radius distribution >10MeV;z[m];phi[deg]",200,z1,z2,200,-180,180);
    hrout_ecut=new TH2D("hrout_ecut","outer radius distribution >10MeV;z[m];phi[deg]",200,z1,z2,200,-180,180);

    hzfrontRate_ecut=new TH2D("hzfrontRate_ecut","rate weighted frontface distribution >10MeV;x[m];y[m]",200,-r2-10,r2+10,200,-r2-10,r2+10);
    hzbackRate_ecut=new TH2D("hzbackRate_ecut","rate weighted backface distribution >10MeV;x[m];y[m]",200,-r2-10,r2+10,200,-r2-10,r2+10);
    hrinRate_ecut=new TH2D("hrinRate_ecut","rate weighted inner radius distribution >10MeV;z[m];phi[deg]",200,z1,z2,200,-180,180);
    hroutRate_ecut=new TH2D("hroutRate_ecut","rate weighted outer radius distribution >10MeV;z[m];phi[deg]",200,z1,z2,200,-180,180);

    hethetaVSz_ecut = new TH2D("hethetaVSz_ecut","2D z-scat angle theta ditribution vs z >10Mev;z[m] ;theta [deg]",200,z1-10,z2+10,200,-180,180);
    hethetaVSz_cut15ecut = new TH2D("hethetaVSz_cut15ecut","2D z-scat angle theta ditribution vs z >10MeV;z[m] ;theta [deg]",200,z1-10,z2+10,200,0,15);

    heVSz_ecut = new TH2D("heVSz_ecut","2D energy ditribution vs z >10MeV;z[m] ;E",200,z1-10,z2+10,200,10,Emax);
    heVSr_ecut = new TH2D("heVSr_ecut","2D energy ditribution vs r >10MeV;r[m] ;E",200,r1-10,r2+10,200,10,Emax);



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
  float currentProc=1,procStep=10;
  vector<int> procID;
  int sector(-1);
  double pi = acos(-1);

  double myr;
  double mytheta;
  double mythetaP;
  double myphi;

  for (Long64_t event = 0; event < nEntries; t->GetEntry(event++)) {
    currentEvNr++;
    if( float(event+1)/nEntries*100 > currentProc){
      cout<<"at tree entry\t"<<event<<"\t"<< float(event+1)/nEntries*100<<endl;
      currentProc+=procStep;
    }

    double asym = ev->A;

    //        double scatAng[2]={-1,-1};
	//        double scatP[2]={-1,-1};

	//    if(part){
      //      for(int j=0;j<part->size();j++){
	//	if(j>2){
	  //	  cout<<"Event npart >2 skipping this event:\n"
	    //	      <<part->at(j).p<<"\t"<<part->at(j).pz<<"\t"<<part->at(j).pid<<endl;
	  //	  break;
	  //	}
	//	if(part->at(j).pid!=11) continue;
	//		scatAng[j] = acos(part->at(j).pz/part->at(j).p) * 180/pi;
		//		scatP[j] = part->at(j).p;
		//      }
      //    }

    procID.clear();

    for(int j=0;j<hit->size();j++){

      //      if(rate>1e10) continue;//this cut is not understandable ... there is some difference between YZ output and mine where rates >1e7 screw up the results

      //select only e- and pi-
      //      if(hit->at(j).pid!=11 && hit->at(j).pid!=-211) continue;
      //select only e-
      if(hit->at(j).pid!=11) continue;

      //make sure this is the detector you want 70
      if(hit->at(j).det != detnum) continue;

      //this will ensure that we process only one hit from a particular track per event
      if( find(procID.begin(),procID.end(), hit->at(j).trid) != procID.end() ) continue;
            procID.push_back(hit->at(j).trid);

      r->Fill(hit->at(j).r);
      rRate->Fill(hit->at(j).r,rate);
      rRateAsym->Fill(hit->at(j).r,rate*asym);
      hXY->Fill(hit->at(j).x,hit->at(j).y);
      hXYrate->Fill(hit->at(j).x,hit->at(j).y,rate);

      hE->Fill(hit->at(j).e);     	
      hERate->Fill(hit->at(j).e,rate);     	

      //get angles
      //=(180/PI())*ACOS(x/SQRT(x^2+y^2))*y/ABS(y)
      //      myr = sqrt(pow(hit->at(j).x,2)+pow(hit->at(j).y,2)+pow(hit->at(j).z,2));
      //      mytheta = acos(hit->at(j).z/myr) *(hit->at(j).z/abs(hit->at(j).z))* 180/pi;
      myr = hit->at(j).r;// (just x,y)
      myphi = hit->at(j).ph *180/pi;//acos(hit->at(j).x/sqrt(pow(hit->at(j).x,2)+pow(hit->at(j).y,2))) * ((hit->at(j).y)/abs(hit->at(j).y))*180/pi;
      mythetaP = acos(hit->at(j).pz/hit->at(j).p)*(hit->at(j).pz/abs(hit->at(j).pz)) * 180/pi;

      //      thetaPvsE;

      hphi->Fill(myphi);
      hphiRate->Fill(myphi,rate);

      hthetaP->Fill(mythetaP);
      hthetaPRate->Fill(mythetaP,rate);

      hradius->Fill(myr);
      hradiusRate->Fill(myr,rate);

      hetheta->Fill(hit->at(j).e,mythetaP);
      hethetaRate->Fill(hit->at(j).e,mythetaP,rate);
      //      hetheta->Fill(hit->at(j).x,hit->at(j).y);
      //      hethetaRate->Fill(hit->at(j).x,hit->at(j).y,rate);
      hethetaVSz->Fill(hit->at(j).z,mythetaP);
      if(mythetaP<15&&mythetaP>0){
	hetheta_cut15->Fill(hit->at(j).e,mythetaP);
	hethetaRate_cut15->Fill(hit->at(j).e,mythetaP,rate);
	hethetaVSz_cut15->Fill(hit->at(j).z,mythetaP);
      }
      heVSz->Fill(hit->at(j).z,hit->at(j).e);
      heVSr->Fill(hit->at(j).r,hit->at(j).e);


      //radial cut and z cut for each detector 
      //      if(hit->at(j).det != 70) continue;
      //y vs x front face (z<z1)
      if(hit->at(j).z < z1) {
	//zfront
	hzfront->Fill(hit->at(j).x,hit->at(j).y);
	hzfrontRate->Fill(hit->at(j).x,hit->at(j).y,rate);
      }
      //back face (z>z2)
      if(hit->at(j).z > z2) {
	//zback
	hzback->Fill(hit->at(j).x,hit->at(j).y);
	hzbackRate->Fill(hit->at(j).x,hit->at(j).y,rate);
      }
      //innner ring (r<r1 && z>z1 &&z<z2)
      if((hit->at(j).r<r1)&&(hit->at(j).z>z1)&&(hit->at(j).z<z2)) {
      //phi vs z ring face
	//rin
	hrin->Fill(hit->at(j).z,myphi);
	hrinRate->Fill(hit->at(j).z,myphi,rate);
      }
     //outer ring (r>r2 && z>z1 &&z<z2)
      if((hit->at(j).r>r2)&&(hit->at(j).z>z1)&&(hit->at(j).z<z2)) {
      //phi vs z ring face
	//rout
	hrout->Fill(hit->at(j).z,myphi);
	hroutRate->Fill(hit->at(j).z,myphi,rate);
      }

      //10MeV cut
      if(hit->at(j).e < 10) continue;

      r_ecut->Fill(hit->at(j).r);
      rRate_ecut->Fill(hit->at(j).r,rate);
      rRateAsym_ecut->Fill(hit->at(j).r,rate*asym);
      hXY_ecut->Fill(hit->at(j).x,hit->at(j).y);
      hXYrate_ecut->Fill(hit->at(j).x,hit->at(j).y,rate);

      hE_ecut->Fill(hit->at(j).e);     	
      hERate_ecut->Fill(hit->at(j).e,rate);     	

      //get angles
      myr = hit->at(j).r;// (just x,y)
      myphi = hit->at(j).ph *180/pi;//acos(hit->at(j).x/sqrt(pow(hit->at(j).x,2)+pow(hit->at(j).y,2))) * ((hit->at(j).y)/abs(hit->at(j).y))*180/pi;
      mythetaP = acos(hit->at(j).pz/hit->at(j).p)*(hit->at(j).pz/abs(hit->at(j).pz)) * 180/pi;

      hphi_ecut->Fill(myphi);
      hphiRate_ecut->Fill(myphi,rate);

      hthetaP_ecut->Fill(mythetaP);
      hthetaPRate_ecut->Fill(mythetaP,rate);

      hradius_ecut->Fill(myr);
      hradiusRate_ecut->Fill(myr,rate);

      hetheta_ecut->Fill(hit->at(j).e,mythetaP);
      hethetaRate_ecut->Fill(hit->at(j).e,mythetaP,rate);
      hethetaVSz_ecut->Fill(hit->at(j).z,mythetaP);
      if(mythetaP<15&&mythetaP>0){
	hetheta_cut15ecut->Fill(hit->at(j).e,mythetaP);
	hethetaRate_cut15ecut->Fill(hit->at(j).e,mythetaP,rate);
	hethetaVSz_cut15ecut->Fill(hit->at(j).z,mythetaP);

      }
      heVSz_ecut->Fill(hit->at(j).z,hit->at(j).e);
      heVSr_ecut->Fill(hit->at(j).r,hit->at(j).e);


      //radial cut and z cut for each detector 
      //y vs x front face (z<z1)
      if(hit->at(j).z < z1) {
	//zfront
	hzfront_ecut->Fill(hit->at(j).x,hit->at(j).y);
	hzfrontRate_ecut->Fill(hit->at(j).x,hit->at(j).y,rate);
      }
      //back face (z>z2)
      if(hit->at(j).z > z2) {
	//zback
	hzback_ecut->Fill(hit->at(j).x,hit->at(j).y);
	hzbackRate_ecut->Fill(hit->at(j).x,hit->at(j).y,rate);
      }
      //innner ring (r<r1 && z>z1 &&z<z2)
      if((hit->at(j).r<r1)&&(hit->at(j).z>z1)&&(hit->at(j).z<z2)) {
      //phi vs z ring face
	//rin
	hrin_ecut->Fill(hit->at(j).z,myphi);
	hrinRate_ecut->Fill(hit->at(j).z,myphi,rate);
      }
     //outer ring (r>r2 && z>z1 &&z<z2)
      if((hit->at(j).r>r2)&&(hit->at(j).z>z1)&&(hit->at(j).z<z2)) {
      //phi vs z ring face
	//rout
	hrout_ecut->Fill(hit->at(j).z,myphi);
	hroutRate_ecut->Fill(hit->at(j).z,myphi,rate);
      }



    }
  }
  fin->Close();
  delete fin;
  return nEntries;
};
    

void writeOutput(){
  fout->cd();

  fout->cd("QA");
  r->Write();

  rRate->Scale(1./nFiles);
  rRateAsym->Scale(1./nFiles);
  rRate->Write();
  rRateAsym->Write();

  hXY->Write();

  hXYrate->Scale(1./nFiles);
  hXYrate->Write();
  
  hE->Write();

  hERate->Scale(1./nFiles);
  hERate->Write();
  hphi->Write();
  hphiRate->Scale(1./nFiles);
  hphiRate->Write();
  hthetaP->Write();
  hthetaPRate->Scale(1./nFiles);
  hthetaPRate->Write();
  hradius->Write();
  hradiusRate->Scale(1./nFiles);
  hradiusRate->Write();
  hetheta->Write();
  hethetaRate->Scale(1./nFiles);
  hethetaRate->Write();

  hetheta_cut15->Write();
  hethetaRate_cut15->Scale(1./nFiles);
  hethetaRate_cut15->Write();

  hethetaVSz->Write();
  hethetaVSz_cut15->Write();

  heVSz->Write();
  heVSr->Write();

  hzfront->Write();
  hzfrontRate->Scale(1./nFiles);
  hzfrontRate->Write();

  hzback->Write();
  hzbackRate->Scale(1./nFiles);
  hzbackRate->Write();

  hrin->Write();
  hrinRate->Scale(1./nFiles);
  hrinRate->Write();

  hrout->Write();
  hroutRate->Scale(1./nFiles);
  hroutRate->Write();

  //>10MeV cut
  r_ecut->Write();

  rRate_ecut->Scale(1./nFiles);
  rRateAsym_ecut->Scale(1./nFiles);
  rRate_ecut->Write();
  rRateAsym_ecut->Write();

  hXY_ecut->Write();

  hXYrate_ecut->Scale(1./nFiles);
  hXYrate_ecut->Write();
  
  hE_ecut->Write();

  hERate_ecut->Scale(1./nFiles);
  hERate_ecut->Write();
  hphi_ecut->Write();
  hphiRate_ecut->Scale(1./nFiles);
  hphiRate_ecut->Write();
  hthetaP_ecut->Write();
  hthetaPRate_ecut->Scale(1./nFiles);
  hthetaPRate_ecut->Write();
  hradius_ecut->Write();
  hradiusRate_ecut->Scale(1./nFiles);
  hradiusRate_ecut->Write();
  hetheta_ecut->Write();
  hethetaRate_ecut->Scale(1./nFiles);
  hethetaRate_ecut->Write();

  hetheta_cut15ecut->Write();
  hethetaRate_cut15ecut->Scale(1./nFiles);
  hethetaRate_cut15ecut->Write();

  hethetaVSz_ecut->Write();
  hethetaVSz_cut15ecut->Write();

  heVSz_ecut->Write();
  heVSr_ecut->Write();

  hzfront_ecut->Write();
  hzfrontRate_ecut->Scale(1./nFiles);
  hzfrontRate_ecut->Write();

  hzback_ecut->Write();
  hzbackRate_ecut->Scale(1./nFiles);
  hzbackRate_ecut->Write();

  hrin_ecut->Write();
  hrinRate_ecut->Scale(1./nFiles);
  hrinRate_ecut->Write();

  hrout_ecut->Write();
  hroutRate_ecut->Scale(1./nFiles);
  hroutRate_ecut->Write();


  fout->Close();
}

