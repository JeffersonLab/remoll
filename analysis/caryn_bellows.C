void caryn_bellows(int detnum=70){

  //  int runnum=1;

  int Nruns=2;//1000;
  //  int detnum=70;

    int Nmax=10000; //max number of tracks stored per detector per run


 TString filename="/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_100M/remoll_bellows_beam_updateftsd_100M_100kEv";
  TFile *_file0;


 //  char outfilename[255];
 //  sprintf(outfilename,Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_100M/remoll_electrons_det%d.txt",detnum));
 //  printf("writing output to %s\n",outfilename);

 TString outfilename = Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_100M/remoll_electrons_updateftsd_det%d.txt",detnum);
 cout<<"writing output to "<<outfilename.Data()<<endl;
  ofstream outfile;
  outfile.open(outfilename);
  //  outfile<<"test"<<endl;
  //only saving tracks that aren't going through the detector a second time to prevent redundancy

	// outfile<<"run"<<" ";
	// outfile<<"det"<<" ";
	// outfile<<"Entry$"<<" ";
	// outfile<<"track"<<" ";
	// outfile<<"x"<<" ";
	// outfile<<"y"<<" ";
	// outfile<<"z"<<" ";
	// outfile<<"px"<<" ";
	// outfile<<"py"<<" ";
	// outfile<<"pz"<<endl;;


  for(int runnum=1;runnum<Nruns+1;runnum++){

    //TString filename="/volatile/halla/moller12gev/palatchi/remoll_bellows_beam/remoll_bellows_beam_100kEv";
    ///volatile/halla/moller12gev/palatchi/remoll_bellows_beam_100M/remoll_bellows_beam_100M_100kEv_001/remollout.root


  if(runnum<10){
    _file0 = TFile::Open(Form("%s_00%d/remollout.root",filename.Data(),runnum));
    cout<<Form("%s_00%d/remollout.root",filename.Data(),runnum)<<endl;
  }
  if(runnum>9&&runnum<100){
    _file0 = TFile::Open(Form("%s_0%d/remollout.root",filename.Data(),runnum));
    cout<<Form("%s_0%d/remollout.root",filename.Data(),runnum)<<endl;
  }
  if(runnum>99){
    _file0 = TFile::Open(Form("%s_%d/remollout.root",filename.Data(),runnum));
    cout<<Form("%s_0%d/remollout.root",filename.Data(),runnum)<<endl;
  }
 


    TTree *tree = (TTree*)_file0->Get("T");
    TH1D *h;


    double vebellows[Nmax];
    double x[Nmax];
    double y[Nmax];
    double z[Nmax];
    double ventrybellows[Nmax];
    double t[Nmax];
    double px[Nmax];
    double py[Nmax];
    double pz[Nmax];
    double det[Nmax];
    double ventrybellows_short[Nmax];
    double vebellows_short[Nmax];
    double x_short[Nmax],y_short[Nmax],z_short[Nmax];
    double px_short[Nmax],py_short[Nmax],pz_short[Nmax],det_short[Nmax];
    double t_short[Nmax];

    //create list of all electron tracks that hit 70
    //    tree->Draw("hit.trid>>hebellows","(hit.det>69&&hit.det<77)&&hit.pid==11");
    tree->Draw("hit.trid>>hebellows",Form("hit.det==%d&&hit.pid==11",detnum),"Q");
 
    h=(TH1D*)gDirectory->FindObject("hebellows");
    int Nebellows=h->GetEntries();
    ///    tree->Draw("hit.trid:hit.x:hit.y:hit.z","(hit.det>69&&hit.det<77)&&hit.pid==11");
    tree->Draw("Entry$:hit.t",Form("hit.det==%d&&hit.pid==11",detnum),"Q");
    double* pventrybellows = tree->GetV1();
    double* pvt = tree->GetV2();
    //    double vebellows[Nebellows];
    //    double x[Nebellows];
    //    double y[Nebellows];
    //    double z[Nebellows];
    //    double ventrybellows[Nebellows];
    //    double px[Nebellows];
    //    double py[Nebellows];
    //    double pz[Nebellows];
    //    double det[Nebellows];
    for(int ii=0;ii<Nebellows;ii++){
      ventrybellows[ii]=*(pventrybellows+ii);
      t[ii]=*(pvt+ii);
    }
    tree->Draw("hit.trid:hit.x:hit.y:hit.z",Form("hit.det==%d&&hit.pid==11",detnum),"Q");
    double* pvebellows = tree->GetV1();
   double* pvx = tree->GetV2();
   double* pvy = tree->GetV3();
   double* pvz = tree->GetV4();
    for(int ii=0;ii<Nebellows;ii++){
      vebellows[ii]=*(pvebellows+ii);
      x[ii]=*(pvx+ii);
      y[ii]=*(pvy+ii);
      z[ii]=*(pvz+ii);
    }
    tree->Draw("hit.px:hit.py:hit.pz:hit.det",Form("hit.det==%d&&hit.pid==11",detnum),"Q");
   double* pvpx = tree->GetV1();
   double* pvpy = tree->GetV2();
   double* pvpz = tree->GetV3();
   double* pvdet = tree->GetV4();
    for(int ii=0;ii<Nebellows;ii++){
      px[ii]=*(pvpx+ii);
      py[ii]=*(pvpy+ii);
      pz[ii]=*(pvpz+ii);
      det[ii]=*(pvdet+ii);
    }

    //    int Neaddbellows = Ne70+Ne71+Ne72+Ne73+Ne74+Ne75+Ne76;
   //create list of all electron tracks that hit 70-76 or main det 28
    // tree->Draw("hit.trid>>hebellows","(hit.det>69&&hit.det<77)&&hit.det==28&&hit.pid==11","Q");
 
    //    cout<<"N Ne Ne28 Ne70 Ne71 Ne72 Ne73 Ne74 Ne75 Ne76 Neaddbellows Nbellows"<<endl;
    //  cout<<N<<" "<<Ne<<" "<<Ne28<<" "<<Ne70<<" "<<Ne71<<" "<<Ne72<<" "<<Ne73<<" "<<Ne74<<" "<<Ne75<<" "<<Ne76<<" "<<Nebellows<<endl;

    cout<<"Nebellows "<<Nebellows<<endl;


    //Identify repeated tracks, cycle through names of tracks in vebellows list, add them to a shortlist if they aren't already included in the shortlist 
    //    double ventrybellows_short[Nebellows];
    //    double vebellows_short[Nebellows];
    //    double x_short[Nebellows],y_short[Nebellows],z_short[Nebellows];
    //    double px_short[Nebellows],py_short[Nebellows],pz_short[Nebellows],det_short[Nebellows];
    double ventrybellows_temp, vebellows_temp;
    double x_temp, y_temp, z_temp;
    double px_temp, py_temp, pz_temp, det_temp;
    double t_temp;
    int repeat;
    int count_short=0;
    int understand=0;
    for(int ii=0;ii<Nebellows;ii++){
      repeat=0;
      ventrybellows_temp = ventrybellows[ii];
      vebellows_temp = vebellows[ii];
      x_temp= x[ii];
      y_temp= y[ii];
      z_temp= z[ii];
      px_temp= px[ii];
      py_temp= py[ii];
      pz_temp= pz[ii];
      t_temp=t[ii];
      //      det_temp= det[ii];
      for(int jj=0;jj<count_short;jj++){
	//       	if((ventrybellows_temp==ventrybellows_short[jj])&&(vebellows_temp==vebellows_short[jj])&&(x_temp==x_short[jj])&&(y_temp==y_short[jj])&&(z_temp==z_short[jj])&&(px_temp==px_short[jj])&&(py_temp==py_short[jj])&&(pz_temp==pz_short[jj])){
       	if((ventrybellows_temp==ventrybellows_short[jj])&&(vebellows_temp==vebellows_short[jj])){

	//	if((x_temp==x_short[jj])&&(y_temp==y_short[jj])&&(z_temp==z_short[jj])&&(px_temp==px_short[jj])&&(py_temp==py_short[jj])&&(pz_temp==pz_short[jj])){	  
	  repeat=1;
	  if(abs(px_temp-px_short[jj])<1e-15&&abs(py_temp-py_short[jj])<1e-15&&abs(pz_temp-pz_short[jj])<1e-15){
	    //	    cout<<"same p: ";
	    understand=1;
	  }
	  if((pz_temp<0&&pz_short[jj]>0)){
	    //	    cout<<"back pz<0: ";
	    understand=1;
	  }
	  if((pz_temp>0&&pz_short[jj]<0)){
	    //	    cout<<"backback pz>0: ";
	    understand=1;
	  }
	  if((px_temp<0&&px_short[jj]>0)||(px_temp>0&&px_short[jj]<0)){
	    //	    cout<<"reverse px: ";
	    understand=1;
	  }

	  if((py_temp<0&&py_short[jj]>0)||(py_temp>0&&py_short[jj]<0)){
	    //	    cout<<"reverse py: ";
	    understand=1;
	  }

	  if(understand==0){
	    cout<<"!!! "<<ventrybellows_temp<<" "<<vebellows_temp<<" "<<z_temp<<" "<<pz_temp<<"diff "<<px_temp-px_short[jj]<<" "<<py_temp-py_short[jj]<<" "<<pz_temp-pz_short[jj]<<" "<<t_temp<<endl;
	    //	    tree->Scan("Entry$:hit.trid:hit.det:hit.x:hit.y:hit.z:hit.pz:hit.px:hit.py:hit.z:hit.t",Form("Entry$==%f&&hit.trid==%f&&hit.det==%d",ventrybellows_temp,vebellows_temp,detnum));
	  }

	  understand=0;

	  }
      }	 
      if(repeat==0){
	ventrybellows_short[count_short]=ventrybellows_temp;
	vebellows_short[count_short]=vebellows_temp;
	x_short[count_short]=x_temp;
	y_short[count_short]=y_temp;
	z_short[count_short]=z_temp;
	px_short[count_short]=px_temp;
	py_short[count_short]=py_temp;
	pz_short[count_short]=pz_temp;
	t_short[count_short]=t_temp;
	count_short++;

	outfile<<runnum<<" ";
	outfile<<detnum<<" ";
	outfile<<ventrybellows_temp<<" ";
	outfile<<vebellows_temp<<" ";
	outfile<<x_temp<<" ";
	outfile<<y_temp<<" ";
	outfile<<z_temp<<" ";
	outfile<<px_temp<<" ";
	outfile<<py_temp<<" ";
	outfile<<pz_temp<<" ";
	outfile<<t_temp<<endl;


      }
      }

    int Nebellows_short = count_short;
    cout<<"Nebellows_short "<<Nebellows_short<<endl;

    //do any of these hit the main det 28?
    //double veboth[Nebellows];
    // double veboth_temp;
    // int bothcheck;
    // int count_both=0;
    // double x_both[Nebellows],y_both[Nebellows],z_both[Nebellows];
    // double px_both[Nebellows],py_both[Nebellows],pz_both[Nebellows],det_both[Nebellows];
    // double xboth_temp, yboth_temp, zboth_temp;
    // double pxboth_temp, pyboth_temp, pzboth_temp, detboth_temp;
    //    for(int ii=0;ii<Ne28;ii++){
    // for(int ii=0;ii<10;ii++){
    //    bothcheck=0;
    //    veboth_temp = ve28[ii];
    //    xboth_temp=x28[ii];
    //    yboth_temp=y28[ii];
    //    zboth_temp=z28[ii];
    //    pxboth_temp=px28[ii];
    //    pyboth_temp=py28[ii];
    //    pzboth_temp=pz28[ii];
    //    //  cout<<veboth_temp<<endl;
    //   for(int jj=0;jj<Nebellows_short;jj++){
    // 	if((veboth_temp==vebellows_short[jj])&&(xboth_temp==x_short[jj])){
    // 	  bothcheck=1;
    // 	  cout<<" found one"<<endl;
    // 	}
    //   }
    //   if(bothcheck==1){
    // 	veboth[count_both]=veboth_temp;
    // 	count_both++;
    //   }
    // }
    
    // int Neboth = count_both;
    // cout<<Neboth<<endl;



    //    for(Int_t ii=0;ii<N;ii++){
      //      tree->Draw("hit.x",Form("hit.trid==%d&&hit.pid==11&&hit.det==28",N),"Q");

    //    }

  
}

  }
