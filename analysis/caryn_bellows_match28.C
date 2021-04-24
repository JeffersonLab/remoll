void caryn_bellows_match28(int detnum=70){
  //counting e's that hit maindet28 and bellows
  //  int runnum=1;

  int Nruns=1000;
  //  int detnum=70;

    int Nmax=10000; //max number of tracks stored per detector per run
    //    int Nmax28=100e3;//for maindet electrons

    TString filename="/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_hybridusfields_100M/remoll_bellows_beam_updateftsd_hybridusfields_100M_100kEv";
    // TString filename="/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_100M/remoll_bellows_beam_100M_100kEv";

  TFile *_file0;


 //  char outfilename[255];
 //  sprintf(outfilename,Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_100M/remoll_electrons_det%d.txt",detnum));
 //  printf("writing output to %s\n",outfilename);

 TString outfilename = Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_hybridusfields_100M/remoll_electrons_updateftsd_hybridusfields_match28_det%d.txt",detnum);
 cout<<"writing output to "<<outfilename.Data()<<endl;
  ofstream outfile;
  outfile.open(outfilename);
  //  outfile<<"test"<<endl;
  //only saving tracks that aren't going through the detector a second time to prevent redundancy

  int bad=0;
  int Nebothsum=0;
  for(int runnum=1;runnum<Nruns+1;runnum++){
    bad=0;
    //don't use runs still going on ifarm
    int badruns[35]={
  980,
  886,
  916,
  854,
  815,
  821,
  799,
  810,
  746,
  754,
  756,
  657,
  592,
  602,
  565,
  573,
  431,
  489,
  412,
  416,
  349,
  385,
  401,
  318,
  320,
  321,
  290,
  292,
  224,
  195,
  199,
  170,
  42,
		   92,
  33};

     for(int ii=0;ii<35;ii++){
       if(runnum==badruns[ii]){
	 //     	bad=1;
       }
     }
     if(runnum>327&&runnum<347){
       //       bad=1;
     }
     bad=0;
    if(bad==0){

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

    //make vectors to store electron track info for bellows
    double trid[Nmax];
    double x[Nmax];
    double y[Nmax];
    double z[Nmax];
    double entry[Nmax];
    double t[Nmax];
    double px[Nmax];
    double py[Nmax];
    double pz[Nmax];
    double det[Nmax];
    double entry_short[Nmax];
    double trid_short[Nmax];
    double x_short[Nmax],y_short[Nmax],z_short[Nmax];
    double px_short[Nmax],py_short[Nmax],pz_short[Nmax],det_short[Nmax];
    double t_short[Nmax];



    //create list of all electron tracks that hit 70
    tree->Draw("hit.trid>>hebellows",Form("hit.det==%d&&hit.pid==11",detnum),"Q");
    h=(TH1D*)gDirectory->FindObject("hebellows");
    int Nebellows=h->GetEntries();
    tree->Draw("Entry$:hit.t",Form("hit.det==%d&&hit.pid==11",detnum),"Q");
    double* pentry = tree->GetV1();
    double* pvt = tree->GetV2();
    for(int ii=0;ii<Nebellows;ii++){
      entry[ii]=*(pentry+ii);
      t[ii]=*(pvt+ii);
    }
    tree->Draw("hit.trid:hit.x:hit.y:hit.z",Form("hit.det==%d&&hit.pid==11",detnum),"Q");
    double* ptrid = tree->GetV1();
   double* pvx = tree->GetV2();
   double* pvy = tree->GetV3();
   double* pvz = tree->GetV4();
    for(int ii=0;ii<Nebellows;ii++){
      trid[ii]=*(ptrid+ii);
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
    cout<<"Nebellows"<<detnum<<" "<<Nebellows<<endl;

    int detnum28=28;//maindet
    //create list of all electron tracks that hit maindet 28
    tree->Draw("hit.trid>>he28",Form("hit.det==%d&&hit.pid==11",detnum28),"Q");
    h=(TH1D*)gDirectory->FindObject("he28");
    int Ne28=h->GetEntries();
    //make vectors to store a limited amount of electron track info for maindet. Can't do more because of vector size
    double trid28[Ne28];
    double entry28[Ne28];
    double t28[Ne28];
    double r28[Ne28];
    double energy28[Ne28];
    //too much memory to have these too
    //    double entry28_short[Ne28];
    //    double trid28_short[Ne28];
    //    double t28_short[Ne28];
    //    double r28_short[Ne28];
    //    double energy28_short[Ne28];

    tree->Draw("Entry$:hit.t",Form("hit.det==%d&&hit.pid==11",detnum28),"Q");
    double* pentry28 = tree->GetV1();
    double* pvt28 = tree->GetV2();
    for(int ii=0;ii<Ne28;ii++){
      entry28[ii]=*(pentry28+ii);
      t28[ii]=*(pvt28+ii);
    }
    tree->Draw("hit.trid:hit.r:hit.e",Form("hit.det==%d&&hit.pid==11",detnum28),"Q");
    double* ptrid28 = tree->GetV1();
   double* pvr28 = tree->GetV2();
   double* pve28 = tree->GetV3();
    for(int ii=0;ii<Ne28;ii++){
      trid28[ii]=*(ptrid28+ii);
      r28[ii]=*(pvr28+ii);
      energy28[ii]=*(pve28+ii);
    }
    cout<<"Ne28 "<<Ne28<<endl;




    //Identify repeated tracks, cycle through names of tracks in trid list, add them to a shortlist if they aren't already included in the shortlist 
    double entry_temp, trid_temp;
    double x_temp, y_temp, z_temp;
    double px_temp, py_temp, pz_temp, det_temp;
    double t_temp;
    int repeat;
    int count_short=0;
    int understand=0;
    for(int ii=0;ii<Nebellows;ii++){
      repeat=0;
      entry_temp = entry[ii];
      trid_temp = trid[ii];
      x_temp= x[ii];
      y_temp= y[ii];
      z_temp= z[ii];
      px_temp= px[ii];
      py_temp= py[ii];
      pz_temp= pz[ii];
      t_temp=t[ii];
      for(int jj=0;jj<count_short;jj++){
       	if((entry_temp==entry_short[jj])&&(trid_temp==trid_short[jj])){
	  repeat=1;
	  if(abs(px_temp-px_short[jj])<1e-15&&abs(py_temp-py_short[jj])<1e-15&&abs(pz_temp-pz_short[jj])<1e-15){
	    understand=1;
	  }
	  if((pz_temp<0&&pz_short[jj]>0)){
	    understand=1;
	  }
	  if((pz_temp>0&&pz_short[jj]<0)){
	    understand=1;
	  }
	  if((px_temp<0&&px_short[jj]>0)||(px_temp>0&&px_short[jj]<0)){
	    understand=1;
	  }

	  if((py_temp<0&&py_short[jj]>0)||(py_temp>0&&py_short[jj]<0)){
	    understand=1;
	  }
	  if(understand==0){
	    //	  	    cout<<"!!! "<<entry_temp<<" "<<trid_temp<<" "<<z_temp<<" "<<pz_temp<<"diff "<<px_temp-px_short[jj]<<" "<<py_temp-py_short[jj]<<" "<<pz_temp-pz_short[jj]<<" "<<t_temp<<endl;
	    //	    tree->Scan("Entry$:hit.trid:hit.det:hit.x:hit.y:hit.z:hit.pz:hit.px:hit.py:hit.z:hit.t",Form("Entry$==%f&&hit.trid==%f&&hit.det==%d",entry_temp,trid_temp,detnum));
	  }
	  understand=0;
	  }
      }	 
      if(repeat==0){
	entry_short[count_short]=entry_temp;
	trid_short[count_short]=trid_temp;
	x_short[count_short]=x_temp;
	y_short[count_short]=y_temp;
	z_short[count_short]=z_temp;
	px_short[count_short]=px_temp;
	py_short[count_short]=py_temp;
	pz_short[count_short]=pz_temp;
	t_short[count_short]=t_temp;
	count_short++;

	// outfile<<runnum<<" ";
	// outfile<<detnum<<" ";
	// outfile<<entry_temp<<" ";
	// outfile<<trid_temp<<" ";
	// outfile<<x_temp<<" ";
	// outfile<<y_temp<<" ";
	// outfile<<z_temp<<" ";
	// outfile<<px_temp<<" ";
	// outfile<<py_temp<<" ";
	// outfile<<pz_temp<<" ";
	// outfile<<t_temp<<endl;
      }
    }

    int Nebellows_short = count_short;
    cout<<"Nebellows_short "<<Nebellows_short<<endl;

    //look for repeated tracks in maindet 28
    //Identify repeated tracks, cycle through names of tracks in trid list, add them to a shortlist if they aren't already included in the shortlist 
    //NEVERMIND TOO MUCH MEMORY AND AREN'T HARDLY ANY REPEATED TRACKS IN MAINDET ANYWAY
    // double entry28_temp, trid28_temp;
    // double t28_temp;
    // double r28_temp,energy28_temp;
    // int repeat28;
    // int count28_short=0;
    // int understand28=0;
    // for(int ii=0;ii<Ne28;ii++){
    //   repeat28=0;
    //   entry28_temp = entry28[ii];
    //   trid28_temp = trid28[ii];
    //   //      t28_temp=t28[ii];
    //   //      r28_temp=r28[ii];
    //   //      energy28_temp=energy28[ii];
    //   for(int jj=0;jj<count28_short;jj++){
    //    	if((entry28_temp==entry28_short[jj])&&(trid28_temp==trid28_short[jj])){
    // 	  repeat28=1;
    // 	  }
    //   }
    //   if(repet28==0){
    // 	entry28_short[count28_short]=entry28_temp;
    // 	trid28_short[count28_short]=trid28_temp;
    // 	//	t28_short[count28_short]=t28_temp;
    // 	//	r28_short[count28_short]=r28_temp;
    // 	//	energy28_short[count28_short]=energy28_temp;
    // 	count28_short++;
    //   }
    //   }

    // int Ne28_short = count28_short;
    // cout<<"Ne28_short "<<Ne28_short<<endl;


    //look for matches between the two lists
    //loop over maindet list
    //loop over bellows short list
    double entry28_temp, trid28_temp;
    double t28_temp;
    double r28_temp,energy28_temp;
    int repeat28;
    int count28_short=0;
    int repeatmatch=0;
    double entry28_short[Nebellows_short];
    double trid28_short[Nebellows_short];
    //    int understand28=0;
    for(int ii=0;ii<Ne28;ii++){
      repeatmatch=0;
      repeat28=0;
      entry28_temp = entry28[ii];
      trid28_temp = trid28[ii];
      t28_temp=t28[ii];
      r28_temp=r28[ii];
      energy28_temp=energy28[ii];
      for(int jj=0;jj<count_short;jj++){
       	if((entry28_temp==entry_short[jj])&&(trid28_temp==trid_short[jj])){
    	  repeat28=1;
	  entry_temp = entry_short[jj];
	  trid_temp = trid_short[jj];
	  x_temp= x_short[jj];
	  y_temp= y_short[jj];
	  z_temp= z_short[jj];
	  px_temp= px_short[jj];
	  py_temp= py_short[jj];
	  pz_temp= pz_short[jj];
	  t_temp=t_short[jj];
	  if(t28_temp<t_temp){
	    cout<<"don't include this: hit main det before hit bellows"<<endl;
	  }
    	  }
      }
      //search for repeats
      for(int kk=0;kk<count28_short;kk++){
      if(trid28_short[kk]==trid28_temp&&entry28_short[kk]==entry28_temp){
      repeatmatch=1;
      }
      }
      if(repeat28==1&&(t28_temp>t_temp)&&repeatmatch==0){//&&repeatmatch==0
	//trid28_short[count28_short]=trid_temp
	//entry28_short[count28_short]=entry_temp
	outfile<<runnum<<" ";
	outfile<<detnum<<" ";
	outfile<<entry_temp<<" ";
	outfile<<trid_temp<<" ";
	outfile<<x_temp<<" ";
	outfile<<y_temp<<" ";
	outfile<<z_temp<<" ";
	outfile<<px_temp<<" ";
	outfile<<py_temp<<" ";
	outfile<<pz_temp<<" ";
	outfile<<t_temp<<" ";

	outfile<<t28_temp<<" ";
	outfile<<r28_temp<<" ";
	outfile<<energy28_temp<<endl;

    	count28_short++;
      }
      }

    int Neboth = count28_short;
    cout<<"Neboth "<<Neboth<<endl;

    Nebothsum=Nebothsum+Neboth;
    cout<<"Nebothsum "<<Nebothsum<<endl;



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
    // 	if((veboth_temp==trid_short[jj])&&(xboth_temp==x_short[jj])){
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
  outfile<<"I'm done"<<endl;
  }
