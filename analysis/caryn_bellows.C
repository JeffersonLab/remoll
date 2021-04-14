void caryn_bellows(int detnum=70){

  //  int runnum=1;

  int Nruns=1000;
  //  int detnum=70;

    int Nmax=10000; //max number of tracks stored per detector per run


 TString filename="/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_100M/remoll_bellows_beam_100M_100kEv";
  TFile *_file0;


 //  char outfilename[255];
 //  sprintf(outfilename,Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_100M/remoll_electrons_det%d.txt",detnum));
 //  printf("writing output to %s\n",outfilename);

 TString outfilename = Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_100M/remoll_electrons_det%d.txt",detnum);
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
 


    TTree *t = (TTree*)_file0->Get("T");
    TH1D *h;


    double vebellows[Nmax];
    double x[Nmax];
    double y[Nmax];
    double z[Nmax];
    double ventrybellows[Nmax];
    double px[Nmax];
    double py[Nmax];
    double pz[Nmax];
    double det[Nmax];
    double ventrybellows_short[Nmax];
    double vebellows_short[Nmax];
    double x_short[Nmax],y_short[Nmax],z_short[Nmax];
    double px_short[Nmax],py_short[Nmax],pz_short[Nmax],det_short[Nmax];




    //how many tracks are there?
    // //    t->Draw("hit.trid>>h1","hit.det==28&&hit.pid==11","Q");
    // t->Draw("hit.trid>>hall","","Q");
    // h=(TH1D*)gDirectory->FindObject("hall");
    // int N=h->GetEntries();
    // //    cout<<"start filling vector"<<endl;
    // //build vector of data
    // double* pvall = t->GetV1();
    // double vall[N];
    // for(int ii=0;ii<N;ii++){
    //   vall[ii]=*(pvall+ii);
    // }
    //  cout<<"done filling vector"<<endl;

    //how many e tracks?
    // t->Draw("hit.trid>>he","hit.pid==11","Q");
    // h=(TH1D*)gDirectory->FindObject("he");
    // int Ne=h->GetEntries();
    // //build vector of data
    // double* pve = t->GetV1();
    // double ve[N];
    // for(int ii=0;ii<Ne;ii++){
    //   ve[ii]=*(pve+ii);
    // }
 
    //how many e's hit main det?
    // t->Draw("hit.trid>>he28","hit.det==28&&hit.pid==11","Q");
    // h=(TH1D*)gDirectory->FindObject("he28");
    // int Ne28=h->GetEntries();
    //build vector of data
   //  t->Draw("hit.trid:hit.x:hit.y:hit.z","hit.det==28&&hit.pid==11");
   //  double* pve28 = t->GetV1();
   // double* pvx28 = t->GetV2();
   // double* pvy28 = t->GetV3();
   // double* pvz28 = t->GetV4();
   //  double ve28[Ne28];
   //  double x28[Ne28];
   //  double y28[Ne28];
   //  double z28[Ne28];
   //  double px28[Ne28];
   //   double py28[Ne28];
   //   double pz28[Ne28];
     // double det28[Ne28];
   //  for(int ii=0;ii<Ne28;ii++){
   //    ve28[ii]=*(pve28+ii);
   //    x28[ii]=*(pvx28+ii);
   //    y28[ii]=*(pvy28+ii);
   //    z28[ii]=*(pvz28+ii);
   //  }
   // t->Draw("hit.px:hit.py:hit.pz","hit.det==28&&hit.pid==11");
   //  double* pvpx28 = t->GetV1();
   //  double* pvpy28 = t->GetV2();
   //  double* pvpz28 = t->GetV3();
   // //   double* pvdet28 = t->GetV4();
   // for(int ii=0;ii<Ne28;ii++){
   //    px28[ii]=*(pvpx28+ii);
   //    py28[ii]=*(pvpy28+ii);
   //    pz28[ii]=*(pvpz28+ii);
   //    // det28[ii]=*(pvdet28+ii);
   //  }




   //how many e's hit bellows1,2....6?
    // t->Draw("hit.trid>>he70","hit.det==70&&hit.pid==11","Q");
    // h=(TH1D*)gDirectory->FindObject("he70");
    // int Ne70=h->GetEntries();
    // //build vector of data
    // double* pve70 = t->GetV1();
    // double ve70[Ne70];
    // for(int ii=0;ii<Ne70;ii++){
    //   ve70[ii]=*(pve70+ii);
    // }


    // t->Draw("hit.trid>>he71","hit.det==71&&hit.pid==11","Q");
    // h=(TH1D*)gDirectory->FindObject("he71");
    // int Ne71=h->GetEntries();
    // double* pve71 = t->GetV1();
    // double ve71[Ne71];
    // for(int ii=0;ii<Ne71;ii++){
    //   ve71[ii]=*(pve71+ii);
    // }

    // t->Draw("hit.trid>>he72","hit.det==72&&hit.pid==11","Q");
    // h=(TH1D*)gDirectory->FindObject("he72");
    // int Ne72=h->GetEntries();
    // double* pve72 = t->GetV1();
    // double ve72[Ne72];
    // for(int ii=0;ii<Ne72;ii++){
    //   ve72[ii]=*(pve72+ii);
    // }


    // t->Draw("hit.trid>>he73","hit.det==73&&hit.pid==11","Q");
    // h=(TH1D*)gDirectory->FindObject("he73");
    // int Ne73=h->GetEntries();
    // double* pve73 = t->GetV1();
    // double ve73[Ne73];
    // for(int ii=0;ii<Ne73;ii++){
    //   ve73[ii]=*(pve73+ii);
    // }

    // t->Draw("hit.trid>>he74","hit.det==74&&hit.pid==11","Q");
    // h=(TH1D*)gDirectory->FindObject("he74");
    // int Ne74=h->GetEntries();
    // double* pve74 = t->GetV1();
    // double ve74[Ne74];
    // for(int ii=0;ii<Ne74;ii++){
    //   ve74[ii]=*(pve74+ii);
    // }

    // t->Draw("hit.trid>>he75","hit.det==75&&hit.pid==11","Q");
    // h=(TH1D*)gDirectory->FindObject("he75");
    // int Ne75=h->GetEntries();
    // double* pve75 = t->GetV1();
    // double ve75[Ne75];
    // for(int ii=0;ii<Ne75;ii++){
    //   ve75[ii]=*(pve75+ii);
    // }

    // t->Draw("hit.trid>>he76","hit.det==76&&hit.pid==11","Q");
    // h=(TH1D*)gDirectory->FindObject("he76");
    // int Ne76=h->GetEntries();
    // double* pve76 = t->GetV1();
    // double ve76[Ne76];
    // for(int ii=0;ii<Ne76;ii++){
    //   ve76[ii]=*(pve76+ii);
    // }

    //create list of all electron tracks that hit 70
    //    t->Draw("hit.trid>>hebellows","(hit.det>69&&hit.det<77)&&hit.pid==11");
    t->Draw("hit.trid>>hebellows",Form("hit.det==%d&&hit.pid==11",detnum),"Q");
 
    h=(TH1D*)gDirectory->FindObject("hebellows");
    int Nebellows=h->GetEntries();
    ///    t->Draw("hit.trid:hit.x:hit.y:hit.z","(hit.det>69&&hit.det<77)&&hit.pid==11");
    t->Draw("Entry$",Form("hit.det==%d&&hit.pid==11",detnum),"Q");
    double* pventrybellows = t->GetV1();
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
    }
    t->Draw("hit.trid:hit.x:hit.y:hit.z",Form("hit.det==%d&&hit.pid==11",detnum),"Q");
    double* pvebellows = t->GetV1();
   double* pvx = t->GetV2();
   double* pvy = t->GetV3();
   double* pvz = t->GetV4();
    for(int ii=0;ii<Nebellows;ii++){
      vebellows[ii]=*(pvebellows+ii);
      x[ii]=*(pvx+ii);
      y[ii]=*(pvy+ii);
      z[ii]=*(pvz+ii);
    }
    t->Draw("hit.px:hit.py:hit.pz:hit.det",Form("hit.det==%d&&hit.pid==11",detnum),"Q");
   double* pvpx = t->GetV1();
   double* pvpy = t->GetV2();
   double* pvpz = t->GetV3();
   double* pvdet = t->GetV4();
    for(int ii=0;ii<Nebellows;ii++){
      px[ii]=*(pvpx+ii);
      py[ii]=*(pvpy+ii);
      pz[ii]=*(pvpz+ii);
      det[ii]=*(pvdet+ii);
    }

    //    int Neaddbellows = Ne70+Ne71+Ne72+Ne73+Ne74+Ne75+Ne76;
   //create list of all electron tracks that hit 70-76 or main det 28
    // t->Draw("hit.trid>>hebellows","(hit.det>69&&hit.det<77)&&hit.det==28&&hit.pid==11","Q");
 
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
	    cout<<"!!! "<<ventrybellows_temp<<" "<<vebellows_temp<<" "<<z_temp<<" "<<pz_temp<<"diff "<<px_temp-px_short[jj]<<" "<<py_temp-py_short[jj]<<" "<<pz_temp-pz_short[jj]<<endl;
	    t->Scan("Entry$:hit.trid:hit.det:hit.x:hit.y:hit.z:hit.pz:hit.px:hit.py:hit.z:hit.t",Form("Entry$==%f&&hit.trid==%f&&hit.det==%d",ventrybellows_temp,vebellows_temp,detnum));
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
	outfile<<pz_temp<<endl;


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
      //      t->Draw("hit.x",Form("hit.trid==%d&&hit.pid==11&&hit.det==28",N),"Q");

    //    }

  
}

  }
