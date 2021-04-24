void caryn_bellows_quickmatch(int detnum=0){
  //counting e's that hit maindet28 and bellows and recording value at 5547 (right after target)
  //  int runnum=1;
  TString detcut;
  if(detnum==0){
    detcut="(hit.det==70||hit.det==71||hit.det==72||hit.det==73||hit.det==74||hit.det==75||hit.det==76)";
  }
  if(detnum==70){
    detcut="(hit.det==70)";
  }
  if(detnum==71){
    detcut="(hit.det==71)";
  }
  if(detnum==72){
    detcut="(hit.det==72)";
  }
  if(detnum==73){
    detcut="(hit.det==73)";
  }
  if(detnum==74){
    detcut="(hit.det==74)";
  }
  if(detnum==75){
    detcut="(hit.det==75)";
  }
  if(detnum==76){
    detcut="(hit.det==76)";
  }


  int Nruns=1000;
  int Nmax=200000;
 //max number of tracks stored per detector per run
    //    int Nmax28=100e3;//for maindet electrons

    int entryMin=-1;
    int entryMax=entryMin+100000+1;

    TString filename="/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_hybridusfields_100M/remoll_bellows_beam_updateftsd_hybridusfields_100M_100kEv";
    // TString filename="/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_100M/remoll_bellows_beam_100M_100kEv";

  TFile *_file0;


 TString outfilename = Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_hybridusfields_100M/remoll_electrons_updateftsd_hybridusfields_quickmatch_det%d.txt",detnum);
 cout<<"writing output to "<<outfilename.Data()<<endl;
  ofstream outfile;
  outfile.open(outfilename);
  //  outfile<<"test"<<endl;
  //only saving tracks that aren't going through the detector a second time to prevent redundancy

  int Nebothsum=0;
  for(int runnum=1;runnum<Nruns+1;runnum++){//<Nruns+1

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
    double entry[Nmax];
    double det[Nmax];
    double time[Nmax];

    //create list of all electron tracks that hit bellows,maindet28
    tree->Draw("hit.trid>>hebellows",Form("(%s||hit.det==28)&&hit.pid==11&&hit.trid==1&&Entry$<%d&&Entry$>%d",detcut.Data(),entryMax,entryMin),"Q");
    h=(TH1D*)gDirectory->FindObject("hebellows");
    int Ne=h->GetEntries();
    tree->Draw("Entry$:hit.det:hit.t",Form("(%s||hit.det==28)&&hit.pid==11&&hit.trid==1&&Entry$<%d&&Entry$>%d",detcut.Data(),entryMax,entryMin),"Q");
    double* pentry = tree->GetV1();
   double* pvdet = tree->GetV2();
   double* pvtime = tree->GetV3();
    for(int ii=0;ii<Ne;ii++){
      entry[ii]=*(pentry+ii);
      det[ii]=*(pvdet+ii);
      time[ii]=*(pvtime+ii);
    }
 
   //filter list: keep if e's hit bellows, check before and after ii for hit to 28 or 5547
    int cc=0;
    int Ntiny = 100;
    double tinylist[Ntiny];
    double tinydetlist[Ntiny];
    int found5547=0;
    int found28=0;
    for(int ii=0;ii<Ne;ii++){
      found5547=0;
      found28=0;
      if(det[ii]==70||det[ii]==71||det[ii]==72||det[ii]==73||det[ii]==74||det[ii]==75||det[ii]==76){

	if(entry[ii]==entry[ii+1]||entry[ii]==entry[ii-1]){
	cout<<entry[ii]<<" "<<det[ii]<<" ";

	//look for maindet hit
	if(entry[ii]==entry[ii+1]&&det[ii+1]==28&&time[ii]<time[ii+1]){
	  cout<<entry[ii+1]<<" "<<det[ii+1]<<endl;
	  found28=1;
	  tinylist[cc]=entry[ii];
	  tinydetlist[cc]=det[ii];
	  cc++;
	}
	if(entry[ii]==entry[ii-1]&&det[ii-1]==28&&time[ii]<time[ii-1]){
	  cout<<entry[ii-1]<<" "<<det[ii-1]<<endl;
	  tinylist[cc]=entry[ii];
	  tinydetlist[cc]=det[ii];
	  found28=1;
	  cc++;
	}
	if(found28==0){
	  cout<<"didn't find maindet 28 hit "<<entry[ii-1]<<" "<<det[ii-1]<<" "<<entry[ii+1]<<" "<<det[ii+1]<<endl;
	}
	}
	// if(found28==1){	//look before and after for the 5547 event
	//   if(entry[ii]==entry[ii+2]&&det[ii+2]==5547){
	//       cout<<entry[ii+2]<<" "<<det[ii+2]<<endl;
	//       found5547=1;
  	//   }
	//   if(entry[ii]==entry[ii+1]&&det[ii+1]==5547){
	//       cout<<entry[ii+1]<<" "<<det[ii+1]<<endl;
	//       found5547=1;
  	//   }
   	//   if(entry[ii]==entry[ii-1]&&det[ii-1]==5547){
	//       cout<<entry[ii-1]<<" "<<det[ii-1]<<endl;
	//       found5547=1;
	//   }
   	//   if(entry[ii]==entry[ii-2]&&det[ii-2]==5547){
	//       cout<<entry[ii-2]<<" "<<det[ii-2]<<endl;
	//       found5547=1;
	//   }

	//   if(found5547==0){
	//     cout<<"uhoh! can't find 5547 source"<<endl;
	//   }
	// }

      }
    }

	int Neboth=cc;
	cout<<"Ne bellows&28 "<<Neboth<<endl;
	if(Neboth>0){
	int entryboth[Neboth];
	int detboth[Neboth];
	double x[Neboth];
	double y[Neboth];
	double z[Neboth];
	double px[Neboth];
	double py[Neboth];
	double pz[Neboth];
	double t[Neboth];
	double e[Neboth];
	//	double r[Neboth];

	double x28[Neboth];
	double y28[Neboth];
	double z28[Neboth];
	double px28[Neboth];
	double py28[Neboth];
	double pz28[Neboth];
	double t28[Neboth];
	double e28[Neboth];
	//	double r28[Neboth];

	double x5547[Neboth];
	double y5547[Neboth];
	double z5547[Neboth];
	double px5547[Neboth];
	double py5547[Neboth];
	double pz5547[Neboth];
	double t5547[Neboth];
	double e5547[Neboth];
	//	double r5547[Neboth];

	int Ndoublehit;
	double trackid=1;
	for(int jj=0;jj<Neboth;jj++){
	  entryboth[jj]=tinylist[jj];
	  detboth[jj]=tinydetlist[jj];
	  //	  cout<<"check double hits"<<endl;
	  tree->Draw("hit.trid>>hdoublehit",Form("hit.det==%d&&hit.pid==11&&hit.trid==1&&Entry$==%d",detboth[jj],entryboth[jj]),"Q");
	  h=(TH1D*)gDirectory->FindObject("hdoublehit");
	  int Ndoublehit = h->GetEntries();
	  if(Ndoublehit>1){
	    cout<<"oh no! a double hit"<<entryboth[jj]<<" "<<Ndoublehit<<endl;
	  }
	  //	  cout<<"get x,y,z,t"<<endl;
	   tree->Draw("hit.t:hit.x:hit.y:hit.z",Form("hit.det==%d&&hit.pid==11&&hit.trid==1&&Entry$==%d",detboth[jj],entryboth[jj]),"Q");
	  double* pvt = tree->GetV1();
	  double* pvx = tree->GetV2();
	  double* pvy = tree->GetV3();
	  double* pvz = tree->GetV4();
	  t[jj]=*(pvt);
	  x[jj]=*(pvx);
	  y[jj]=*(pvy);
	  z[jj]=*(pvz);
	  double xtemp=x[jj];
	  double ytemp=y[jj];
	  //	  r[jj]=sqrt(pow(x[jj],2)+pow(y[jj],2));      
	  // 	  cout<<"get px,py,pz,e"<<endl;
	   tree->Draw("hit.e:hit.px:hit.py:hit.pz",Form("hit.det==%d&&hit.pid==11&&hit.trid==1&&Entry$==%d",detboth[jj],entryboth[jj]),"Q");
	  double* pve = tree->GetV1();
	  double* pvpx = tree->GetV2();
	  double* pvpy = tree->GetV3();
	  double* pvpz = tree->GetV4();
	  e[jj]=*(pve);
	  px[jj]=*(pvpx);
	  py[jj]=*(pvpy);
	  pz[jj]=*(pvpz);

	  //	  cout<<"get maindet x,y,z,t"<<endl;
	  tree->Draw("hit.t:hit.x:hit.y:hit.z",Form("hit.det==28&&hit.pid==11&&hit.trid==1&&Entry$==%d&&hit.t>%6.5f",entryboth[jj],t[jj]),"Q");
	  double* pvt28 = tree->GetV1();
	  double* pvx28 = tree->GetV2();
	  double* pvy28 = tree->GetV3();
	  double* pvz28 = tree->GetV4();
	  t28[jj]=*(pvt28);
	  x28[jj]=*(pvx28);
	  y28[jj]=*(pvy28);
	  z28[jj]=*(pvz28);
	  //r28[jj]=sqrt(pow(x28[jj],2)+pow(y28[jj],2));       
	  //	  cout<<"get maindet px,py,pz,e"<<endl;
	  tree->Draw("hit.e:hit.px:hit.py:hit.pz",Form("hit.det==28&&hit.pid==11&&hit.trid==1&&Entry$==%d&&hit.t>%6.5f",entryboth[jj],t[jj]),"Q");
	  double* pve28 = tree->GetV1();
	  double* pvpx28 = tree->GetV2();
	  double* pvpy28 = tree->GetV3();
	  double* pvpz28 = tree->GetV4();
	  e28[jj]=*(pve28);
	  px28[jj]=*(pvpx28);
	  py28[jj]=*(pvpy28);
	  pz28[jj]=*(pvpz28);

	  //	  cout<<"get 5547 x,y,z,t"<<endl;
	  tree->Draw("hit.t:hit.x:hit.y:hit.z",Form("hit.det==5547&&hit.pid==11&&hit.trid==1&&Entry$==%d",entryboth[jj]),"Q");
	  double* pvt5547 = tree->GetV1();
	  double* pvx5547 = tree->GetV2();
	  double* pvy5547 = tree->GetV3();
	  double* pvz5547 = tree->GetV4();
	  t5547[jj]=*(pvt5547);
	  x5547[jj]=*(pvx5547);
	  y5547[jj]=*(pvy5547);
	  z5547[jj]=*(pvz5547);
	  //	  r5547[jj]=sqrt(pow(x5547[jj],2)+pow(y5547[jj],2));       
	  //	  cout<<"get 5547 px,py,pz,e"<<endl;
	  tree->Draw("hit.e:hit.px:hit.py:hit.pz",Form("hit.det==5547&&hit.pid==11&&hit.trid==1&&Entry$==%d",entryboth[jj]),"Q");
	  double* pve5547 = tree->GetV1();
	  double* pvpx5547 = tree->GetV2();
	  double* pvpy5547 = tree->GetV3();
	  double* pvpz5547 = tree->GetV4();
	  e5547[jj]=*(pve5547);
	  px5547[jj]=*(pvpx5547);
	  py5547[jj]=*(pvpy5547);
	  pz5547[jj]=*(pvpz5547);

	  //only count in txtfile if time(hit bellows)<time(hit main det)
	  if(t28[jj]<t[jj]){
	    cout<<"time: hit maindet before bellows: don't count"<<endl;
	  }
	  if(t[jj]<t28[jj]){
	    cout<<"write to file "<<detboth[jj]<<" "<<entryboth[jj]<<endl;
	    outfile<<runnum<<" ";
	    outfile<<detboth[jj]<<" ";
	    outfile<<entryboth[jj]<<" ";
	    outfile<<trackid<<" ";
	    outfile<<x[jj]<<" ";
	    outfile<<y[jj]<<" ";
	    outfile<<z[jj]<<" ";
	    outfile<<px[jj]<<" ";
	    outfile<<py[jj]<<" ";
	    outfile<<pz[jj]<<" ";
	    outfile<<e[jj]<<" ";
	    outfile<<t[jj]<<" ";

	    outfile<<x28[jj]<<" ";
	    outfile<<y28[jj]<<" ";
	    outfile<<z28[jj]<<" ";
	    outfile<<px28[jj]<<" ";
	    outfile<<py28[jj]<<" ";
	    outfile<<pz28[jj]<<" ";
	    outfile<<e28[jj]<<" ";
	    outfile<<t28[jj]<<" ";

	    outfile<<x5547[jj]<<" ";
	    outfile<<y5547[jj]<<" ";
	    outfile<<z5547[jj]<<" ";
	    outfile<<px5547[jj]<<" ";
	    outfile<<py5547[jj]<<" ";
	    outfile<<pz5547[jj]<<" ";
	    outfile<<e5547[jj]<<" ";
	    outfile<<t5547[jj]<<endl;
	  }

	}
	}
	//    cout<<"Ne "<<detnum<<"||28: "<<Ne<<endl;
	 cout<<"Ne bellows||28: "<<Ne<<endl;


  }
  outfile<<endl;
  outfile<<"I'm done"<<endl;
  }
