void count28_caryn_bellows(int detnum=75){

  TCanvas *c1 = new TCanvas("c1","c1",0,0,200,200);

 TString filename="/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_100M/remoll_bellows_beam_updateftsd_100M_100kEv";
  TFile *_file0;


  TString infilename=Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_100M/remoll_electrons_updateftsd_det%d.txt",detnum);

  ifstream infile;
  infile.open(infilename);
  cout<<infilename<<endl;

TString outfilename=Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_100M/remoll_electrons_updateftsd_det28_det%d.txt",detnum);
  ofstream outfile;
  outfile.open(outfilename);

  double irun, idet, ientry, itrack, ix, iy, iz, ipx, ipy, ipz, ipt;
  double ien,ip;
  double me=0.511;
  //E = sqrt (me^2 + p^2)
  //p =sqrt(px^2+ py^2+pz^2)

  const int N=80000;//icnt-1;80k is maximum allowed

  double run[N], det[N], entry[N], track[N];
  double x[N], y[N], z[N], px[N], py[N], pz[N], time[N];
  double en[N],p[N];
  //  double me=0.511;
  //E = sqrt (me^2 + p^2)
  //p =sqrt(px^2+ py^2+pz^2)
 

  int icnt=0;
  int prevrunnum=0;
  int hitmaindet=0;
  TTree *t;
  TH1D *h;
  int runnum;
  int iifound;
  double myradius, mye, myt;
  while(1) {
    infile >> irun >> idet >> ientry >> itrack >> ix >> iy >> iz >> ipx >> ipy >> ipz >> ipt;
      ip=sqrt(pow(ipx,2)+pow(ipy,2)+pow(ipz,2));
      ien=sqrt(pow(me,2)+pow(ip,2));

      runnum=irun;
      //      if(runnum<10){
      if(runnum!=prevrunnum){//don't reopen the file if it's already open
	//     	cout<<"opening new file "<<runnum<<endl;
	//	cout<<" "<<runnum<<" ";
	if(runnum<10){
	  _file0 = TFile::Open(Form("%s_00%d/remollout.root",filename.Data(),runnum));
	  //	  cout<<Form("%s_00%d/remollout.root",filename.Data(),runnum)<<endl;
	}
	if(runnum>9&&runnum<100){
  	_file0 = TFile::Open(Form("%s_0%d/remollout.root",filename.Data(),runnum));
	//  	cout<<Form("%s_0%d/remollout.root",filename.Data(),runnum)<<endl;
	}
	if(runnum>99){
	  _file0 = TFile::Open(Form("%s_%d/remollout.root",filename.Data(),runnum));
	  //	  cout<<Form("%s_0%d/remollout.root",filename.Data(),runnum)<<endl;
	}
      }
	//t = (TTree*)_file0->Get("T");
      

        TTree *t = (TTree*)_file0->Get("T");

    //look for same event, trackid, electron that also hits det 28
	//	t->Draw("hit.e","hit.det==28&&hit.pid==11","Q");
      
       t->Draw(Form("hit.r>>hr%d",icnt),Form("hit.det==28&&hit.pid==11&&hit.trid==%6.0f&&Entry$==%6.0f",itrack,ientry),"Q");
       h=(TH1D*)gDirectory->FindObject(Form("hr%d",icnt));
          hitmaindet=h->GetEntries();
	  myradius=h->GetMean();

       t->Draw(Form("hit.e>>he%d",icnt),Form("hit.det==28&&hit.pid==11&&hit.trid==%6.0f&&Entry$==%6.0f",itrack,ientry),"Q");
       h=(TH1D*)gDirectory->FindObject(Form("he%d",icnt));
	  mye=h->GetMean();

       t->Draw(Form("hit.t>>ht%d",icnt),Form("hit.det==28&&hit.pid==11&&hit.trid==%6.0f&&Entry$==%6.0f",itrack,ientry),"Q");
       h=(TH1D*)gDirectory->FindObject(Form("ht%d",icnt));
	  myt=h->GetMean();

	  //	  cout<<hitmaindet<<endl;
          if(hitmaindet!=0){
            cout<<"hit maindet28! "<<"run="<<irun<<" track="<<itrack<<" Entry$="<<ientry<<" Energy="<<ien<<endl;
	    iifound ++;
	    run[iifound]=irun;
	    det[iifound]=idet;
	    entry[iifound]=ientry;
	    track[iifound]=itrack;
	    x[iifound]=ix;
	    y[iifound]=iy;
	    z[iifound]=iz;
	    px[iifound]=ipx;
	    py[iifound]=ipy;
	    pz[iifound]=ipz;
	    time[iifound]=ipt;
	    p[iifound]=sqrt(pow(px[iifound],2)+pow(py[iifound],2)+pow(pz[iifound],2));
	    en[iifound]=sqrt(pow(me,2)+pow(p[iifound],2));
	    outfile<<irun<<" ";
	    outfile<<idet<<" ";
	    outfile<<ientry<<" ";
	    outfile<<itrack<<" ";
	    outfile<<ix<<" ";
	    outfile<<iy<<" ";
	    outfile<<iz<<" ";
	    outfile<<ipx<<" ";
	    outfile<<ipy<<" ";
	    outfile<<ipz<<" ";
	    outfile<<ipt<<" ";
	    outfile<<ip<<" ";
	    outfile<<ien<<" ";
	    outfile<<myradius<<" "<<endl;
	    outfile<<mye<<" "<<endl;
	    outfile<<myt<<" "<<endl;
          }

      

	  //            _file0->Close();
	  //}//close runnum<10

  
      prevrunnum=runnum;

      icnt++;
    if(!infile.good()) break;
  }


  int Ne = icnt-1;
  int Nemain = iifound;






}
