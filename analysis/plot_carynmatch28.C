void plot_carynmatch28(int plot=0){

   //Tfile
  //TString filename="/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_100M/remoll_bellows_beam_100M_100kEv";

  //  TString filename = "_updateftsd_carynplots.root";
  TString filename = "count_uspdatefttsd_hybridusfields_match28.root";
  TFile *_file0 = TFile::Open(Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_hybridusfields_100M/%s",filename.Data()));
  //TDirectory *d = _file0->GetDirectory("c");
  TTree *tree = (TTree*)_file0->Get("c");

  TString  cut73="";//"&&(emain<1100||emain>1600)";
  TString cut74="&&(sqrt(pow(x,2)+pow(y,2))<518.16+30)";
  TString cut75="&&(sqrt(pow(x,2)+pow(y,2))<523.24+30)";


  //Get energy #N info

    TH1D* h;
    int N;
    double mean;

    int Nall;
    double meanall;
    int N1;
    double mean1;
    int N10;
    double mean10;
    int N100;
    double mean100;
    int N1000;
    double mean1000;
    int N10GeV;
    double mean10GeV;
    int N11GeV;
    double mean11GeV;


    int detnum;
    TString cut;
    int countdet[7];
    int Ndet=0;
    cout<<"detnum N(<1MeV) N(1-10MeV) N(10-1000MeV) N(1-10GeV) N(>10GeV)  E(<1MeV) E(1-10MeV) E(10-1000MeV) E(1-10GeV) E(>10GeV)"<<endl;
    cout<<"for 74,75 (b5,b6) cut on radial thickness <30mm"<<endl;
    cout<<"for 73 (b4) cut out ~1.1-1.6MeV peak"<<endl;
    for(int ii=0;ii<7;ii++){
      detnum=70+ii;
      cut="";
	if(detnum==73){
	  cut=cut73;//"(emain<1100||emain>1600)";
	}
	if(detnum==74){
	  cut=cut74;//"(sqrt(pow(x,2)+pow(y,2))<518.16+30)";
	}
	if(detnum==75){
	  cut=cut75;//;"(sqrt(pow(x,2)+pow(y,2))<523.24+30)";
	}
	tree->Draw(Form("emain>>heall_%d",detnum),Form("track==1&&det==%d%s",detnum,cut.Data()),"Q");
      h=(TH1D*)_file0->FindObject(Form("heall_%d",detnum));
      Nall = h->GetEntries();
      meanall=h->GetMean();

      tree->Draw(Form("emain>>he1_%d",detnum),Form("track==1&&det==%d&&emain<1%s",detnum,cut.Data()),"Q");
      h=(TH1D*)_file0->FindObject(Form("he1_%d",detnum));
      N1 = h->GetEntries();
      mean1=h->GetMean();

      tree->Draw(Form("emain>>he10_%d",detnum),Form("track==1&&det==%d&&emain>1&&emain<10%s",detnum,cut.Data()),"Q");
      h=(TH1D*)_file0->FindObject(Form("he10_%d",detnum));
      N10 = h->GetEntries();
      mean10=h->GetMean();

      tree->Draw(Form("emain>>he100_%d",detnum),Form("track==1&&det==%d&&emain>10&&emain<100%s",detnum,cut.Data()),"Q");
      h=(TH1D*)_file0->FindObject(Form("he100_%d",detnum));
      N100 = h->GetEntries();
      mean100=h->GetMean();

      tree->Draw(Form("emain>>he1000_%d",detnum),Form("track==1&&det==%d&&emain>100&&emain<1000%s",detnum,cut.Data()),"Q");
      h=(TH1D*)_file0->FindObject(Form("he1000_%d",detnum));
      N1000 = h->GetEntries();
      mean1000=h->GetMean();

      tree->Draw(Form("emain>>he10g_%d",detnum),Form("track==1&&det==%d&&emain>1000&&emain<10000%s",detnum,cut.Data()),"Q");
      h=(TH1D*)_file0->FindObject(Form("he10g_%d",detnum));
      N10GeV = h->GetEntries();
      mean10GeV=h->GetMean();

      tree->Draw(Form("emain>>he11g_%d",detnum),Form("track==1&&det==%d&&emain>10000%s",detnum,cut.Data()),"Q");
      h=(TH1D*)_file0->FindObject(Form("he11g_%d",detnum));
      N11GeV = h->GetEntries();
      mean11GeV=h->GetMean();

      cout<<detnum<<" "<<N1<<" "<<N10<<" "<<N100<<" "<<N1000<<" "<<N10GeV<<" "<<N11GeV<<" "<<Nall<<" "<<mean1<<" "<<mean10<<" "<<mean100<<" "<<mean1000<<" "<<mean10GeV<<" "<<mean11GeV<<" "<<meanall<<endl;
      if(Nall==0){
	countdet[ii]=0;
      }
      else{
	countdet[ii]=detnum;
	Ndet=Ndet+1;
      }

    }


      TCanvas *c0 = new TCanvas("c0","c0",100,100,1400*2,700*2);  
      c0->cd();
      gStyle->SetOptStat("eMRoui");
      c0->Divide(Ndet,3);
      int cjj=0;
      for(int ii=0;ii<7;ii++){
	if(countdet[ii]!=0){
	  c0->cd(cjj+1);
	  detnum=countdet[ii];
	  cut="";
	if(detnum==73){
	  cut=cut73;//"(emain<1100||emain>1600)";
	}
	if(detnum==74){
	  cut=cut74;//"(sqrt(pow(x,2)+pow(y,2))<518.16+30)";
	}
	if(detnum==75){
	  cut=cut75;//"(sqrt(pow(x,2)+pow(y,2))<523.24+30)";
	}
	tree->Draw(Form("emain>>he_%dplotemain",detnum),Form("track==1&&det==%d%s",detnum,cut.Data()));
	if(detnum==76){
          h=(TH1D*)_file0->FindObject(Form("he_%dplotemain",detnum));
	  h->GetXaxis()->SetRangeUser(0,11000);
	}
	c0->cd(cjj+1+Ndet);
	tree->Draw(Form("rmain>>he_%dplotrmain",detnum),Form("track==1&&det==%d%s",detnum,cut.Data()));
	if(detnum==76){
          h=(TH1D*)_file0->FindObject(Form("he_%dplotrmain",detnum));
	  h->GetXaxis()->SetRangeUser(0,11000);
	}
	c0->cd(cjj+1+Ndet*2);
	tree->Draw(Form("rmain:emain>>he_%dplot2dermain",detnum),Form("track==1&&det==%d%s",detnum,cut.Data()),"*");

	cjj++;
	}
      }

      TCanvas *c1 = new TCanvas("c1","c1",100,100,1400*2,700*2);  
      c1->cd();
      gStyle->SetOptStat("eMRoui");
      c1->Divide(Ndet,3);
      cjj=0;
      for(int ii=0;ii<7;ii++){
	if(countdet[ii]!=0){
	  c1->cd(cjj+1);
	  detnum=countdet[ii];
	  cut="";
	if(detnum==73){
	  cut=cut73;//"(emain<1100||emain>1600)";
	}
	if(detnum==74){
	  cut=cut74;//"(sqrt(pow(x,2)+pow(y,2))<518.16+30)";
	}
	if(detnum==75){
	  cut=cut75;//"(sqrt(pow(x,2)+pow(y,2))<523.24+30)";
	}
	tree->Draw(Form("z>>he_%dplotz",detnum),Form("track==1&&det==%d%s",detnum,cut.Data()));
	if(detnum==76){
          h=(TH1D*)_file0->FindObject(Form("he_%dplotz",detnum));
	  h->GetXaxis()->SetRangeUser(0,11000);
	}
	c1->cd(cjj+1)->SetLogy();
	c1->cd(cjj+1+Ndet);
	tree->Draw(Form("sqrt(pow(x,2)+pow(y,2))>>he_%dplotr",detnum),Form("track==1&&det==%d%s",detnum,cut.Data()));
	if(detnum==76){
          h=(TH1D*)_file0->FindObject(Form("he_%dplotr",detnum));
	  h->GetXaxis()->SetRangeUser(0,11000);
	}
	c1->cd(cjj+1+Ndet)->SetLogy();
	c1->cd(cjj+1+Ndet*2);
	tree->Draw(Form("sqrt(pow(x,2)+pow(y,2)):emain>>he_%dplot2dzr",detnum),Form("track==1&&det==%d%s",detnum,cut.Data()),"*");
	//	c1->cd(cjj+1+Ndet*2)->SetLogz();
	cjj++;
	}
      }



    //      h->SetTitle(Form("bellows %d E(MeV) 0-1MeV: %d, 1-10MeV: %d, 10-100MeV: %d, 0.1-10GeV: %d",b, N0_1MeV,N1_10MeV,N10_100MeV,N100_10GeV));
    //      h->Draw();
    //      c0->SetLogx();
    //      c0->SetLogy();

      //cout<<h->Integral(1,10);
      if(plot==1){

    TCanvas *c1 = new TCanvas("c1","c1",0,0,1600*1.5,850*1.5);
    c1->Divide(4,2);
    //    TExec* ex1 = new TExec("ex1","gStyle->SetOptStat(1111);");
    //    TExec* ex1 = new TExec("ex1",Form("gStyle->SetOptStat(%s);",""eMRoui""));
    //    ex1->Draw();
       gStyle->SetOptStat("eMRoui");

//        vector<string> hNms = {"emain"};

//     int logx[] = {1,0,0,0,
// 		1,0,0,0};
//     int logy[] = {1,0,1,1,
// 		1,0,1,1};

//     for(int i=0;i<hNms.size();i++){
//       c1->cd(1+i);
//       c1->cd(1+i)->SetLogx(logx[i]);
//       c1->cd(1+i)->SetLogy(logy[i]);
//       h=(TH1D*)d->Get(hNms[i].c_str());
//       h->Draw();
//        }

//     //2D plots
//     //    gStyle->SetOptStat(0);
//     TExec* ex2 = new TExec("ex2","gStyle->SetOptStat(0);");
//     TCanvas *c2a = new TCanvas("c2a","c2a",0,0,1600*1.5,650*1.5);
//     ex2->Draw();
//     c2a->Divide(4,2);
//     vector<string> hNms2a = {"hetheta","hetheta_cut15","hethetaVSz","hethetaVSz_cut15",
// "hetheta_ecut","hetheta_cut15ecut","hethetaVSz_ecut","hethetaVSz_cut15ecut"    };
//     int logx2a[] = {0,0,0,0,
// 		0,0,0,0};
//     int logy2a[] = {0,0,0,0,
// 		0,0,0,0};
//     int logz2a[] = {0,0,1,1,
// 		0,0,1,1};


//     TH2D *h2a;
//     for(int i=0;i<hNms2a.size();i++){
//       c2a->cd(1+i);
//       c2a->cd(1+i)->SetLogx(logx2a[i]);
//       c2a->cd(1+i)->SetLogy(logy2a[i]);
//       c2a->cd(1+i)->SetLogz(logz2a[i]);
//       h2a=(TH2D*)d->Get(hNms2a[i].c_str());
//       h2a->Draw("colz");
//       h2a->GetYaxis()->SetTitleOffset(1.5);
//        }


    //    gStyle->SetOptStat(0);


      }

}
