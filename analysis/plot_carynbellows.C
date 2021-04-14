void plot_carynbellows(int b = 70, int plot=0){

   //Tfile
  //TString filename="/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_100M/remoll_bellows_beam_100M_100kEv";

  TString filename = "__carynplots.root";
  TFile *_file0 = TFile::Open(Form("det%d%s",b,filename.Data()));
  TDirectory *d = _file0->GetDirectory("QA");
  //Get energy info

    TCanvas *c0 = new TCanvas("c0","c0",100,100,700,700);  
    c0->cd();
    gStyle->SetOptStat("eMRoui");

    TH1D *h;
    //get energy info
      h=(TH1D*)d->Get("hE");
      double Nbins=1500;//1000
      double Min=0;
      double Max=1500;//1500
      if(b==28){
	Nbins=11000;
	Max=11000;
      }
      if(b==70){
	Nbins=6000;
	Max=6000;
      }
      double scale = 1;//set Nbins=Max in basicana (Max-Min)/Nbins;
      //bin#*scale+Min=#MeV
      //to get bin# for 10MeV, take (10MeV-Min)/scale= (10MeV-Min) * Nbins/(Max-Min)= 10MeV * 1000/1500
      int bin10MeV=(10-Min)/scale;
      int bin1MeV=(1-Min)/scale;
      int bin100MeV=(100-Min)/scale;
      int bin1500MeV=(1500-Min)/scale;
      int bin3000MeV=(3000-Min)/scale;
      int bin10GeV=(10000-Min)/scale;
      int bin10950MeV=(10950-Min)/scale;

      cout<<"bellows "<<b<<endl;
      cout<<"bin# 1MeV="<<bin1MeV<<" 10MeV="<<bin10MeV<<" 100MeV="<<bin100MeV<<" 1500MeV="<<bin1500MeV<<endl;
      cout<<"N@ 1MeV="<<h->GetBinContent(bin1MeV)<<" 10MeV="<<h->GetBinContent(bin10MeV)<<" 100MeV="<<h->GetBinContent(bin100MeV)<<" 1500MeV="<<h->GetBinContent(bin1500MeV)<<endl;
      cout<<"Entries="<<Form("%6.0f",h->GetEntries())<<" Overflow="<<h->GetBinContent(Nbins+1)<<" Underflow="<<h->GetBinContent(-1)<<endl;
      cout<<"0-1MeV: "<<h->Integral(-1,bin1MeV)<<" 1-10MeV:"<<Form("%6.0f",h->Integral(bin1MeV+1,bin10MeV))<<" 10-100MeV:"<<Form("%6.0f",h->Integral(bin10MeV+1,bin100MeV))<<" 100MeV-10GeV:"<<Form("%6.0f",h->Integral(bin100MeV+1,bin10GeV))<<" 10GeV-10950MeV:"<<Form("%6.0f",h->Integral(bin10GeV+1,bin10950MeV))<<" >10950MeV:"<<Form("%6.0f",h->Integral(bin10950MeV+1,Nbins+1))<<endl;
      int N0_1MeV = h->Integral(-1,bin1MeV);
      int N1_10MeV = h->Integral(bin1MeV+1,bin10MeV);
      int N10_100MeV = h->Integral(bin10MeV+1,bin100MeV);
      int Ngt100MeV = h->Integral(bin100MeV+1,Nbins+1);
      int N100_10GeV = h->Integral(bin100MeV+1,bin10GeV);
      int N10000_10950MeV = h->Integral(bin10GeV+1,bin10950MeV);
      int Ngt10950MeV =h->Integral(bin10950MeV+1,Nbins+1);

      h->SetTitle(Form("bellows %d E(MeV) 0-1MeV: %d, 1-10MeV: %d, 10-100MeV: %d, 0.1-10GeV: %d",b, N0_1MeV,N1_10MeV,N10_100MeV,N100_10GeV));
      h->Draw();
      c0->SetLogx();
      c0->SetLogy();

      //cout<<h->Integral(1,10);
      if(plot==1){

    TCanvas *c1 = new TCanvas("c1","c1",0,0,1600,850);
    c1->Divide(4,2);
    //    TExec* ex1 = new TExec("ex1","gStyle->SetOptStat(1111);");
    //    TExec* ex1 = new TExec("ex1",Form("gStyle->SetOptStat(%s);",""eMRoui""));
    //    ex1->Draw();
       gStyle->SetOptStat("eMRoui");

    vector<string> hNms = {"hE","hphi","hthetaP","hradius",
			   "hE_ecut","hphi_ecut","hthetaP_ecut","hradius_ecut"};

    int logx[] = {1,0,0,0,
		1,0,0,0};
    int logy[] = {1,0,1,1,
		1,0,1,1};

    for(int i=0;i<hNms.size();i++){
      c1->cd(1+i);
      c1->cd(1+i)->SetLogx(logx[i]);
      c1->cd(1+i)->SetLogy(logy[i]);
      h=(TH1D*)d->Get(hNms[i].c_str());
      h->Draw();
       }

    //2D plots
    //    gStyle->SetOptStat(0);
    TExec* ex2 = new TExec("ex2","gStyle->SetOptStat(0);");
    TCanvas *c2a = new TCanvas("c2a","c2a",0,0,1600,650);
    ex2->Draw();
    c2a->Divide(4,2);
    vector<string> hNms2a = {"hetheta","hetheta_cut15","hethetaVSz","hethetaVSz_cut15",
"hetheta_ecut","hetheta_cut15ecut","hethetaVSz_ecut","hethetaVSz_cut15ecut"    };
    int logx2a[] = {0,0,0,0,
		0,0,0,0};
    int logy2a[] = {0,0,0,0,
		0,0,0,0};
    int logz2a[] = {0,0,1,1,
		0,0,1,1};


    TH2D *h2a;
    for(int i=0;i<hNms2a.size();i++){
      c2a->cd(1+i);
      c2a->cd(1+i)->SetLogx(logx2a[i]);
      c2a->cd(1+i)->SetLogy(logy2a[i]);
      c2a->cd(1+i)->SetLogz(logz2a[i]);
      h2a=(TH2D*)d->Get(hNms2a[i].c_str());
      h2a->Draw("colz");
      h2a->GetYaxis()->SetTitleOffset(1.5);
       }


    //    gStyle->SetOptStat(0);

    TCanvas *c2b = new TCanvas("c2b","c2b",0,0,1600,650);
    c2b->Divide(4,2);
    ex2->Draw();
    vector<string> hNms2b = {"hzfront","hzback","hrin","hrout",
			     "hzfront_ecut","hzback_ecut","hrin_ecut","hrout_ecut"};
    int logx2b[] = {0,0,0,0,
		0,0,0,0};
    int logy2b[] = {0,0,0,0,
		0,0,0,0};

    TH2D *h2b;
    for(int i=0;i<hNms2b.size();i++){
      c2b->cd(1+i);
      c2b->cd(1+i)->SetLogx(logx2b[i]);
      c2b->cd(1+i)->SetLogy(logy2b[i]);
      h2b=(TH2D*)d->Get(hNms2b[i].c_str());
      h2b->Draw("colz");
      h2b->GetYaxis()->SetTitleOffset(1);
       }

    //new plots
    TCanvas *c2c = new TCanvas("c2c","c2c",0,0,1600,650);
    ex2->Draw();
    c2c->Divide(4,2);
    vector<string> hNms2c = {"heVSz","heVSr",
"heVSz_ecut","heVSr_ecut" };
    int logx2c[] = {0,0,0,0};
    int logy2c[] = {0,0,0,0};
    int logz2c[] = {1,1,1,1};


    TH2D *h2c;
    for(int i=0;i<hNms2c.size();i++){
      c2c->cd(1+i);
      c2c->cd(1+i)->SetLogx(logx2c[i]);
      c2c->cd(1+i)->SetLogy(logy2c[i]);
      c2c->cd(1+i)->SetLogz(logz2c[i]);
      h2c=(TH2D*)d->Get(hNms2c[i].c_str());
      h2c->Draw("colz");
      h2c->GetYaxis()->SetTitleOffset(1.5);
       }


  
  //TH1D *h = (TH1D*)fin->Get(hNms[i].c_str());
  //h->SetLineWidth(1);
  //h->GetYaxis()->SetRangeUser(1,1500);
  //h->DrawCopy("h");
  //gPad->SetGridx(1);
  //gPad->SetLogx(1);

  //QA tree
  //    TTree *QA = (TTree*)_file0->Get("QA");
  //Canvas
    // c1->cd();
    // QA->cd();
    // hE->Draw();
    // c1->SetLogy();
    // c1->SetLogx();

    // c1->SetLogx(0);

    // c1->SetLogy(0);
    // hphi->Draw();

    // hthetaP->Draw();
    // c1->SetLogy();

    // hradius->Draw();

    // c1->Logy(0);
    // hetheta->Draw("colz");
    // c1->Logx();
    // c1->Logy();

    // c1->Logx(0);
    // c1->Logy(0);
    // hzfront->Draw("colz");

    // hzback->Draw("colz");

    // hrin->Draw("colz");

    // hrout->Draw("colz");

    
    //      TH1D* hE = (TH1D*)gDirectory->FindObject("hE");
    //	hE->Draw();


  //hE->Draw()??
    //_file0->cd("QA");
    //     QA->Get("hE");
    //  _file0->Get("hE");
    //       qa->FindObject("hE");
    //TH1D* h =   (TH1D*)qa->Get("hE");
     //     QA->Draw("hE");
    //    _file0->Draw("hE");
     // qa->Draw("h");
    //    qa->h.Draw();
    //       c1->SetLogy();
    //       c1->SetLogx();
  //savePDF

      }

}
