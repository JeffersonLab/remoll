void plotConfig1D();
void plotConfig2D();
void plotCompare();
void plotCompareQ2();
void plotConfigRate();

void plotConfig(){
  //plotConfigRate();
  //plotConfig1D();
  //plotConfig2D();
  //plotCompare();
  plotCompareQ2();
}

/*
  rates for close/open/transition
  rates for all-primary
  spectra for ring7
*/

void plotCompare(){

  auto *c1=new TCanvas("c1","c1",1800,800);
  c1->Divide(2,2);
  gStyle->SetOptStat(0);

  string hNMs[4]={"det28rRateAsym_eP1","det28_rRateAsym_S0_eP1","det28_rRateAsym_S1_eP1","det28_rRateAsym_S2_eP1"};
  //string hNMs[4]={"det28rRate_eP1","det28_rRate_S0_eP1","det28_rRate_S1_eP1","det28_rRate_S2_eP1"};

  TFile *fin2=TFile::Open("../epInelastic_Segmented_shldAnaV4.root");
  TFile *fin1=TFile::Open("../epInelastic_UpdatedHybrid_shldAnaV4.root");
  for(int i=0;i<4;i++){
    c1->cd(i+1);
    auto *h1=(TH1D*)fin1->Get(Form("det28/%s",hNMs[i].c_str()));
    auto *h2=(TH1D*)fin2->Get(Form("det28/%s",hNMs[i].c_str()));

    h1->SetTitle(Form("%s B=hyb R=Seg",h1->GetTitle()));
    h1->SetLineColor(4);
    h1->SetLineWidth(2);
    h2->SetLineColor(2);
    h2->SetLineWidth(2);
    //cout<<h1->Integral()<<" "<<h2->Integral()<<endl;
    h1->DrawCopy("h");
    h2->DrawCopy("same&&h");
    //gPad->SetLogy(1);
    gPad->SetGridx(1);
    gPad->SetGridy(1);
  }
  fin1->Close();
  fin2->Close();
}
void plotCompareQ2(){

  auto *c1=new TCanvas("c1","c1",1800,800);
  c1->Divide(3,3);
  gStyle->SetOptStat("eou");

  TFile *fin1=TFile::Open("../epInelastic_UpdatedHybrid_shldAnaV6.root");
  TFile *fin2=TFile::Open("../epInelastic_Segmented_shldAnaV6.root");
  for(int i=0;i<3;i++){
    for(int j=0;j<3;j++){
      c1->cd(i*3+j+1);
      auto *h1=(TH1D*)fin1->Get(Form("det28/hQ2_R%d_S%d_primary e E>1",i+2,j));
      auto *h2=(TH1D*)fin2->Get(Form("det28/hQ2_R%d_S%d_primary e E>1",i+2,j));

      h1->SetTitle(Form("%s B=hyb R=Seg",h1->GetTitle()));
      h1->SetLineColor(4);
      h1->SetLineWidth(2);
      h2->SetLineColor(2);
      h2->SetLineWidth(2);
      //cout<<h1->Integral()<<" "<<h2->Integral()<<endl;
      h1->DrawCopy("h");
      h2->DrawCopy("same&&h");
      gPad->SetLogy(1);
      gPad->SetGridx(1);
      gPad->SetGridy(1);
    }
  }
  fin1->Close();
  fin2->Close();
}

void plotConfig1D(){
  string cfgNm[4]={"beam", "ee","epElastic","epInelastic"};

  auto *c1=new TCanvas("c1","c1",1800,800);
  c1->Divide(2,2);
  int cls[4]={0,2,4,3};
  int sty[4]={0,20,22,24};
  string hNMs[4]={"det28rRateAsym_eP1","det28_rRateAsym_S0_eP1","det28_rRateAsym_S1_eP1","det28_rRateAsym_S2_eP1"};
  //string hNMs[4]={"det28rRate_eP1","det28_rRate_S0_eP1","det28_rRate_S1_eP1","det28_rRate_S2_eP1"};
  gStyle->SetOptStat(0);
  for(int i=1;i<4;i++){

    TFile *fin=TFile::Open(Form("../%s_UpdatedHybrid_shldAnaV5.root",cfgNm[i].c_str()));
    //auto *h=(TH1D*)fin->Get("det28/hRate_g");
    //auto *h=(TH1D*)fin->Get("det28/hRate_eP1");
    for(int k=0;k<4;k++){
      auto *h=(TH1D*)fin->Get(Form("det28/%s",hNMs[k].c_str()));
      if(!h) continue;
      //h->SetTitle(cfgNm[i].c_str());
      h->SetMarkerColor(cls[i]);
      h->SetMarkerStyle(sty[i]);
      h->SetLineColor(cls[i]);
      h->SetLineWidth(3);
      h->GetXaxis()->SetRangeUser(600,1200);
      c1->cd(k+1);
      if(i==1)
	h->DrawCopy();
      else
	h->DrawCopy("same");
    
      gPad->SetLogy(1);
      gPad->SetGridx(1);
      gPad->SetGridy(1);
      delete h;
    }
    fin->Close();
  }
}
void plotConfigRate(){
  string cfgNm[3]={"ee","epElastic","epInelastic"};

  auto *c1=new TCanvas("c1","c1",1800,800);
  int cls[3]={2,4,3};
  int sty[3]={20,22,24};
  gStyle->SetOptStat(0);

  for(int i=0;i<3;i++){
    //TFile *fin=TFile::Open(Form("../%s_UpdatedHybrid_shldAnaV6.root",cfgNm[i].c_str()));
    TFile *fin=TFile::Open(Form("../%s_Segmented_shldAnaV6.root",cfgNm[i].c_str()));

    //auto *h=(TH1D*)fin->Get("det28/hRate_g");
    auto *h=(TH1D*)fin->Get("det28/hRateAsym_eP1");

    h->SetMarkerColor(cls[i]);
    h->SetMarkerStyle(sty[i]);
    h->SetLineColor(cls[i]);
    h->SetLineWidth(3);

    if(i==0)
      h->DrawCopy();
    else
      h->DrawCopy("same");
    
    gPad->SetLogy(1);
    gPad->SetGridx(1);
    gPad->SetGridy(1);
    fin->Close();
  }
}

void plotConfig2D(){
  string cfgNm[4]={"beam", "ee","epElastic","epInelastic"};

  auto *c1=new TCanvas("c1","c1");
  c1->Divide(2,2);
  gStyle->SetOptStat("ei");
  for(int i=0;i<4;i++){
    c1->cd(i+1);
    TFile *fin=TFile::Open(Form("../%s_shldAnaV3.root",cfgNm[i].c_str()));
    auto *h=(TH2D*)fin->Get("det28/det28Z0R0_e1");
    //auto *h=(TH2D*)fin->Get("det28/det28XYrate_n");
    //auto *h=(TH2D*)fin->Get("det28/hgXYrateEwght");
    //auto *h=(TH2D*)fin->Get("det28/hXYrate");
    //auto *h=(TH2D*)fin->Get("det9X/hZPhi_91");

    h->SetTitle(cfgNm[i].c_str());
    gPad->SetLogz(1);
    h->DrawCopy("colz");
    fin->Close();
  }
}
