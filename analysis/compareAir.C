const int nProc=3;
string procNm[nProc]={"Moller","epElastic","epInelastic"};
TCanvas *can[nProc];
void drawOneProc(string process,TCanvas *c1);

void compareAir(){
  gStyle->SetOptStat(0);
  for(int i=0;i<nProc;i++){
    can[i]=new TCanvas(Form("c_%d",i),Form("%s",procNm[i].c_str()),1600,800);
    drawOneProc(procNm[i],can[i]);
  }
}

void drawOneProc(string process,TCanvas *c1){
  c1->Divide(2,2);
  TFile *f[3];
  TH1D *hr[3], *dr[3];
  TH1D *ha[3], *da[3];
  string condition[3]={"Vac","Air","Can"};
  if(process == "epElastic" || process == "epInelastic"){
    condition[1]="beamline_Air";
    condition[2]="beamline_Can";
  }
  string drawOpt[3]={"h","h&&same","h&&same"};
  int color[3]={1,2,4};
  int marker[3]={20,21,24};
  double rates[3]={0,0,0};

  for(int i=0;i<3;i++){
    f[i]=TFile::Open(Form("../output/remollout_VacTst_%s_%s_detAna.root",process.c_str(),condition[i].c_str()),"READ");
    hr[i]=(TH1D*)f[i]->Get("QA/rRate");
    hr[i]->Rebin(2);
    hr[i]->GetXaxis()->SetRangeUser(0.6,1.3);
    hr[i]->SetMarkerColor(color[i]);
    hr[i]->SetLineColor(color[i]);
    hr[i]->SetMarkerStyle(marker[i]);
    c1->cd(1);
    hr[i]->DrawCopy(drawOpt[i].c_str());
    int b1 = hr[i]->GetXaxis()->FindBin(0.69);
    int b2 = hr[i]->GetXaxis()->FindBin(1.20);
    rates[i] = hr[i]->Integral(b1,b2);

    ha[i]=(TH1D*)f[i]->Get("QA/rRateAsym");
    ha[i]->Rebin(2);
    ha[i]->GetXaxis()->SetRangeUser(0.6,1.3);
    ha[i]->SetMarkerColor(color[i]);
    ha[i]->SetLineColor(color[i]);
    ha[i]->SetMarkerStyle(marker[i]);
    c1->cd(2);
    ha[i]->DrawCopy(drawOpt[i].c_str());

    dr[i]=(TH1D*)hr[i]->Clone(Form("dr_%s",condition[i].c_str()));
    dr[i]->Add(hr[0],-1.);
    dr[i]->Divide(hr[0]);
    dr[i]->SetTitle("percent difference (this - vac)/vac");
    dr[i]->GetXaxis()->SetRangeUser(0.6,1.3);
    dr[i]->SetMarkerColor(color[i]);
    dr[i]->SetLineColor(color[i]);
    dr[i]->SetMarkerStyle(marker[i]);
    c1->cd(3);
    dr[i]->DrawCopy(drawOpt[i].c_str());

    da[i]=(TH1D*)ha[i]->Clone(Form("da_%s",condition[i].c_str()));
    da[i]->SetTitle("percent difference (this - vac)/vac");
    da[i]->GetXaxis()->SetRangeUser(0.6,1.3);
    da[i]->Add(ha[0],-1.);
    da[i]->Divide(ha[0]);
    da[i]->SetMarkerColor(color[i]);
    da[i]->SetLineColor(color[i]);
    da[i]->SetMarkerStyle(marker[i]);
    c1->cd(4);
    da[i]->DrawCopy(drawOpt[i].c_str());
  }
  cout<<process<<"\tRates (V,A,C):\t"<<rates[0]<<"\t"<<rates[1]<<"\t"<<rates[2]<<endl;
  cout<<process<<"\tratios A/V C/V:\t"<<rates[1]/rates[0]<<"\t"<<rates[2]/rates[0]<<endl;
}
