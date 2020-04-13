#include "anaConst.h"

TCanvas* plotCompare1D(vector<string> hNms,string cNm);
TCanvas* plotCompare1DMD(string hNms,string cNm);
TCanvas* plotCompare2D(vector<string> hNms,string cNm);
//void plot2D();

double scale=1/1000.;
TFile *fin=TFile::Open("../beam_radAnaV4.root","READ");

void plotRadAna(){
  // vector<string> hNms;
  // for(int i=0;i<nSpecies;i++)
    //hNms.push_back(Form("det28/d28_energyNEIL_R7_%s",spH[i].c_str()));
  //hNms.push_back(Form("det28/d28_energy_R7_%s",spH[i].c_str()));
    //hNms.push_back(Form("det28/d28_xy_R0_%s_Dmg0",spH[i].c_str()));
  
  //auto *c1=plotCompare1D(hNms,"kinE");
  //auto *c1=plotCompare2D(hNms,"xyHits");

  // auto *c1=plotCompare1DMD("det28/d28_z0","zSource");
  // return;

  TH2D* h=(TH2D*)fin->Get("det28/d28_z0r0_R0_g_Dmg0");
  if(h==nullptr) return;
  h->Scale(scale);
  h->GetZaxis()->SetRangeUser(1e-8,1e3);
  h->DrawCopy("colz");
  gPad->SetLogz(1);
  //plotCompare2D();
}

TCanvas* plotCompare1D(vector<string> hNms,string cNm){

  auto *c1=new TCanvas(cNm.c_str(),cNm.c_str(),1800,800);
  gStyle->SetOptStat(0);

  int cls[6]={2,6,1,4,7,3};

  for(int i=0;i<hNms.size();i++){
    TH1D *h=(TH1D*)fin->Get(hNms[i].c_str());
    if(h==nullptr) {
      cout<<"can't find "<<hNms[i]<<endl;
      continue;
    }
    h->SetLineWidth(2);
    h->SetLineColor(cls[i]);
    h->SetMarkerColor(cls[i]);

    //h->GetYaxis()->SetRangeUser(1e-9,1e-4);
    h->Scale(scale);

    if(i==0)
      h->DrawCopy("h");
    else
      h->DrawCopy("same&&h");

  }
  gPad->SetGridx(1);
  gPad->SetGridy(1);
  gPad->SetLogx(1);
  gPad->SetLogy(1);
  return c1;
}

TCanvas* plotCompare1DMD(string hNms,string cNm){

  auto *c1=new TCanvas(cNm.c_str(),cNm.c_str(),1800,800);
  gStyle->SetOptStat(0);

  int cls[6]={2,3,1,4,6,7};

  for(int i=0;i<nSpecies;i++){
    TH1D *h=(TH1D*)fin->Get(Form("%s_R1_%s_Dmg1",hNms.c_str(),spH[i].c_str()));
    if(h==nullptr) {
      cout<<"can't find "<<Form("%s_R1_%s_Dmg1",hNms.c_str(),spH[i].c_str())<<endl;
      continue;
    }
    for(int j=2;j<=7;j++)
      h->Add((TH1D*)fin->Get(Form("%s_R%d_%s_Dmg1",hNms.c_str(),j,spH[i].c_str())));

    h->SetLineWidth(2);
    h->SetLineColor(cls[i]);
    h->SetMarkerColor(cls[i]);

    h->GetYaxis()->SetRangeUser(1e-9,1e-1);
    h->Scale(scale);

    if(i==0)
      h->DrawCopy("h");
    else
      h->DrawCopy("same&&h");

  }
  gPad->SetGridx(1);
  gPad->SetGridy(1);
  // gPad->SetLogx(1);
  gPad->SetLogy(1);
  return c1;
}

TCanvas* plotCompare2D(vector<string> hNms,string cNm){

  auto *c1=new TCanvas(cNm.c_str(),cNm.c_str(),1800,800);
  c1->Divide(3,2);
  gStyle->SetOptStat(0);

  for(int i=0;i<hNms.size();i++){
    c1->cd(i+1);
    TH2D *h=(TH2D*)fin->Get(hNms[i].c_str());
    h->Scale(scale);
    h->DrawCopy("colz");

  }
  gPad->SetGridx(1);
  gPad->SetGridy(1);
  gPad->SetLogz(1);
  return c1;
}

