#include "anaConst.h"
#include "histogramUtilities.h"

TCanvas *ringRates(vector<string> hNms,string cNm, TFile *fin);
TCanvas *ratioRingRates(vector<string> hNms, TFile *fnum, TFile *fden);

const double scale=1;

void compareMD(){
  vector<string> hNms;
  for(int i=0;i<nSpecies;i++)
    hNms.push_back(Form("det28/d28_mdHits_%s_ER0_Dmg0",spH[i].c_str()));

  TFile *fin1=TFile::Open("./tgtShld_conf1_tgtDSanaV0.root","READ");
  TFile *fin2=TFile::Open("./tgtShld_conf3_tgtDSanaV0.root","READ");

  auto c1=ringRates(hNms,"conf1",fin1);
  auto c2=ringRates(hNms,"conf3",fin2);
  auto c3=ratioRingRates(hNms,fin2,fin1);
}

TCanvas *ringRates(vector<string> hNms,string cNm, TFile *fin){
  auto *c1=new TCanvas(cNm.c_str(),cNm.c_str(),1900,800);
  gStyle->SetOptStat(0);

  for(int i=0;i<hNms.size();i++){
    TH1D *h=(TH1D*)fin->Get(hNms[i].c_str());
    if(h==nullptr) {
      cout<<"can't find "<<hNms[i]<<endl;
      continue;
    }
    string hT=h->GetTitle();
    h->SetTitle(hT.substr(hT.find("for")+4,hT.length()).c_str());
    
    h->SetLineWidth(3);
    h->SetLineColor(spCls[i]);
    h->SetMarkerColor(spCls[i]);
    h->Scale(scale);

    if(i==0){
      h->GetYaxis()->SetTitle("hits per electron on target");
      h->GetYaxis()->SetRangeUser(1e-9,1e-1);
      h->DrawCopy("h");
    }else{
      h->DrawCopy("same&&h");
    }
  }

  gPad->BuildLegend(0.3,0.1,0.7,0.35);
  gPad->SetGridx(1);
  gPad->SetGridy(1);
  gPad->SetLogy(1);

  return c1;
}

TCanvas *ratioRingRates(vector<string> hNms,TFile *fnum, TFile *fden){
  auto *c1=new TCanvas("ratio","rate ratio",1900,800);
  gStyle->SetOptStat(0);

  for(int i=0;i<hNms.size();i++){
    TH1D *h1=(TH1D*)fden->Get(hNms[i].c_str());
    TH1D *h2=(TH1D*)fnum->Get(hNms[i].c_str());
    if(h1==nullptr || h2==nullptr) {
      cout<<"can't find "<<hNms[i]<<endl;
      continue;
    }
    TH1D *h=(TH1D*)h2->Clone(Form("ratio%s",h2->GetName()));
    string hT=h->GetTitle();
    h->SetTitle(hT.substr(hT.find("for")+1,hT.length()).c_str());
    
    h->Divide(h1);
    h->SetLineWidth(2);
    h->SetLineColor(spCls[i]);
    h->SetMarkerColor(spCls[i]);
    h->Scale(scale);
    if(i==0){
      h->GetYaxis()->SetTitle("rate ratio");
      h->GetYaxis()->SetRangeUser(0,4);
      h->DrawCopy("h");
    }else{
      h->DrawCopy("same&&h");
    }
  }

  gPad->BuildLegend(0.3,0.9,0.7,0.55);
  gPad->SetGridx(1);
  gPad->SetGridy(1);

  return c1;
}

