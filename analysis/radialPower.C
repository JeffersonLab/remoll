#include "anaConst.h"

void radialPower(){
  
  TFile *fin=TFile::Open("shieldConf5_c1CollAnaSubV1.root");
  gStyle->SetOptStat("ni");

  TCanvas *c1[nSpecies];
  for(int i=0;i<nSpecies;i++){
    c1[i]=new TCanvas(Form("c%d",i),Form("%s",spTit[i].c_str()),1600,600);
    TH1F *rb = (TH1F*) fin->Get(Form("det5619/d5619__r_%s_pzG0_Dmg1",spH[i].c_str()));
    TH1F *ra = (TH1F*) fin->Get(Form("det5620/d5620__r_%s_pzG0_Dmg1",spH[i].c_str()));
    
    TH1F *rd = (TH1F*)rb->Clone(Form("rd_%s",rb->GetName()));
    rd->Add(ra,-1);
    c1[i]->Divide(3);
    c1[i]->cd(1);
    rb->GetXaxis()->SetRangeUser(260,2500);
    rb->SetLineColor(2);
    ra->SetLineColor(4);
    rb->SetLineWidth(3);
    rb->DrawCopy("hist");
    ra->DrawCopy("hist same");
    gPad->SetLogy(1);

    c1[i]->cd(2);
    rd->GetYaxis()->SetTitle("E [MeV]");
    rd->GetXaxis()->SetRangeUser(260,2500);
    rd->DrawCopy("hist");
    
    c1[i]->cd(3);
    TH2F *xy=(TH2F*)fin->Get(Form("det5619/d5619__xy_%s_pzG0_Dmg1",spH[i].c_str()));
    xy->DrawCopy("colz");
  }
  
  fin->Close();
}
