#include "anaConst.h"

void dumpDonutEdeposit(string fnm="../beamGeoV2_radAnaV4.root"){
  TFile *fin=TFile::Open(fnm.c_str(),"READ");
  if(!fin) return;

  TH1F *hB(nullptr), *hA, *hD;
  for(int i=0;i<nSpecies;i++){
    TH1F *hb = (TH1F*)fin->Get(Form("det43/d43_r_%s_Pg1_Dmg1",spH[i].c_str()));
    TH1F *ha = (TH1F*)fin->Get(Form("det44/d44_r_%s_Pg1_Dmg1",spH[i].c_str()));
    // TH1F *hb = (TH1F*)fin->Get(Form("det44/d44_r_%s_Pg1_Dmg1",spH[i].c_str()));
    // TH1F *ha = (TH1F*)fin->Get(Form("det45/d45_r_%s_Pg1_Dmg1",spH[i].c_str()));

    if(!hb || !ha){
      cout<<i<<endl;
      continue;
    }

    if(hB!=nullptr){
      hB->Add(hb);
      hA->Add(ha);
    }else{
      hB=(TH1F*)hb->Clone("before");
      hA=(TH1F*)hb->Clone("after");
      hB->SetTitle("Total kinetic energy before donut");
      hA->SetTitle("Total kinetic energy;r[mm];E [MeV]");
    }
  }
  hB->Scale(1./1000);
  hA->Scale(1./1000);

  hD=(TH1F*)hB->Clone("diff");
  hD->SetTitle("Total kinetic energy difference;r[mm];E [MeV]");
  hD->Add(hA,-1);

  double MeV2J = 1.602e-13;//Joules
  double beamCurrent = 65e-6;//uA
  double eOnTgtPerSecond = beamCurrent / (1.602e-19);

  auto *c1=new TCanvas();
  c1->Divide(2);
  c1->cd(1);
  hA->GetXaxis()->SetRangeUser(0,620);
  hA->SetLineColor(2);
  hA->SetLineWidth(3);
  hA->DrawCopy("h");
  hB->SetLineWidth(2);
  hB->DrawCopy("h&&same");
  gPad->SetGridx(1);
  gPad->SetGridy(1);
  gPad->SetLogy(1);

  c1->cd(2);
  hD->GetXaxis()->SetRangeUser(0,620);
  hD->SetLineWidth(3);
  hD->DrawCopy("h");
  gPad->SetGridx(1);
  gPad->SetGridy(1);
  //gPad->SetLogy(1);

  cout<<"before energy: "<<hB->Integral()<<" MeV "<< eOnTgtPerSecond*MeV2J*hB->Integral()<<endl;
  cout<<"after energy: "<<hA->Integral()<<" MeV "<< eOnTgtPerSecond*MeV2J*hA->Integral()<<endl;
  cout<<"Total energy: "<<hD->Integral()<<" MeV "<< eOnTgtPerSecond*MeV2J*hD->Integral()<<endl;
  fin->Close();
}
