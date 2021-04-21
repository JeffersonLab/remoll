#include "histogramUtilities.h"

TH2D* getPlot2D(string fnm, string hnm,string thnm);

void mdRadiationDose(){

  string hnm = "d28_xy";

  TH2D *hee=getPlot2D("../ee_UpdatedHybrid_radAnaV3.root",hnm,"ee");
  TH2D *hep=getPlot2D("../epElastic_UpdatedHybrid_radAnaV3.root",hnm,"ep");
  
  TH2D *ht = (TH2D*)hee->Clone("htotal");
  ht->Add(hep);

  int logz=0;
  double zlow=0;
  double zhigh = 5e7;
  int fixedZ = 0;

  int septantPlot=1;
  int drawQuartz=1; 
  double xlow = -1200;
  double xhigh = -600;
  double ylow = -500;
  double yhigh = 500;

  double mipF = 2.4; // MeV/(g/cm2)
  double PACdays = 235  + 95 + 14; 
  double h2s = 3600;
  double hIn1day = 24;
  double area = 0.5*0.5; //cm2
  double MeV2rad = 100/6.24e9; //100rad = 1 Gy = 6.24 10^12 MeV/kg
  double doseScale = mipF * PACdays * hIn1day * h2s / area * MeV2rad / 1e6; // [Mrad]

  auto *c1=new TCanvas();
  c1->Divide(2,2);
  hee->SetTitle("rate per 5x5 mm2 ee events");
  hep->SetTitle("rate per 5x5 mm2 ep Elastic events");
  ht->SetTitle("rate per 5x5 mm2 ee+(ep Elastic) events");

  c1->cd(1);
  if(fixedZ) hee->GetZaxis()->SetRangeUser(zlow,zhigh);
  if(septantPlot){
    hee->GetXaxis()->SetRangeUser(xlow,xhigh);
    hee->GetYaxis()->SetRangeUser(ylow,yhigh);
  }
  hee->DrawCopy("colz");
  if(septantPlot && drawQuartz)
    drawQuartzOutline(c1->GetPad(1));
  gPad->SetGridx(1);
  gPad->SetGridy(1);
  if(logz) gPad->SetLogz(1);

  c1->cd(2);
  if(fixedZ) hep->GetZaxis()->SetRangeUser(zlow,zhigh);
  if(septantPlot){
    hep->GetXaxis()->SetRangeUser(xlow,xhigh);
    hep->GetYaxis()->SetRangeUser(ylow,yhigh);
  }
  hep->DrawCopy("colz");
  if(septantPlot && drawQuartz)
    drawQuartzOutline(c1->GetPad(2));
  gPad->SetGridx(1);
  gPad->SetGridy(1);
  if(logz) gPad->SetLogz(1);

  c1->cd(3);
  if(fixedZ) ht->GetZaxis()->SetRangeUser(zlow,zhigh);
  if(septantPlot){
    ht->GetXaxis()->SetRangeUser(xlow,xhigh);
    ht->GetYaxis()->SetRangeUser(ylow,yhigh);
  }
  ht->DrawCopy("colz");
  if(septantPlot && drawQuartz)
    drawQuartzOutline(c1->GetPad(3));
  gPad->SetGridx(1);
  gPad->SetGridy(1);
  if(logz) gPad->SetLogz(1);

  auto *c2=new TCanvas();
  c2->Divide(2,2);
  hee->SetTitle("dose in Mrad per 5x5 mm2 ee events");
  hep->SetTitle("dose in Mrad per 5x5 mm2 ep Elastic events");
  ht->SetTitle("dose in Mrad per 5x5 mm2 ee+(ep Elastic) events");
  hee->Scale(doseScale);
  hep->Scale(doseScale);
  ht->Scale(doseScale);

  c2->cd(1);
  if(fixedZ) hee->GetZaxis()->SetRangeUser(zlow,zhigh);
  if(septantPlot){
    hee->GetXaxis()->SetRangeUser(xlow,xhigh);
    hee->GetYaxis()->SetRangeUser(ylow,yhigh);
  }
  hee->DrawCopy("colz");
  if(septantPlot && drawQuartz)
    drawQuartzOutline(c2->GetPad(1));
  gPad->SetGridx(1);
  gPad->SetGridy(1);
  if(logz) gPad->SetLogz(1);

  c2->cd(2);
  if(fixedZ) hep->GetZaxis()->SetRangeUser(zlow,zhigh);
  if(septantPlot){
    hep->GetXaxis()->SetRangeUser(xlow,xhigh);
    hep->GetYaxis()->SetRangeUser(ylow,yhigh);
  }
  hep->DrawCopy("colz");
  if(septantPlot && drawQuartz)
    drawQuartzOutline(c2->GetPad(2));
  gPad->SetGridx(1);
  gPad->SetGridy(1);
  if(logz) gPad->SetLogz(1);

  c2->cd(3);
  if(fixedZ) ht->GetZaxis()->SetRangeUser(zlow,zhigh);
  if(septantPlot){
    ht->GetXaxis()->SetRangeUser(xlow,xhigh);
    ht->GetYaxis()->SetRangeUser(ylow,yhigh);
  }
  ht->DrawCopy("colz");
  if(septantPlot && drawQuartz)
    drawQuartzOutline(c2->GetPad(3));
  gPad->SetGridx(1);
  gPad->SetGridy(1);
  if(logz) gPad->SetLogz(1);

}

TH2D* getPlot2D(string fnm, string hnm,string thnm){
  TFile *fin=TFile::Open(fnm.c_str(),"READ");
  
  TH2D *h1=(TH2D*)fin->Get(Form("det28/%s_R1_eP1_Dmg0",hnm.c_str()));
  TH2D *h=(TH2D*)h1->Clone(thnm.c_str());
  for(int i=2;i<7;i++){
    TH2D *ht=(TH2D*)fin->Get(Form("det28/%s_R%d_eP1_Dmg0",hnm.c_str(),i));
    h->Add(ht);
  }
  h->SetDirectory(0);
  fin->Close();
  return h;
}
