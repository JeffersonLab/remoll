#include "anaConst.h"
#include "histogramUtilities.h"

TCanvas* plotCompare1D(vector<string> hNms,string cNm);
TCanvas* plotCompare1DMD(string hNms,string cNm);
TCanvas* plotCompare2D(vector<string> hNms,string cNm);
TCanvas *plot2DMD(string hNms, string cNm, string sp);
TCanvas *rMD(string hXY);
TCanvas *plotMDringRates(vector<string> hNms,string cNm);
TCanvas *plotRatioMDringRates(vector<string> hNms,string cNm);

//const double scale=1/1000.;
//TFile *fin=TFile::Open("../beamGeoV1_radAnaV4.root","READ");
//TFile *fin=TFile::Open("../beamGeoV2_radAnaV4.root","READ");
//TFile *fin=TFile::Open("../beamGeoV2_radAnaV4_correctMD.root","READ");

const double scale=1;
TFile *fin2=TFile::Open("/phenix/spin/phnxsp01/ciprian/mollerOut/targetShieldStudy_Sep21/radAnaV5_Config1.root","READ");
//TFile *fin2=TFile::Open("/phenix/spin/phnxsp01/ciprian/mollerOut/targetShieldStudy_Nov21/radAnaV5_Config2.root","READ");
TFile *fin=TFile::Open("/phenix/spin/phnxsp01/ciprian/mollerOut/targetShieldStudy_Jan23_Config3/tgtShld_conf3_radAnaV5.root","READ");

void plotRadAna(){
  vector<string> hNms;
  for(int i=0;i<nSpecies;i++)
    hNms.push_back(Form("det28/d28_mdHits_%s_ER0_Dmg0",spH[i].c_str()));
  //hNms.push_back(Form("det45/d45_r_%s_Pg1_Dmg0",spH[i].c_str()));
  //hNms.push_back(Form("det45/d45_energy_Pl1_%s",spH[i].c_str()));
    //hNms.push_back(Form("det28/d28_energyNEIL_R7_%s",spH[i].c_str()));
    //hNms.push_back(Form("det28/d28_energy_R7_%s",spH[i].c_str()));
  //hNms.push_back(Form("det28/d28_xy_R0_%s_Dmg0",spH[i].c_str()));
  
  auto *c1=plotMDringRates(hNms,"kinE");
  //auto *c1=plotRatioMDringRates(hNms,"kinE");
  //auto *c1=plotCompare1D(hNms,"kinE");
  //auto *c1=plotCompare1D(hNms,"kinE");
  //auto *c1=plotCompare2D(hNms,"xyHits");

  //auto *c1=plotCompare1DMD("det28/d28_z0","zSource");
  // return;

  // TH2D* h=(TH2D*)fin->Get("det28/d28_z0x0_R7_e1_Dmg0");
  // //TH2D* h=(TH2D*)fin->Get("hallDet/dHL_Wall_z0x0_g_Dmg0");
  // //TH2D* h=(TH2D*)fin->Get("hallDet/dHL_Wall_yPhi_e1_Dmg0");
  // //TH2D* h=(TH2D*)fin->Get("hallDet/dHL_Roof_zx_n_Dmg0");
  // if(h==nullptr) return;
  // h->Scale(scale);
  // gStyle->SetOptStat("i");
  // //h->GetZaxis()->SetRangeUser(1e-9,1e-3);
  // h->DrawCopy("colz");
  // gPad->SetLogz(1);

  //auto *c1=rMD("det28/d28_xy_R7_e1_Dmg1");

  //auto *c1=plot2DMD("det28/d28_z0x0","z0gamma","e1_Dmg1");
}

TCanvas *plotMDringRates(vector<string> hNms,string cNm){

  auto *c1=new TCanvas(cNm.c_str(),cNm.c_str(),1800,800);
  c1->Divide(1);

  gStyle->SetOptStat(0);

  for(int i=0;i<hNms.size();i++){
    TH1D *h=(TH1D*)fin->Get(hNms[i].c_str());
    if(h==nullptr) {
      cout<<"can't find "<<hNms[i]<<endl;
      continue;
    }
    h->SetLineWidth(2);
    h->SetLineColor(spCls[i]);
    h->SetMarkerColor(spCls[i]);
    h->Scale(scale);

    if(i==0){
      h->GetYaxis()->SetTitle("hits per electron on target");
      h->GetYaxis()->SetRangeUser(1e-9,5e-3);
      h->DrawCopy("h");
    }else{
      h->DrawCopy("same&&h");
    }
  }

  gPad->SetGridx(1);
  gPad->SetGridy(1);
  gPad->SetLogy(1);

  return c1;
}

TCanvas *plotRatioMDringRates(vector<string> hNms,string cNm){

  auto *c1=new TCanvas(cNm.c_str(),cNm.c_str(),1800,800);
  c1->Divide(1);

  gStyle->SetOptStat(0);

  for(int i=0;i<hNms.size();i++){
    TH1D *h1=(TH1D*)fin->Get(hNms[i].c_str());
    TH1D *h2=(TH1D*)fin2->Get(hNms[i].c_str());
    if(h1==nullptr || h2==nullptr) {
      cout<<"can't find "<<hNms[i]<<endl;
      continue;
    }
    TH1D *h=(TH1D*)h2->Clone(Form("ratio%s",h2->GetName()));

    h->Divide(h1);
    h->SetLineWidth(2);
    h->SetLineColor(spCls[i]);
    h->SetMarkerColor(spCls[i]);
    h->Scale(scale);
    // cout<<i<<" "<<h1->GetTitle()<<endl;
    //h->Fit("pol0");
    // cout<<endl;
    if(i==0){
      h->GetYaxis()->SetTitle("hits per electron on target");
      h->GetYaxis()->SetRangeUser(0,10);
      h->DrawCopy("h");
    }else{
      h->DrawCopy("same&&h");
    }
  }

  gPad->SetGridx(1);
  gPad->SetGridy(1);
  //gPad->SetLogy(1);

  return c1;
}

TCanvas* plotCompare1D(vector<string> hNms,string cNm){

  auto *c1=new TCanvas(cNm.c_str(),cNm.c_str(),1800,800);
  c1->Divide(1,2);

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

    //h->GetXaxis()->SetRangeUser(0,320);
    h->Scale(scale);

    TH1F *h2=(TH1F*)dNdXscaleLogX(h);
    
    if(i==0){
      c1->cd(1);
      h->GetYaxis()->SetTitle("hits in each bin");
      h->GetYaxis()->SetRangeUser(1e-9,1e-3);
      h->DrawCopy("h");
      c1->cd(2);
      h2->GetYaxis()->SetRangeUser(1e-9,1e+3);
      h2->DrawCopy("h");
    }else{
      c1->cd(1);
      h->DrawCopy("same&&h");
      c1->cd(2);
      h2->DrawCopy("same&&h");
    }
  }
  for(int i=1;i<3;i++){
    c1->cd(i);
    gPad->SetGridx(1);
    gPad->SetGridy(1);
    gPad->SetLogx(1);
    gPad->SetLogy(1);
  }
  return c1;
}

TCanvas* rMD(string hNm){

  auto *c1=new TCanvas(hNm.c_str(),hNm.c_str(),1800,800);
  gStyle->SetOptStat(0);

  TH2F *h=(TH2F*)fin->Get(hNm.c_str());
  if(h==nullptr) {
    cout<<"can't find "<<hNm<<endl;
    return nullptr;
  }

  TH1F *hr=new TH1F("hr","radial distribution of hits",
		    200,1100,1600);

  int xBins = h->GetXaxis()->GetNbins();
  int yBins = h->GetYaxis()->GetNbins();

  for(int i=1;i<=xBins;i++)
    for(int j=1;j<=yBins;j++){
      double xx = h->GetXaxis()->GetBinCenter(i);
      double yy = h->GetYaxis()->GetBinCenter(j);
      double r = sqrt(xx*xx + yy*yy);
      double val = h->GetBinContent(i,j);
      if(!std::isnan(val))
	 hr->Fill(r,val);
    }

  c1->Divide(2);
  c1->cd(1);
  h->DrawCopy("colz");
  gPad->SetGridx(1);
  gPad->SetGridy(1);
  c1->cd(2);
  hr->DrawCopy("h");
  gPad->SetLogy(1);
  gPad->SetGridx(1);
  gPad->SetGridy(1);
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

TCanvas* plot2DMD(string hNms,string cNm, string sp){

  auto *c1=new TCanvas(cNm.c_str(),cNm.c_str(),3000,800);
  gStyle->SetOptStat("i");

  TH2D *h=(TH2D*)fin->Get(Form("%s_R1_%s",hNms.c_str(),sp.c_str()));
  if(h==nullptr) {
    cout<<"can't find "<<Form("%s_R1_%s",hNms.c_str(),sp.c_str())<<endl;
    return c1;
  }
  for(int j=2;j<=7;j++)
    h->Add((TH1D*)fin->Get(Form("%s_R%d_%s",hNms.c_str(),j,sp.c_str())));
  
  h->Scale(scale);

  h->GetZaxis()->SetRangeUser(1e-7,1e-2);
  h->DrawCopy("colz");

  gPad->SetGridx(1);
  gPad->SetGridy(1);
  gPad->SetLogz(1);

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

