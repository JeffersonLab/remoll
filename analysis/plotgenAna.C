#include "anaConst.h"
float scale = 1./1000;
//TFile *fin = TFile::Open("/lustre19/expphy/volatile/halla/moller12gev/rahmans/root/radiationStudy_April14/beam_gemAnaV4.root","READ"); 
TFile *fin = TFile::Open("/lustre19/expphy/volatile/halla/moller12gev/rahmans/root/radiationStudy_April14/beam_det_6667.root","READ"); 
TCanvas* plot2d();
void plotgenAna()
{

gStyle->SetOptStat(0);
TCanvas *p1 = new TCanvas();
vector<string> hNms;
int cls[] = {2,6,1,4,7,3};
for(int ij=0;ij<nSpecies;ij++)
{
//hNms.push_back(Form("gemdet/gemdet_z0_%s_Dmg0",spH[ij].c_str()));
//hNms.push_back(Form("det6669/det6669_energyNEIL_%s",spH[ij].c_str()));
hNms.push_back(Form("det6667/det6667_z0_%s_Dmg2",spH[ij].c_str()));
//std::cout<<Form("det6669/det6669_z0_%s_Dmg0",spH[ij].c_str())<<std::endl;
}
for(int ij=0;ij<hNms.size();ij++)
{
TH1F* h1= (TH1F*)fin->Get(hNms[ij].c_str());
if(h1==nullptr)
{
std::cout<<"Can't find histogram"<<std::endl;
}
h1->SetLineWidth(2);
h1->SetLineColor(cls[ij]);
h1->SetMarkerColor(cls[ij]);
h1->Scale(scale);
gPad->SetTitle("");
if(ij==0){
h1->Draw("h1");
//h1->DrawCopy("h1");
h1->GetYaxis()->SetRangeUser(1.0e-12,1.0e-5);
//h1->GetYaxis()->SetRangeUser(1.0e-9,5.0e-3);
h1->SetTitle("");}
else
h1->Draw("same&&h1");
//h1->DrawCopy("same&&h1");
}

gPad->SetGridx(1);
gPad->SetGridy(1);
//gPad->SetLogx(1);
gPad->SetLogy(1);
//plot2d();
}

TCanvas *plot2d()
{
gStyle->SetOptStat(0);
TCanvas *p2 = new TCanvas();
//TH2F* h2= (TH2F*)fin->Get("det6667/det6667_z0x0_n_Dmg1");
TH2F* h2= (TH2F*)fin->Get("gemdet/gemdet_z0x0_n_Dmg0");
if(h2==nullptr)
{
std::cout<<"Can't find histogram"<<std::endl;
}
h2->Scale(scale);
gPad->SetTitle("");
h2->Draw("colz");
h2->GetYaxis()->SetRangeUser(-4000,4000);
h2->GetYaxis()->SetTitleOffset(0.8);
h2->GetZaxis()->SetRangeUser(1.e-9,1.e-3);
h2->SetTitle("");
gPad->SetLogz(1);
return p2;
}
