#include "histogramUtilities.h"

void printLogX(string fnm="../beamGeoV2_radAnaV4_correctMD.root"){
  TFile *fin=TFile::Open(fnm.c_str(),"READ");
  TH1F *h=(TH1F*)fin->Get("det28/d28_energy_R7_ep");
  TH1F *hN=(TH1F*)fin->Get("det28/d28_energyNEIL_R7_ep");
  
  auto *c1=new TCanvas();
  c1->Divide(2);
  c1->cd(1);
  h->DrawCopy();
  gPad->SetLogx(1);
  gPad->SetLogy(1);
  gPad->SetGridx(1);
  gPad->SetGridy(1);

  string rangeNm[13]={"KE<1e-8","1e-8<KE<1e-7","1e-7<KE<1e-6","1e-6<KE<1e-5","1e-5<KE<1e-4","1e-4<KE<1e-3","1e-3<KE<1e-2","1e-2<KE<1e-1","1e-1<KE<1e-0","1e+0<KE<1e+1","1e+1<KE<1e+2","1e+2<KE<1e+3","1e+3<KE<1e+4"};
  int nBins = h->GetXaxis()->GetNbins();
  double sum(0),sN(0);
  for(int i=0;i<=nBins;i++){
    sum+= h->GetBinContent(i);
    sN+= hN->GetBinContent(i);
    if(i%10==1){
      cout<<rangeNm[int(i/10)]<<"\t"<<sum<<"\t"<<sN<<endl;
      //cout<<rangeNm[int(i/10)]<<"\t"<<h->GetBinCenter(i-1)<<"\t"<<sum<<endl;
      sum=0;
      sN=0;
    }
  }

  c1->cd(2);
  TH1F *h2=(TH1F*)dNdXscaleLogX(h);
  h2->DrawCopy();
  gPad->SetLogx(1);
  gPad->SetLogy(1);
  gPad->SetGridx(1);
  gPad->SetGridy(1);

  fin->Close();
}
