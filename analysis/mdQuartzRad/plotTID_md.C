void plotTID_md(){
  // auto fee = TFile::Open("../o_tid_ee_pzG0_MD_quartz_tidAnaMDV1.root","READ");
  // auto fep = TFile::Open("../o_tid_epE_pzG0_MD_quartz_tidAnaMDV1.root","READ");
  auto fee = TFile::Open("../o_tid_ee_pzL0_MD_quartz_tidAnaMDV1.root","READ");
  auto fep = TFile::Open("../o_tid_epE_pzL0_MD_quartz_tidAnaMDV1.root","READ");

  int zoom(0);
  gStyle->SetOptStat(0);

  auto c1=new TCanvas("c1","rates",1800,1500);
  auto he=(TH1D*)fee->Get("hXYrate");
  auto hp=(TH1D*)fep->Get("hXYrate");
  c1->Divide(2,2);
  c1->cd(1);
  if(zoom){
    he->GetXaxis()->SetRangeUser(-1200,-600);
    he->GetYaxis()->SetRangeUser(-500,500);
  }
  he->DrawCopy("colz");
  c1->cd(2);
  if(zoom){
    hp->GetXaxis()->SetRangeUser(-1200,-600);
    hp->GetYaxis()->SetRangeUser(-500,500);
  }
  hp->DrawCopy("colz");
  c1->cd(3);
  auto hs=(TH1D*)he->Clone("hsum");
  hs->Add(hp);
  if(zoom){
    hs->GetXaxis()->SetRangeUser(-1200,-600);
    hs->GetYaxis()->SetRangeUser(-500,500);
  }
  hs->DrawCopy("colz");

  auto c2=new TCanvas("c2","radDmgElec",1800,1500);
  he=(TH1D*)fee->Get("rXYrate");
  hp=(TH1D*)fep->Get("rXYrate");
  c2->Divide(2,2);
  c2->cd(1);
  if(zoom){
    he->GetXaxis()->SetRangeUser(-1200,-600);
    he->GetYaxis()->SetRangeUser(-500,500);
  }
  he->DrawCopy("colz");
  c2->cd(2);
  if(zoom){
    hp->GetXaxis()->SetRangeUser(-1200,-600);
    hp->GetYaxis()->SetRangeUser(-500,500);
  }
  hp->DrawCopy("colz");
  c2->cd(3);
  delete hs;
  hs=(TH1D*)he->Clone("hsum");
  hs->Add(hp);
  if(zoom){
    hs->GetXaxis()->SetRangeUser(-1200,-600);
    hs->GetYaxis()->SetRangeUser(-500,500);
  }
  hs->DrawCopy("colz");


  auto c3=new TCanvas("c3","radDmgTot",1800,1500);
  he=(TH1D*)fee->Get("rXYtot");
  hp=(TH1D*)fep->Get("rXYtot");
  c3->Divide(2,2);
  c3->cd(1);
  if(zoom){
    he->GetXaxis()->SetRangeUser(-1200,-600);
    he->GetYaxis()->SetRangeUser(-500,500);
  }
  he->DrawCopy("colz");
  c3->cd(2);
  if(zoom){
    hp->GetXaxis()->SetRangeUser(-1200,-600);
    hp->GetYaxis()->SetRangeUser(-500,500);
  }
  hp->DrawCopy("colz");
  c3->cd(3);
  delete hs;
  hs=(TH1D*)he->Clone("hsum");
  hs->Add(hp);
  if(zoom){
    hs->GetYaxis()->SetRangeUser(-1200,-600);
    hs->GetYaxis()->SetRangeUser(-500,500);
  }
  hs->DrawCopy("colz");
  
}
