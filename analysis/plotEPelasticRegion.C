void plotEPelasticRegion(){
  TFile *_file0 = TFile::Open("../output/remollout_VacTst_epElastic_kryp_beamline_Air.root");
  TTree *T=(TTree*)_file0->Get("T");
  gStyle->SetOptStat(0);

  string detRegion = "hit.r>0.935 && hit.r<1.1";//r5
  //string detRegion = "&& hit.r>0.6 && hit.r<1.2";//all
  string defaultCuts = "hit.pid==11 && hit.det==28";

  TCanvas *c1=new TCanvas();
  T->Draw("sqrt(hit.vx*hit.vx+hit.vy*hit.vy):hit.vz>>h0(200,-1,30,200,0,4)",Form("rate*( %s && %s)",defaultCuts.c_str(),detRegion.c_str()),"colz");
  gPad->SetLogz(1);

  TCanvas *c2=new TCanvas();
  //T->Draw("sqrt(hit.vx*hit.vx+hit.vy*hit.vy)/hit.vz>>h1(500,0,0.5)","(hit.pid==11 && hit.det==28 && hit.r>0.6 && hit.r<1.2 && hit.vz<26)","h");
  T->Draw("sqrt(hit.vx*hit.vx+hit.vy*hit.vy)/hit.vz>>h1(500,0,0.1)",Form("(%s && %s && hit.vz<26 && hit.vz>1)",defaultCuts.c_str(),detRegion.c_str()),"h");

  TCanvas *c3=new TCanvas();
  T->Draw("sqrt(hit.vx*hit.vx+hit.vy*hit.vy):hit.vz>>h2(200,-1,30,200,0,4)",Form("(%s && %s && hit.vz<26)",defaultCuts.c_str(),detRegion.c_str()),"colz");
  gPad->SetLogz(1);

  TCanvas *c4=new TCanvas();
  T->Draw("sqrt(hit.vx*hit.vx+hit.vy*hit.vy):hit.vz>>h21(200,-1,30,200,0,4)",Form("(%s && %s && hit.vz<26 && sqrt(hit.vx*hit.vx+hit.vy*hit.vy)/hit.vz<0.03 && sqrt(hit.vx*hit.vx+hit.vy*hit.vy)/hit.vz>0.018 && hit.vz>1)",defaultCuts.c_str(),detRegion.c_str()),"colz");
  gPad->SetLogz(1);

  TCanvas *c5=new TCanvas();
  T->Draw("sqrt(hit.vx*hit.vx+hit.vy*hit.vy):hit.vz>>h3(200,-1,30,200,0,4)",Form("(%s && %s && hit.vz<26 && sqrt(hit.vx*hit.vx+hit.vy*hit.vy)/hit.vz>0.045 && hit.vz>1)",defaultCuts.c_str(),detRegion.c_str()),"colz");

  TCanvas *c6=new TCanvas();
  T->Draw("sqrt(hit.vx*hit.vx+hit.vy*hit.vy)/hit.vz>>h4(500,0,0.1)",Form("rate*(%s && %s && hit.vz<26 && hit.vz>1)",defaultCuts.c_str(),detRegion.c_str()),"h");

  TCanvas *c7=new TCanvas();
  T->Draw("sqrt(hit.vx*hit.vx+hit.vy*hit.vy)/hit.vz:ev.Q2",Form("rate*(%s && %s && hit.vz<26 && sqrt(hit.vx*hit.vx+hit.vy*hit.vy)/hit.vz<0.1 && hit.vz>1)",defaultCuts.c_str(),detRegion.c_str()),"colz");
  gPad->SetLogz(1);

  TCanvas *c8=new TCanvas();
  T->Draw("sqrt(hit.vx*hit.vx+hit.vy*hit.vy)/hit.vz:rate",Form("rate*(%s && %s && hit.vz<26 && sqrt(hit.vx*hit.vx+hit.vy*hit.vy)/hit.vz<0.1 && hit.vz>1)",defaultCuts.c_str(),detRegion.c_str()),"colz");
  gPad->SetLogz(1);

  TCanvas *c9=new TCanvas();
  T->Draw("sqrt(hit.vx*hit.vx+hit.vy*hit.vy)/hit.vz:hit.e",Form("rate*(%s && %s && hit.vz<26 && sqrt(hit.vx*hit.vx+hit.vy*hit.vy)/hit.vz<0.1 && hit.vz>1 && hit.e<1)",defaultCuts.c_str(),detRegion.c_str()),"colz");
  gPad->SetLogz(1);

  TCanvas *c10=new TCanvas();
  T->Draw("sqrt(hit.vx*hit.vx+hit.vy*hit.vy)/hit.vz:hit.n",Form("rate*(%s && %s && hit.vz<26 && sqrt(hit.vx*hit.vx+hit.vy*hit.vy)/hit.vz<0.1 && hit.vz>1 && hit.e<1)",defaultCuts.c_str(),detRegion.c_str()),"colz");

  TCanvas *c11=new TCanvas();
  T->Draw("sqrt(hit.vx*hit.vx+hit.vy*hit.vy)/hit.vz:ev.th[0]",Form("rate*(%s && %s && hit.vz<26 && sqrt(hit.vx*hit.vx+hit.vy*hit.vy)/hit.vz<0.1 && hit.vz>1)",defaultCuts.c_str(),detRegion.c_str()),"colz");

  TCanvas *c12=new TCanvas();
  T->Draw("ev.tpx:ev.px",Form("rate*(%s && %s && hit.vz<26 && sqrt(hit.vx*hit.vx+hit.vy*hit.vy)/hit.vz<0.1 && hit.vz>1)",defaultCuts.c_str(),detRegion.c_str()),"colz");

}
