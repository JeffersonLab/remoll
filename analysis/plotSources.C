void writeHisto(TH1*, TFile*);

void plotSources(){
  string rRangeCut = "&& hit.r>0.6 && hit.r<1.2";//all acceptance
  //string rRangeCut = "&& hit.r>0.935 && hit.r<1.1";//ring5

  string foutNm = "o_plotSource.root";
  cout<<"Output goes into: "<<foutNm << endl;
  TFile *fout = new TFile(foutNm.c_str(),"RECREATE");
  TFile *finM = TFile::Open("../output/remollout_VacTst3reg_Moller_Al3mmBeamline_DSwin1p5mmR1020_winShield_b3050_ABC-VVA.root","READ");
  TFile *finE = TFile::Open("../output/remollout_VacTst3reg_epElastic_Al3mmBeamline_DSwin1p5mmR1020_winShield_b3050_ABC-VVA.root","READ");

  TTree *tM=(TTree*)finM->Get("T");
  TTree *tE=(TTree*)finE->Get("T");

  gStyle->SetOptStat("e");
  double zhigh=1e11;

  string tgtCuts = Form("rate*(hit.e>0.001 && hit.det==28 && hit.pid==11 %s && hit.vz<1)",rRangeCut.c_str());
  string NoNtgtCuts = Form("rate*(hit.e>0.001 && hit.det==28 && hit.pid==11 %s && hit.vz>1)",rRangeCut.c_str());
  string defaultCuts = Form("rate*(hit.e>0.001 && hit.det==28 && hit.pid==11 %s)",rRangeCut.c_str());

  TCanvas *c0=new TCanvas("c0","rate");
  TH1D *rateM = new TH1D("rateM","rate weighted dist",100,0.55,1.2);
  TH1D *rateE = new TH1D("rateE","rate weighted dist",100,0.55,1.2);
  tM->Project("rateM","hit.r",defaultCuts.c_str());
  tE->Project("rateE","hit.r",defaultCuts.c_str());
  rateE->SetLineColor(2);
  rateM->SetLineColor(1);
  rateE->SetLineColor(2);
  rateE->DrawCopy("h");
  rateM->DrawCopy("h&&same");
  writeHisto(rateE,fout);
  writeHisto(rateM,fout);
  gPad->SetLogy(1);

  TCanvas *c91=new TCanvas("c91","Full detector plane");
  c91->Divide(2);
  c91->cd(1);
  TH2D *rzSrcM=new TH2D("rzSrcM",";z[m];r[m]",200,-1,30,200,0,4);
  tM->Project("rzSrcM","sqrt(hit.vy*hit.vy + hit.vx*hit.vx):hit.vz",defaultCuts.c_str());
  rzSrcM->GetZaxis()->SetRangeUser(1,zhigh);
  rzSrcM->DrawCopy("colz");
  writeHisto(rzSrcM,fout);
  gPad->SetLogz(1);
  gPad->SetGridx(1);
  gPad->SetGridy(1);
  c91->cd(2);
  TH2D *rzSrcE=new TH2D("rzSrcE",";z[m];r[m]",200,-1,30,200,0,4);
  tE->Project("rzSrcE","sqrt(hit.vy*hit.vy + hit.vx*hit.vx):hit.vz",defaultCuts.c_str());
  rzSrcE->GetZaxis()->SetRangeUser(1,zhigh);
  rzSrcE->DrawCopy("colz");
  writeHisto(rzSrcE,fout);
  gPad->SetLogz(1);
  gPad->SetGridx(1);
  gPad->SetGridy(1);

  TCanvas *c5=new TCanvas("c5","hitE:hitR default cuts");
  c5->Divide(2);
  c5->cd(1);
  TH2D *erCorrM=new TH2D("erCorrM",";r[m];E[GeV]",200,0.5,1.3,200,0,12);
  tM->Project("erCorrM","hit.e:hit.r",defaultCuts.c_str());
  erCorrM->GetZaxis()->SetRangeUser(1,zhigh);
  erCorrM->DrawCopy("colz");
  writeHisto(erCorrM,fout);
  gPad->SetLogz(1);
  gPad->SetGridx(1);
  gPad->SetGridy(1);
  c5->cd(2);
  TH2D *erCorrE=new TH2D("erCorrE",";r[m];E[GeV]",200,0.5,1.3,200,0,12);
  tE->Project("erCorrE","hit.e:hit.r",defaultCuts.c_str());
  erCorrE->GetZaxis()->SetRangeUser(1,zhigh);
  erCorrE->DrawCopy("colz");
  writeHisto(erCorrE,fout);
  gPad->SetLogz(1);
  gPad->SetGridx(1);
  gPad->SetGridy(1);

  // TCanvas *c8=new TCanvas("c8","Z:X default cuts");
  // c8->Divide(2);
  // c8->cd(1);
  // TH2D *xzSrcM=new TH2D("xzSrcM",";z[m];x[m]",200,-1,30,200,-3,3);
  // tM->Project("xzSrcM","hit.vx:hit.vz",defaultCuts.c_str());
  // xzSrcM->GetZaxis()->SetRangeUser(1,zhigh);
  // xzSrcM->DrawCopy("colz");
  // writeHisto(xzSrcM,fout);
  // gPad->SetLogz(1);
  // gPad->SetGridx(1);
  // gPad->SetGridy(1);
  // c8->cd(2);
  // TH2D *xzSrcE=new TH2D("xzSrcE",";z[m];x[m]",200,-1,30,200,-3,3);
  // tE->Project("xzSrcE","hit.vx:hit.vz",defaultCuts.c_str());
  // xzSrcE->GetZaxis()->SetRangeUser(1,zhigh);
  // xzSrcE->DrawCopy("colz");
  // writeHisto(xzSrcE,fout);
  // gPad->SetLogz(1);
  // gPad->SetGridx(1);
  // gPad->SetGridy(1);

  // TCanvas *c1=new TCanvas("c1","X:Y default cuts");
  // c1->Divide(2);
  // c1->cd(1);
  // TH2D *xyPosM=new TH2D("xyPosM",";x;y",200,-1.2,1.2,200,-1.2,1.2);
  // tM->Project("xyPosM","hit.y:hit.x",defaultCuts.c_str());
  // xyPosM->GetZaxis()->SetRangeUser(1,zhigh);
  // xyPosM->DrawCopy("colz");
  // writeHisto(xyPosM,fout);
  // gPad->SetLogz(1);
  // gPad->SetGridx(1);
  // gPad->SetGridy(1);
  // c1->cd(2);
  // TH2D *xyPosE=new TH2D("xyPosE",";x;y",200,-1.2,1.2,200,-1.2,1.2);
  // tE->Project("xyPosE","hit.y:hit.x",defaultCuts.c_str());
  // xyPosE->GetZaxis()->SetRangeUser(1,zhigh);
  // xyPosE->DrawCopy("colz");
  // writeHisto(xyPosE,fout);
  // gPad->SetLogz(1);
  // gPad->SetGridx(1);
  // gPad->SetGridy(1);

  // TCanvas *c2=new TCanvas("c2","Px:Py default cuts");
  // c2->Divide(2);
  // c2->cd(1);
  // TH2D *xyMomM=new TH2D("xyMomM",";P_x;P_y",200,-0.5,0.5,200,-0.5,0.5);
  // tM->Project("xyMomM","hit.py:hit.px",defaultCuts.c_str());
  // xyMomM->GetZaxis()->SetRangeUser(1,zhigh);
  // xyMomM->DrawCopy("colz");
  // writeHisto(xyMomM,fout);
  // gPad->SetLogz(1);
  // gPad->SetGridx(1);
  // gPad->SetGridy(1);
  // c2->cd(2);
  // TH2D *xyMomE=new TH2D("xyMomE",";P_x;P_y",200,-0.5,0.5,200,-0.5,0.5);
  // tE->Project("xyMomE","hit.py:hit.px",defaultCuts.c_str());
  // xyMomE->GetZaxis()->SetRangeUser(1,zhigh);
  // xyMomE->DrawCopy("colz");
  // writeHisto(xyMomE,fout);
  // gPad->SetLogz(1);
  // gPad->SetGridx(1);
  // gPad->SetGridy(1);

  TCanvas *c51=new TCanvas("c51","E:r tgt");
  c51->Divide(2);
  c51->cd(1);
  TH2D *erCorrTgtM=new TH2D("erCorrTgtM",";r[m];E[GeV]",200,0.5,1.3,200,0,12);
  tM->Project("erCorrTgtM","hit.e:hit.r",tgtCuts.c_str());
  erCorrTgtM->GetZaxis()->SetRangeUser(1,zhigh);
  erCorrTgtM->DrawCopy("colz");
  writeHisto(erCorrTgtM,fout);
  gPad->SetLogz(1);
  gPad->SetGridx(1);
  gPad->SetGridy(1);
  c51->cd(2);
  TH2D *erCorrTgtE=new TH2D("erCorrTgtE",";r[m];E[GeV]",200,0.5,1.3,200,0,12);
  tE->Project("erCorrTgtE","hit.e:hit.r",tgtCuts.c_str());
  erCorrTgtE->GetZaxis()->SetRangeUser(1,zhigh);
  erCorrTgtE->DrawCopy("colz");
  writeHisto(erCorrTgtE,fout);
  gPad->SetLogz(1);
  gPad->SetGridx(1);
  gPad->SetGridy(1);

  TCanvas *c52=new TCanvas("c52","E:r nonTgt");
  c52->Divide(2);
  c52->cd(1);
  TH2D *erCorrNoNtgtM=new TH2D("erCorrNoNtgtM",";r[m];E[GeV]",200,0.5,1.3,200,0,12);
  tM->Project("erCorrNoNtgtM","hit.e:hit.r",NoNtgtCuts.c_str());
  erCorrNoNtgtM->GetZaxis()->SetRangeUser(1,zhigh);
  erCorrNoNtgtM->DrawCopy("colz");
  writeHisto(erCorrNoNtgtM,fout);
  gPad->SetLogz(1);
  gPad->SetGridx(1);
  gPad->SetGridy(1);
  c52->cd(2);
  TH2D *erCorrNoNtgtE=new TH2D("erCorrNoNtgtE",";r[m];E[GeV]",200,0.5,1.3,200,0,12);
  tE->Project("erCorrNoNtgtE","hit.e:hit.r",NoNtgtCuts.c_str());
  erCorrNoNtgtE->GetZaxis()->SetRangeUser(1,zhigh);
  erCorrNoNtgtE->DrawCopy("colz");
  writeHisto(erCorrNoNtgtE,fout);
  gPad->SetLogz(1);
  gPad->SetGridx(1);
  gPad->SetGridy(1);

  TCanvas *c53=new TCanvas("c53","E tgt/noTgt");
  TH1D *eMtgt=new TH1D("eMtgt",";E[GeV]",200,0,1);
  tM->Project("eMtgt","hit.e",tgtCuts.c_str());
  TH1D *eMntgt=new TH1D("eMntgt",";E[GeV]",200,0,1);
  tM->Project("eMntgt","hit.e",NoNtgtCuts.c_str());
  TH1D *eEtgt=new TH1D("eEtgt",";E[GeV]",200,0,1);
  tE->Project("eEtgt","hit.e",tgtCuts.c_str());
  TH1D *eEntgt=new TH1D("eEntgt",";E[GeV]",200,0,1);
  tE->Project("eEntgt","hit.e",NoNtgtCuts.c_str());
  eMtgt->SetLineColor(1);
  eEtgt->SetLineColor(2);
  eMntgt->SetLineColor(1);
  eEntgt->SetLineColor(2);
  eMntgt->SetLineStyle(1);
  eEntgt->SetLineStyle(2);
  eEtgt->DrawCopy("h");
  eMtgt->DrawCopy("h&&same");
  eEntgt->DrawCopy("h&&same");
  eMntgt->DrawCopy("h&&same");
  gPad->SetLogy(1);

  TCanvas *c3=new TCanvas("c3","momentum default cuts");
  TH1D *pM=new TH1D("pM",";momentum[GeV]",100,0,12);
  tM->Project("pM","hit.p",defaultCuts.c_str());
  TH1D *pE=new TH1D("pE",";momentum[GeV]",100,0,12);
  tE->Project("pE","hit.p",defaultCuts.c_str());
  pM->SetLineColor(1);
  pE->SetLineColor(2);
  pE->DrawCopy("h");
  pM->DrawCopy("h&&same");
  gPad->SetLogy(1);
  writeHisto(pE,fout);
  writeHisto(pM,fout);

  TCanvas *c4=new TCanvas("c4","theta default cuts");
  TH1D *thM=new TH1D("thM",";theta [deg]",180,0,90);
  tM->Project("thM","acos(hit.pz/hit.p)*180/3.1415",defaultCuts.c_str());
  TH1D *thE=new TH1D("thE",";theta [deg]",180,0,90);
  tE->Project("thE","acos(hit.pz/hit.p)*180/3.1415",defaultCuts.c_str());
  thM->SetLineColor(1);
  thE->SetLineColor(2);
  thE->DrawCopy("h");
  thM->DrawCopy("h&&same");
  gPad->SetLogy(1);
  writeHisto(thE,fout);
  writeHisto(thM,fout);

  TCanvas *c41=new TCanvas("c41","theta tgt");
  TH1D *thTgTM=new TH1D("thTgTM",";theta [deg]",180,0,90);
  tM->Project("thTgTM","acos(hit.pz/hit.p)*180/3.1415",tgtCuts.c_str());
  TH1D *thTgTE=new TH1D("thTgTE",";theta [deg]",180,0,90);
  tE->Project("thTgTE","acos(hit.pz/hit.p)*180/3.1415",tgtCuts.c_str());
  thTgTM->SetLineColor(1);
  thTgTE->SetLineColor(2);
  thTgTE->DrawCopy("h");
  thTgTM->DrawCopy("h&&same");
  gPad->SetLogy(1);
  cout<<"Percent of contributions with tgt source(Mac): "<<thTgTM->Integral()/thM->Integral()<<endl;
  cout<<"Percent of contributions with tgt source(Eir): "<<thTgTE->Integral()/thE->Integral()<<endl;
  writeHisto(thTgTE,fout);
  writeHisto(thTgTM,fout);

  TCanvas *c42=new TCanvas("c42","theta nonTgt");
  TH1D *thNoNtgtM=new TH1D("thNoNtgtM",";theta [deg]",180,0,90);
  tM->Project("thNoNtgtM","acos(hit.pz/hit.p)*180/3.1415",NoNtgtCuts.c_str());
  TH1D *thNoNtgtE=new TH1D("thNoNtgtE",";theta [deg]",180,0,90);
  tE->Project("thNoNtgtE","acos(hit.pz/hit.p)*180/3.1415",NoNtgtCuts.c_str());
  thNoNtgtM->SetLineColor(1);
  thNoNtgtE->SetLineColor(2);
  thNoNtgtE->DrawCopy("h");
  thNoNtgtM->DrawCopy("h&&same");
  gPad->SetLogy(1);
  cout<<"Percent of contributions with non tgt source(Mac): "<<thNoNtgtM->Integral()/thM->Integral()
      <<"\tTotal rate compared to vacuum\t"<<thM->Integral()/thM->Integral()<<endl;
  cout<<"Percent of contributions with non tgt source(Eir): "<<thNoNtgtE->Integral()/thE->Integral()
      <<"\tTotal rate compared to vacuum\t"<<thE->Integral()/thM->Integral()<<endl;
  writeHisto(thNoNtgtE,fout);
  writeHisto(thNoNtgtM,fout);

  TCanvas *c7=new TCanvas("c7","xyz");
  c7->Divide(2);
  c7->cd(1);
  tM->Draw("hit.vy:hit.vx:hit.vz","rate*(hit.det==28 && hit.pid==11 && hit.r>0.6 && hit.r<1.2)");
  c7->cd(2);
  tE->Draw("hit.vy:hit.vx:hit.vz","rate*(hit.det==28 && hit.pid==11 && hit.r>0.6 && hit.r<1.2 && hit.vz>1)");

  TCanvas *c9=new TCanvas("c9","x0y0 default cuts");
  c9->Divide(2);
  c9->cd(1);
  TH2D *xySrcM=new TH2D("xySrcM",";x[m];y[m]",200,-3,3,200,-3,3);
  tM->Project("xySrcM","hit.vy:hit.vx",defaultCuts.c_str());
  xySrcM->GetZaxis()->SetRangeUser(1,zhigh);
  xySrcM->DrawCopy("colz");
  writeHisto(xySrcM,fout);
  gPad->SetLogz(1);
  gPad->SetGridx(1);
  gPad->SetGridy(1);
  c9->cd(2);
  TH2D *xySrcE=new TH2D("xySrcE",";x[m];y[m]",200,-3,3,200,-3,3);
  tE->Project("xySrcE","hit.vy:hit.vx",defaultCuts.c_str());
  xySrcE->GetZaxis()->SetRangeUser(1,zhigh);
  xySrcE->DrawCopy("colz");
  writeHisto(xySrcE,fout);
  gPad->SetLogz(1);
  gPad->SetGridx(1);
  gPad->SetGridy(1);

  TCanvas *c92=new TCanvas("c92","r0 default cuts");
  TH1D *rSrcM=new TH1D("rSrcM",";source r [m]",100,-1,4);
  TH1D *rSrcE=new TH1D("rSrcE",";source r [m]",100,-1,4);
  tM->Project("rSrcM","sqrt(hit.vy*hit.vy + hit.vx*hit.vx)",defaultCuts.c_str());
  tE->Project("rSrcE","sqrt(hit.vy*hit.vy + hit.vx*hit.vx)",defaultCuts.c_str());
  rSrcM->SetLineColor(1);
  rSrcE->SetLineColor(2);
  rSrcM->DrawCopy("h");
  rSrcE->DrawCopy("h&&same");
  writeHisto(rSrcE,fout);
  writeHisto(rSrcM,fout);

  // TCanvas *c10=new TCanvas("c10","pT default cuts");
  // TH1D *pTpM=new TH1D("pTpM",";pT [%]",100,0,1);
  // tM->Project("pTpM","sqrt(hit.px*hit.px + hit.py*hit.py)",defaultCuts.c_str());
  // TH1D *pTpE=new TH1D("pTpE",";pT [%]",100,0,1);
  // tE->Project("pTpE","sqrt(hit.px*hit.px + hit.py*hit.py)",defaultCuts.c_str());
  // pTpM->SetLineColor(1);
  // pTpE->SetLineColor(2);
  // pTpM->DrawCopy("h");
  // pTpE->DrawCopy("h&&same");
  // gPad->SetLogy(1);
  // writeHisto(pTpE,fout);
  // writeHisto(pTpM,fout);

  TCanvas *c11=new TCanvas("c11","pT default cuts");
  TH1D *pTM=new TH1D("pTM",";pT [GeV]",100,0,12);
  tM->Project("pTM","hit.p*sqrt(hit.px*hit.px + hit.py*hit.py)",defaultCuts.c_str());
  TH1D *pTE=new TH1D("pTE",";pT [GeV]",100,0,12);
  tE->Project("pTE","hit.p*sqrt(hit.px*hit.px + hit.py*hit.py)",defaultCuts.c_str());
  pTM->SetLineColor(1);
  pTE->SetLineColor(2);
  pTM->DrawCopy("h");
  pTE->DrawCopy("h&&same");
  gPad->SetLogy(1);
  writeHisto(pTE,fout);
  writeHisto(pTM,fout);

  finM->Close();
  finE->Close();
  fout->Close();
}

void writeHisto(TH1* h, TFile *f){
  f->cd();
  h->Write();
}
