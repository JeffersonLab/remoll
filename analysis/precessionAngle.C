void precessionAngle(){
  gSystem->Load("../build/libremoll.so");
  TFile *fin=TFile::Open("../remollout_SpinPrecTest.root","READ");
  TTree *t=(TTree*)fin->Get("T");

  string ang="180/3.14159 * acos(hit.pz/sqrt(hit.px*hit.px + hit.py*hit.py + hit.pz*hit.pz ))";
  TCanvas *c1=new TCanvas("c1","c1");
  c1->Divide(2);
  c1->cd(1);
  t->Draw("atan2(hit.r,9.875)","hit.det==201 && hit.trid==1");
  c1->cd(2);
  t->Draw(ang.c_str(),"hit.det==201 && hit.trid==1");

  TCanvas *c2=new TCanvas("c2","c2");
  t->Draw(ang.c_str(),Form("hit.det==28 && hit.trid==1 && %s<10",ang.c_str()));

  TCanvas *c7=new TCanvas("c7","c7");
  t->Draw(Form("hit.r:%s",ang.c_str()),
  	  Form("hit.det==28 && hit.trid==1 && %s<10",ang.c_str()),
  	  "colz");

  TCanvas *c3=new TCanvas("c3","c3");
  c3->Divide(3);
  c3->cd(1);
  t->Draw("hit.sx","hit.det==201 && hit.trid==1");
  c3->cd(2);
  t->Draw("hit.sy","hit.det==201 && hit.trid==1");
  c3->cd(3);
  t->Draw("hit.sz","hit.det==201 && hit.trid==1");

  TCanvas *c4=new TCanvas("c4","c4");
  c4->Divide(3);
  c4->cd(1);
  t->Draw("hit.sx","hit.det==28 && hit.trid==1");
  c4->cd(2);
  t->Draw("hit.sy","hit.det==28 && hit.trid==1");
  c4->cd(3);
  t->Draw("hit.sz","hit.det==28 && hit.trid==1");

  ang = "180/3.14159 * acos(hit.sz/sqrt(hit.sx*hit.sx + hit.sy*hit.sy + hit.sz*hit.sz ))";
  TCanvas *c5=new TCanvas("c5","c5");
  c5->Divide(2);
  c5->cd(1);
  t->Draw(ang.c_str(),"hit.det==201 && hit.trid==1");
  c5->cd(2);
  t->Draw(ang.c_str(),"hit.det==28 && hit.trid==1");

  TCanvas *c6=new TCanvas("c6","c6");
  t->Draw(Form("hit.r:%s",ang.c_str()),"hit.det==28 && hit.trid==1","colz");

  TH2F *h1=new TH2F("h1","phi-spin angle correlations;phi[deg];spin angle [deg]",100,-190,190,100,0,60);
  TCanvas *c9=new TCanvas("c9","c9");
  t->Draw(Form("%s:hit.ph*180/3.14159>>h1",ang.c_str()),"hit.det==28 && hit.trid==1","colz");
  h1->DrawCopy("colz");

  TCanvas *c10=new TCanvas("c10","c10");
  t->Draw("sqrt(1-hit.sz*hit.sz)","hit.det==28 && hit.trid==1");

  TCanvas *c11=new TCanvas("c11","c11");
  t->Draw("sqrt(1-hit.sz*hit.sz):hit.ph*180/3.14159","hit.det==28 && hit.trid==1","colz");

  TCanvas *c12=new TCanvas("c12","c12");
  t->Draw("atan2(hit.sx,hit.sy)","hit.det==28 && hit.trid==1");

  TCanvas *c13=new TCanvas("c13","c13");
  t->Draw("atan2(hit.sx,hit.sy):hit.ph*180/3.14159","hit.det==28 && hit.trid==1","colz");

  fin->Close();
}
