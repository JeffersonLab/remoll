TH2D *plotOne(string fnm, int detID);

void plotTID_dsTorus(){
  gStyle->SetOptStat(0);

  auto can = new TCanvas("cs","sums",2300,1000);
  can->Divide(3,2);
  for(int i=1;i<6;i++){
    TH2D *hG = plotOne(Form("../o_tid_conf14_%d_pzG0_tidAnaV0.root",5700+i),5700+i);
    TH2D *hL = plotOne(Form("../o_tid_conf14_%d_pzL0_tidAnaV0.root",5700+i),6700+i);
    hG->Add(hL);
    can->cd(i+1);
    hG->GetXaxis()->SetRangeUser(100,600);
    hG->GetYaxis()->SetRangeUser(-400,400);
    hG->DrawCopy("colz");
    gPad->SetLogz(1);
    gPad->SetGridx(1);
    gPad->SetGridy(1);
  }
}

TH2D *plotOne(string fnm, int detID){
  auto c1=new TCanvas(Form("c1_%d",detID),Form("rad%d",detID),
		      2300,1000);
  c1->Divide(3,2);

  TFile *fin=TFile::Open(fnm.c_str(),"READ");
  TH2D *h[5];
  const string spTit[5]={"o","ep","em","g","n"};
  for(int i=0;i<5;i++){
    h[i]=(TH2D*)fin->Get(Form("hXY_dt2_wg2_%s",spTit[i].c_str()));
    c1->cd(i+1);
    h[i]->GetXaxis()->SetRangeUser(-1500,1500);
    h[i]->GetYaxis()->SetRangeUser(-1500,1500);
    h[i]->DrawCopy("colz");
    gPad->SetLogz(1);
    gPad->SetGridx(1);
    gPad->SetGridy(1);
  }

  c1->cd(6);
  auto hs=(TH2D*)h[0]->Clone("hs");
  for(int i=1;i<5;i++){
    //cout<<i<<" "<<h[i]->GetXaxis()->GetNbins()<<endl;
    hs->Add(h[i]);
  }
  hs->GetXaxis()->SetRangeUser(-1500,1500);
  hs->GetYaxis()->SetRangeUser(-1500,1500);
  hs->SetTitle(Form("det %d all particles [rad]",detID));
  hs->DrawCopy("colz");
  gPad->SetLogz(1);
  gPad->SetGridx(1);
  gPad->SetGridy(1);
  
  hs->SetDirectory(0);
  fin->Close();
  return hs;
}
