string rSelect="&& hit.r>0.935 && hit.r<1.1";//r5
// string rSelect="&& hit.r>0.6 && hit.r<1.2";//all det

void doOne(TTree*, TCanvas*,double);

void plotRate(){
  const int nFl=12;
  string fnms[nFl]={
    "../output/remollout_VacTst_Moller_Vac.root",
    // "../output/remollout_VacTst_Moller_kryp_beamline_Can.root",
    // "../output/remollout_VacTst_Moller_kryp_beamline_Air.root",
    // "../output/remollout_VacTst_Moller_kryp_beamline_He.root",
    // "../output/remollout_VacTst_Moller_Can.root",
    // "../output/remollout_VacTst_Moller_Air.root",
    // "../output/remollout_VacTst_epInelastic_Vac.root",
    // "../output/remollout_VacTst_epInelastic_kryp_beamline_Can.root",
    // "../output/remollout_VacTst_epInelastic_kryp_beamline_Air.root",
    "../output/remollout_VacTst_epElastic_Vac.root",
    // "../output/remollout_VacTst_epElastic_kryp_beamline_Can.root",
    // "../output/remollout_VacTst_epElastic_kryp_beamline_Air.root",
    // "../output/remollout_VacTst_epElastic_kryp_beamline_He.root",
  //   "../remollout_VacTst_Moller_window_kryp_beamline_Air.root",
  //   "../remollout_VacTst_Moller_window_kryp_beamline_Can.root",
  //   "../remollout_VacTst_Moller_window_kryp_beamline_HeAir.root",
  //   "../remollout_VacTst_Moller_window_kryp_beamline_HeCan.root",
  //   "../remollout_VacTst_Moller_window_kryp_beamline_CanHeAll.root",
  //   "../remollout_VacTst_epElastic_window_kryp_beamline_Air.root",
  //   "../remollout_VacTst_epElastic_window_kryp_beamline_Can.root",
  //   "../remollout_VacTst_epElastic_window_kryp_beamline_HeAir.root",
  //   "../remollout_VacTst_epElastic_window_kryp_beamline_HeCan.root",
  //   "../remollout_VacTst_epElastic_window_kryp_beamline_CanHeAll.root"
    // "../remollout_VacTst3reg_epElastic_krypBeamline_ABC-VVA.root",
    // "../remollout_VacTst3reg_epElastic_krypBeamline_DSwin_ABC-VVA.root",
    // "../remollout_VacTst3reg_epElastic_krypBeamline_DSwin_b3075_ABC-VVA.root",
    // "../remollout_VacTst3reg_epElastic_krypBeamline_DSwin_b3050_ABC-VVA.root",
    // "../remollout_VacTst3reg_epElastic_krypBeamline_DSwin1p5mm_b3050_ABC-VVA.root",
    // "../remollout_VacTst3reg_epElastic_krypBeamline_USwin_DSwin_ABC-HHA.root",
    // "../remollout_VacTst3reg_epElastic_krypBeamline_USwin_DSwin_ABC-AHA.root",
    //"../remollout_VacTst3reg_epElastic_krypBeamline_USwin_movedDSwin_ABC-HAA.root",
    "../remollout_VacTst3reg_epElastic_krypBeamline_DSwin1p5mm_b3050_ABC-VVA.root",
    "../remollout_VacTst3reg_epElastic_krypBeamline_DSwin1p0mm_b3050_ABC-VVA.root",
    "../remollout_VacTst3reg_epElastic_krypBeamline_DSwin0p5mm_b3050_ABC-VVA.root",
    "../remollout_VacTst3reg_epElastic_krypBeamline_DSwin1p5mm_b3075_ABC-VVA.root",
    "../remollout_VacTst3reg_epElastic_krypBeamline_DSwin1p5mm_b3100_ABC-VVA.root",
    // "../remollout_VacTst3reg_Moller_krypBeamline_ABC-VVA.root",
    // "../remollout_VacTst3reg_Moller_krypBeamline_DSwin_ABC-VVA.root",
    // "../remollout_VacTst3reg_Moller_krypBeamline_DSwin_b3075_ABC-VVA.root",
    // "../remollout_VacTst3reg_Moller_krypBeamline_DSwin_b3050_ABC-VVA.root",
    // "../remollout_VacTst3reg_Moller_krypBeamline_DSwin1p5mm_b3050_ABC-VVA.root",
    // "../remollout_VacTst3reg_Moller_krypBeamline_USwin_DSwin_ABC-HHA.root",
    // "../remollout_VacTst3reg_Moller_krypBeamline_USwin_DSwin_ABC-AHA.root",
    // "../remollout_VacTst3reg_Moller_krypBeamline_USwin_movedDSwin_ABC-HAA.root",
    "../remollout_VacTst3reg_Moller_krypBeamline_DSwin1p5mm_b3050_ABC-VVA.root",
    "../remollout_VacTst3reg_Moller_krypBeamline_DSwin1p0mm_b3050_ABC-VVA.root",
    "../remollout_VacTst3reg_Moller_krypBeamline_DSwin0p5mm_b3050_ABC-VVA.root",
    "../remollout_VacTst3reg_Moller_krypBeamline_DSwin1p5mm_b3075_ABC-VVA.root",
    "../remollout_VacTst3reg_Moller_krypBeamline_DSwin1p5mm_b3100_ABC-VVA.root"
  };

  double highLimit[nFl]={4e10,4e10,4e10,2e9,2e9,2e9,1e9,1e9,5e9};
  gStyle->SetOptStat("MRoui");

  TCanvas *c1[nFl];
  TFile *fin;
  TTree *t;
  //for(int i=0;i<1;i++){
  for(int i=0;i<nFl;i++){
    c1[i]=new TCanvas(Form("c%d",i),fnms[i].c_str());
    fin = TFile::Open(fnms[i].c_str(),"READ");
    t = (TTree *)fin->Get("T");
    cout<<fnms[i];
    doOne(t,c1[i],highLimit[i]);
    fin->Close();
  }
}

void doOne(TTree *t, TCanvas *can,double highLimit){
  TH1D *h1=new TH1D("h1","hits ;rate[Hz]",200,0,highLimit);
  TH1D *h2=new TH1D("h2","rate weighted ;rate[Hz]",200,0,highLimit);
  int b1 = h1->GetXaxis()->FindBin(1e7);
  can->Divide(2);
  can->cd(1);
  t->Project("h1","rate",Form("(hit.det==28 && hit.e>0.001 && hit.pid==11 %s)",rSelect.c_str()));
  double bf = h1->Integral(1,b1);
  double tot = h1->Integral();
  h1->SetTitle(Form("%s, %4.2f<1e7",h1->GetTitle(),bf/tot));
  h1->DrawCopy();
  gPad->SetLogy(1);
  gPad->SetGridy(1);

  can->cd(2);
  t->Project("h2","rate",Form("rate*(hit.det==28 && hit.e>0.001 && hit.pid==11 %s)",rSelect.c_str()),"h");
  bf = h2->Integral(1,b1);
  tot = h2->Integral();
  h2->SetTitle(Form("%s, %4.2f<1e7",h2->GetTitle(),bf/tot));
  h2->DrawCopy("h");
  gPad->SetLogy(1);
  gPad->SetGridy(1);

  cout<<"\t"<<h1->Integral(0,201)<<"\t"<<h2->Integral(0,201)<<endl;
  delete h1,h2;
}
