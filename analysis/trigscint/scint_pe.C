// Run using (for example):
//   build/reroot -l -q 'analysis/trigscint/scint_pe.C("o_trigscint_moller_lg350mm_10k_pe.root")'

void scint_pe(const TString& files)
{
  TChain* T = new TChain("T");
  T->Add(files);

  std::vector<remollEventParticle_t>* parts = 0;
  std::vector<remollGenericDetectorHit_t>* hits = 0;

  T->SetBranchAddress("hit", &hits);
  T->SetBranchAddress("part", &parts);

  TH1F primary_e("primary_e","Primary electrons hitting the Scintillator; Momemtum (MeV)",500,500,9000);
  TH1F primary_e_vx("primary_e_vx","Primary electrons hitting the Scintillator;Vertex X (mm)",300,-150,150);
  TH1F primary_e_vy("primary_e_vy","Primary electrons hitting the Scintillator;Vertex Y (mm)",520,-260,260);
  TH1F hpe("hpe","Photo-electrons on scintillator detector photo-cathode",100,0,100);
  TH1F hpe_bottom("hpe_bottom","Photo-electrons on scintillator detector photo-cathode from bottom (-250 to -100 mm)",100,0,100);
  TH1F hpe_center("hpe_center","Photo-electrons on scintillator detector photo-cathode from center (-100 to 100 mm)",100,0,100);
  TH1F hpe_top("hpe_top","Photo-electrons on scintillator detector photo-cathode from top (100 to 250 mm)",30,0,60);
  TH1F hpe_t("hpe_t","Photon arrival times on scintillator detector photo-cathode; drift time (ns)",200,0,1000);
  TH1F hpe_t_bottom("hpe_t_bottom","Photon (-250 to -100 mm) arrival  times on scintillator detector photo-cathode; drift time (ns)",200,0,50);
  //needs to find a way to separate hit times based on primary charge particle vertex
  TH1F hpe_t_top("hpe_t_top","Photon (100 to 250 mm) arrival times on scintillator detector photo-cathode; drift time (ns)",200,0,50);
  TH1F hpe_t_center("hpe_t_center","Photon  (-100 to 100 mm) arrival times on scintillator detector photo-cathode; drift time (ns)",200,0,50);
  TH2F hpe_n("hpe_n","Number of events versus impact position on scintillator",20,-300,+300,20,-300,+300);
  TH2F hpe_xy("hpe_xy","Photo-electrons versus impact position on scintillator",20,-300,+300,20,-300,+300);

  // Loop over events
  for (size_t iev = 0; iev < T->GetEntries(); iev++) {
    T->GetEntry(iev);

    Int_t nlu = 0;
    Int_t npe = 0;
    
    Double_t vx, vy,p;	
    for (size_t ihit = 0; ihit < hits->size(); ihit++) {
      remollGenericDetectorHit_t hit = hits->at(ihit);

      // Count primary hits on detector at the moment no other detector is available in scint geometry
      //if (hit.det == 10 && hit.trid == 1) {
        //nlu++;
      //}

      // Count PE hits on photocathode
      if (hit.det == 10 && hit.pid == 0) {
	if (hit.t>=0 && hit.t<2000){
        npe++;
	hpe_t.Fill(hit.t);
	}
      }
    }
    for (size_t ipart = 0; ipart < parts->size(); ipart++) {
      remollEventParticle_t part = parts->at(ipart);
      vx = part.vx;
      vy = part.vy;
      p = part.p;
    }

    //if (nlu > 0)
    if (npe>0){
    primary_e.Fill(p);
    primary_e_vx.Fill(vx);
    primary_e_vy.Fill(vy);
    hpe.Fill(npe);
    hpe_n.Fill(vx,vy);
    hpe_xy.Fill(vx,vy,npe);
    if (vy<-100 && vy>=-300){
      hpe_bottom.Fill(npe);
    }else if (vy<100 && vy>=-100){
      hpe_center.Fill(npe);
    }else if (vy<300 && vy>=100){
      hpe_top.Fill(npe);
    }
    }
  }
  std::cout << "AVG PE Whole Scint " << hpe.GetMean() << " " << hpe.GetMeanError() << std::endl;
  std::cout << "AVG PE Bottom Scint " << hpe_bottom.GetMean() << " " << hpe_bottom.GetMeanError() << std::endl;
  std::cout << "AVG PE center Scint " << hpe_center.GetMean() << " " << hpe_center.GetMeanError() << std::endl;
  std::cout << "AVG PE Top Scint " << hpe_top.GetMean() << " " << hpe_top.GetMeanError() << std::endl;
  /*
  TCanvas c;
  hpe.Draw();
  c.SaveAs("tmp/full_shortlg_mylar_scint_pe.png");
  hpe_bottom.Draw();
  c.SaveAs("tmp/full_shortlg_mylar_scint_bottom_pe.png");
  hpe_center.Draw();
  c.SaveAs("tmp/full_shortlg_mylar_scint_center_pe.png");
  hpe_top.Draw();
  c.SaveAs("tmp/full_shortlg_mylar_scint_top_pe.png");
  hpe_t.Draw();
  c.SaveAs("tmp/full_shortlg_mylar_scint_pe_t.png");
  hpe_n.Draw("colz");
  c.SaveAs("tmp/full_shortlg_mylar_scint_pe_n.png");	
  hpe_xy.Draw("colz");
  c.SaveAs("tmp/full_shortlg_mylar_scint_pe_xy.png");
  primary_e_vy.Draw();
  c.SaveAs("tmp/full_shortlg_mylar_scint_primary_e_vy.png");
  primary_e_vx.Draw();
  c.SaveAs("tmp/full_shortlg_mylar_scint_primary_e_vx.png");
  primary_e.Draw();
  c.SaveAs("tmp/full_shortlg_mylar_scint_primary_e_p.png");
  */
}
