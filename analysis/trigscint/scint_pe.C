// Run using (for example):
//   build/reroot -l -q 'analysis/trigscint/scint_pe.C("o_trigscint_moller_248events_10k.root")'

void scint_pe(const TString& files)
{
  TChain* T = new TChain("T");
  T->Add(files);

  std::vector<remollEventParticle_t>* parts = 0;
  std::vector<remollGenericDetectorHit_t>* hits = 0;

  T->SetBranchAddress("hit", &hits);
  T->SetBranchAddress("part", &parts);

  TH1F hpe("hpe","Photo-electrons on scintillator detector photo-cathode",500,0,100);
  TH1F hpe_t("hpe_t","Photo-electron arrival times on scintillator detector photo-cathode",200,0,200);
  TH2F hpe_n("hpe_n","Number of events versus impact position on scintillator",20,-200,+200,20,-300,+300);
  TH2F hpe_xy("hpe_xy","Photo-electrons versus impact position on scintillator",20,-200,+200,20,-300,+300);

  // Loop over events
  for (size_t iev = 0; iev < T->GetEntries(); iev++) {
    T->GetEntry(iev);

    Int_t nlu = 0;
    Int_t npe = 0;
    Double_t vx, vy;	
    for (size_t ihit = 0; ihit < hits->size(); ihit++) {
      remollGenericDetectorHit_t hit = hits->at(ihit);

      // Count primary hits on detector at the moment no other detector is available in scint geometry
      //if (hit.det == 10 && hit.trid == 1) {
        //nlu++;
      //}

      // Count PE hits on photocathode
      if (hit.det == 10 && hit.pid == 0) {
        npe++;
	hpe_t.Fill(hit.t);
      }
    }
    for (size_t ipart = 0; ipart < parts->size(); ipart++) {
      remollEventParticle_t part = parts->at(ipart);
      vx = part.vx;
      vy = part.vy;
    }

    //if (nlu > 0) 
	hpe.Fill(npe);
	hpe_n.Fill(vx,vy);
    	hpe_xy.Fill(vx,vy,npe);
  }
  std::cout << hpe.GetMean() << " " << hpe.GetMeanError() << std::endl;
  TCanvas c;
  hpe.Draw();
  c.SaveAs("shortlg_scint_pe.png");
  hpe_t.Draw();
  c.SaveAs("shortlg_scint_pe_t.png");
  hpe_n.Draw("colz");
  c.SaveAs("shortlg_scint_pe_n.png");	
  hpe_xy.Draw("colz");
  c.SaveAs("shortlg_scint_pe_xy.png");
}
