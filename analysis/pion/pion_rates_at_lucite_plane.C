// Run using (for example):
//   build/reroot -l -q 'analysis/pion/pion_rates_at_lucite_plane.C("pion_rates_at_lucite_plane_evgen_pion_1k.root")'

void pion_rates_at_lucite_plane(const TString& files)
{
  TChain* T = new TChain("T");
  T->Add(files);

  Double_t rate = 0;
  std::vector<remollEventParticle_t>* parts = 0;
  std::vector<remollGenericDetectorHit_t>* hits = 0;

  T->SetBranchAddress("rate", &rate);
  T->SetBranchAddress("hit", &hits);
  T->SetBranchAddress("part", &parts);

  TH1F hnpe("hnpe","Photo-electrons on pion lucite detector photo-cathode",500,0,500);
  hnpe.GetXaxis()->SetTitle("#pe, 1 / bin"); hnpe.GetYaxis()->SetTitle("a.u.");
  TH1F htpe("htpe","Photo-electron arrival times on pion lucite detector photo-cathode",200,0,200);
  htpe.GetXaxis()->SetTitle("t [ns], 1 ns / bin"); htpe.GetYaxis()->SetTitle("a.u.");
  TH1F hr28("hr28","Hit radius at detector plane 28 (main det)",100,0,2000);
  hr28.GetXaxis()->SetTitle("r [mm], 20 mm / bin"); hr28.GetYaxis()->SetTitle("Hz/uA");
  TH1F hr29("hr29","Hit radius at detector plane 29 (pion det)",100,0,2000);
  hr29.GetXaxis()->SetTitle("r [mm], 20 mm / bin"); hr29.GetYaxis()->SetTitle("Hz/uA");
  TH2F hvrz8001("hvrz8001","Hit vertex r v. z for pion lucite hits",100,22000,25000,100,0,2000);
  hvrz8001.GetXaxis()->SetTitle("z [mm], 30 mm / bin"); hvrz8001.GetYaxis()->SetTitle("r [mm], 20 mm / bin");

  // Loop over events
  for (size_t iev = 0; iev < T->GetEntries(); iev++) {
    T->GetEntry(iev);

    // Count PE hits on photocathode
    Int_t npe = 0;
    for (size_t ihit = 0; ihit < hits->size(); ihit++) {
      remollGenericDetectorHit_t hit = hits->at(ihit);
      if (hit.det == 8000 && hit.pid == 0) {
        npe++;
        htpe.Fill(hit.t);
      }
    }
    hnpe.Fill(npe);

    // Process hits
    for (size_t ihit = 0; ihit < hits->size(); ihit++) {
      remollGenericDetectorHit_t hit = hits->at(ihit);
      if (hit.det == 28 && hit.pid != 22 && hit.pid != 0) {
        hr28.Fill(hit.r, rate);
      }
      if (hit.det == 29 && hit.pid != 22 && hit.pid != 0) {
        hr29.Fill(hit.r, rate);
      }
      if (hit.det == 8001 && hit.pid != 22 && hit.pid != 0) {
        hvrz8001.Fill(hit.vz, sqrt(hit.vx*hit.vx + hit.vy*hit.vy), rate);
      }
    }
  }

  TCanvas c;
  hr28.Draw();
  c.SaveAs("images/pion_rates_at_det_28_moller_plane.png");
  hr29.Draw();
  c.SaveAs("images/pion_rates_at_det_29_pion_plane.png");
  hvrz8001.Draw("colz");
  c.SaveAs("images/pion_rates_at_lucite_plane_vrz8001.png");
  hnpe.Draw();
  c.SaveAs("images/pion_rates_at_lucite_plane_npe.png");
  htpe.Draw();
  c.SaveAs("images/pion_rates_at_lucite_plane_tpe.png");
}
