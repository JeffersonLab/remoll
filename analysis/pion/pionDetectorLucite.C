// Run using (for example):
//   build/reroot -l -q 'analysis/pion/pionDetectorLucite.C("pionDetectorLucite_e.root")'

void pionDetectorLucite(const TString& files)
{
  TChain* T = new TChain("T");
  T->Add(files);

  Double_t rate = 0;
  std::vector<remollEventParticle_t>* parts = 0;
  std::vector<remollGenericDetectorHit_t>* hits = 0;

  T->SetBranchAddress("rate", &rate);
  T->SetBranchAddress("hit", &hits);
  T->SetBranchAddress("part", &parts);

  TH1F hpe("hpe","Photo-electrons on pion lucite detector photo-cathode",500,0,500);
  TH1F hpe_t("hpe_t","Photo-electron arrival times on pion lucite detector photo-cathode",200,0,200);
  TH2F hpe_n("hpe_n","Number of events versus impact position on lucite",20,-200,+200,20,-200,+200);
  TH2F hpe_xy("hpe_xy","Photo-electrons versus impact position on lucite",20,-200,+200,20,-200,+200);

  // Loop over events
  for (size_t iev = 0; iev < T->GetEntries(); iev++) {
    T->GetEntry(iev);

    // Count PE hits on photocathode
    Int_t npe = 0;
    Double_t vx, vy;
    for (size_t ihit = 0; ihit < hits->size(); ihit++) {
      remollGenericDetectorHit_t hit = hits->at(ihit);
      if (hit.det == 8000 && hit.pid == 0) {
        npe++;
        hpe_t.Fill(hit.t);
      }
    }
    for (size_t ipart = 0; ipart < parts->size(); ipart++) {
      remollEventParticle_t part = parts->at(ipart);
      vx = part.vx;
      vy = part.vy;
    }
    hpe.Fill(npe);
    hpe_n.Fill(vx,vy);
    hpe_xy.Fill(vx,vy,npe);
  }

  TCanvas c;
  hpe.Draw();
  c.SaveAs("images/pionDetectorLucite_pe.png");
  hpe_t.Draw();
  c.SaveAs("images/pionDetectorLucite_pe_t.png");
  hpe_n.Draw("colz");
  c.SaveAs("images/pionDetectorLucite_pe_n.png");
  hpe_xy.Draw("colz");
  c.SaveAs("images/pionDetectorLucite_pe_xy.png");
  hpe_xy.Divide(&hpe_n);
  hpe_xy.Draw("colz");
  c.SaveAs("images/pionDetectorLucite_pe_xy_by_n.png");
}
