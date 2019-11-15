// Run using (for example):
//   build/reroot -l -q 'analysis/hyperon/hyperon_pion_rates.C("hyperon_lambda_2M.root")'

void hyperon_pion_rates(const TString& files)
{
  TChain* T = new TChain("T");
  T->Add(files);

  std::vector<remollEventParticle_t>* parts = 0;
  std::vector<remollGenericDetectorHit_t>* hits = 0;

  T->SetBranchAddress("hit", &hits);
  T->SetBranchAddress("part", &parts);

  TH1F h_hit_r("h_hit_r","Pion hit radius at the main detector plane",20,0,2000);

  // Loop over events
  for (size_t iev = 0; iev < T->GetEntries(); iev++) {
    T->GetEntry(iev);

    // Store hits on main detector
    for (size_t ihit = 0; ihit < hits->size(); ihit++) {
      remollGenericDetectorHit_t hit = hits->at(ihit);
      if (hit.det == 28 && hit.pid == -211) {
        h_hit_r.Fill(hit.r);
      }
    }
    for (size_t ipart = 0; ipart < parts->size(); ipart++) {
      remollEventParticle_t part = parts->at(ipart);
    }
  }

  TCanvas c;
  h_hit_r.Draw();
  c.SaveAs("images/hyperon_pion_rates_hit_r.png");
}
