void Analyze(TChain* T)
{
  TH1F h_log10_hit_p_neutrons("h_log10_hit_p_neutrons", "log10(p) for neutrons", 50,-5.0,+5.0);
  std::vector<remollGenericDetectorHit_t>* hits = 0;
  T->SetBranchAddress("hit", &hits);
  for (size_t iev = 0; iev < T->GetEntries(); iev++) {
    T->GetEntry(iev);
    for (size_t ihit = 0; ihit < hits->size(); ihit++) {
      remollGenericDetectorHit_t hit = hits->at(ihit);
      if (hit.pid == 2112) {
        h_log10_hit_p_neutrons.Fill(log10(hit.p));
      }
    }
  }
  for (int i = 1; i <= h_log10_hit_p_neutrons.GetNbinsX(); i++) {
    std::cout << h_log10_hit_p_neutrons.GetBinCenter(i) << ": "
              << h_log10_hit_p_neutrons.GetBinContent(i) << std::endl;
  }
}

void issue248()
{
  TChain* lead = new TChain("T");
  lead->Add("issue248.root");
  Analyze(lead);
}


