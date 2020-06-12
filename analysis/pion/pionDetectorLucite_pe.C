// Run using (for example):
//   build/reroot -l -q 'analysis/pion/pionDetectorLucite_pe.C("pionDetectorLucite_e.root")'

void pionDetectorLucite_pe(const TString& files)
{
  TChain* T = new TChain("T");
  T->Add(files);

  std::vector<remollEventParticle_t>* parts = 0;
  std::vector<remollGenericDetectorHit_t>* hits = 0;

  T->SetBranchAddress("hit", &hits);
  T->SetBranchAddress("part", &parts);

  TH1F hpe("hpe","Photo-electrons on pion lucite detector photo-cathode",500,0,100);

  // Loop over events
  for (size_t iev = 0; iev < T->GetEntries(); iev++) {
    T->GetEntry(iev);

    Int_t nlu = 0;
    Int_t npe = 0;
    for (size_t ihit = 0; ihit < hits->size(); ihit++) {
      remollGenericDetectorHit_t hit = hits->at(ihit);

      // Count primary hits on detector
      if (hit.det == 8001 && hit.trid == 1) {
        nlu++;
      }

      // Count PE hits on photocathode
      if (hit.det == 8000 && hit.pid == 0) {
        npe++;
      }
    }

    if (nlu > 0) hpe.Fill(npe);
  }
  std::cout << hpe.GetMean() << " " << hpe.GetMeanError() << std::endl;
}
