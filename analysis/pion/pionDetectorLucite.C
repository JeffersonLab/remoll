#include <TChain.h>

void pionDetectorLucite(const TString& files)
{
  TChain* T = new TChain("T");
  T->Add(files);

  std::vector<remollEventParticle_t>* parts = 0;
  std::vector<remollGenericDetectorHit_t>* hits = 0;

  T->SetBranchAddress("hit", &hits);
  T->SetBranchAddress("part", &parts);

  TH1F hpe("hpe","Photo-electrons on pion lucite detector photo-cathode",500,0,500);

  // Loop over events
  for (size_t iev = 0; iev < T->GetEntries(); iev++) {
    T->GetEntry(iev);

    // Count PE hits on photocathode
    Int_t npe = 0;
    for (size_t ihit = 0; ihit < hits->size(); ihit++) {
      remollGenericDetectorHit_t hit = hits->at(ihit);
      if (hit.det == 8000 && hit.pid == 0) npe++;
    }
    hpe.Fill(npe);
  }

  TCanvas c;
  hpe.Draw();
  c.SaveAs("pionDetectorLucite.png");
}
