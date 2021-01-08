// Run using (for example):
//   build/reroot -l -q 'analysis/pion/pionDetectorLucite_surround.C("remollout_pi-*.root",36,18)'

void pionDetectorLucite_surround(const TString& files, const int nbins_phi = 36, const int nbins_theta = 18)
{
  TChain* T = new TChain("T");
  T->Add(files);

  Double_t rate = 0;
  remollUnits_t* units = 0;
  std::vector<remollEventParticle_t>* parts = 0;
  std::vector<remollGenericDetectorHit_t>* hits = 0;
  std::vector<remollGenericDetectorSum_t>* sums = 0;

  T->SetBranchAddress("rate", &rate);
  T->SetBranchAddress("hit", &hits);
  T->SetBranchAddress("sum", &sums);
  T->SetBranchAddress("part", &parts);
  T->SetBranchAddress("units", &units);

  TH2D hpolar_pe("hpolar_pe","Photo-electrons on pion lucite detector photo-cathode",nbins_phi,-180,180,nbins_theta,0,180);
  TH2D hpolar_n("hpolar_n","Photo-electrons on pion lucite detector photo-cathode",nbins_phi,-180,180,nbins_theta,0,180);

  // Loop over events
  for (size_t iev = 0; iev < T->GetEntries(); iev++) {
    T->GetEntry(iev);

    // Get beam direction
    remollEventParticle_t part = parts->at(0);

    // Count PE hits on photocathode
    for (size_t isum = 0; isum < sums->size(); isum++) {
      remollGenericDetectorSum_t sum = sums->at(isum);
      if (sum.det == 8000) {
        hpolar_pe.Fill(part.ph/units->deg, part.th/units->deg, sum.n);
        hpolar_n.Fill(part.ph/units->deg, part.th/units->deg);
      }
    }
  }

  TH2D hpolar(hpolar_pe);
  hpolar.Divide(&hpolar_n);
  hpolar.SetStats(false);

  TCanvas c1;
  c1.SetTheta(90);
  c1.SetPhi(180);
  hpolar.Draw("lego2 polz");//pol surf z");
  c1.SaveAs("images/pionDetectorLucite_surround_polar.png");

  TCanvas c2;
  hpolar.Draw("colz");
  c2.SaveAs("images/pionDetectorLucite_surround_rectilinear.png");
}
