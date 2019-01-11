void Analyze(TChain* T)
{
  std::vector<remollGenericDetectorHit_t>* hits = 0;
  std::vector<remollGenericDetectorSum_t>* sums = 0;
  T->SetBranchAddress("hit", &hits);
  T->SetBranchAddress("sum", &sums);
  for (size_t iev = 0; iev < T->GetEntries(); iev++) {
    T->GetEntry(iev);
    Double_t edep = 0;
    std::cout << "hit:";
    for (size_t ihit = 0; ihit < hits->size(); ihit++) {
      remollGenericDetectorHit_t hit = hits->at(ihit);
      std::cout << " " << hit.edep;
      edep += hit.edep;
    }
    std::cout << " MeV" << std::endl;
    std::cout << "hit sum = " << edep << " MeV" << std::endl;

    for (size_t isum = 0; isum < sums->size(); isum++) {
      remollGenericDetectorSum_t sum = sums->at(isum);
      std::cout << "sum: " << sum.edep << " MeV" << std::endl;
    }
  }
}

void issue172()
{
  TChain* lead = new TChain("T");
  lead->Add("issue172_lead.root");

  std::cout << "Lead" << std::endl;
  Analyze(lead);

  TChain* krypt = new TChain("T");
  krypt->Add("issue172_krypt.root");

  std::cout << "Krypt" << std::endl;
  Analyze(krypt);
}


