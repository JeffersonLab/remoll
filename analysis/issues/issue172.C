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
      for (size_t ipid = 0; ipid < sum.by_pid.size(); ipid++) {
        std::cout << "sum [pid = " << sum.by_pid[ipid].pid << "]: "
                  << sum.by_pid[ipid].edep << " MeV at ("
                  << sum.by_pid[ipid].x << ", "
                  << sum.by_pid[ipid].y << ", "
                  << sum.by_pid[ipid].z << ") mm "
                  << std::endl;
      }
    }
  }
}

void issue172()
{
  TChain* lead_all = new TChain("T");
  lead_all->Add("issue172_lead_all.root");

  std::cout << "Lead (all)" << std::endl;
  Analyze(lead_all);

  TChain* lead = new TChain("T");
  lead->Add("issue172_lead.root");

  std::cout << "Lead" << std::endl;
  Analyze(lead);

  TChain* krypt = new TChain("T");
  krypt->Add("issue172_krypt.root");

  std::cout << "Krypt" << std::endl;
  Analyze(krypt);

  TChain* userlimits = new TChain("T");
  userlimits->Add("issue172_userlimits.root");

  std::cout << "Userlimits" << std::endl;
  Analyze(userlimits);
}


