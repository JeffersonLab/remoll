void runremollana(){
    gStyle->SetOptStat(0);

    gSystem->Load("libremollroot.so");
    gROOT->ProcessLine(".L remollH1.C+");
    gROOT->ProcessLine(".L remollAna.C+");

    // Do all three types

    TFile *f[3];
    TChain *T[3];
    remollAna *ana[3];
    remollRunData *data[3];

    char filename[3][255] = {
      "remollout_ee_*.root",
      "remollout_ep_*.root",
      "remollout_in_*.root",
    };

    char genname[3][255] = {"moller", "elastic", "inelastic"};
    
    int nfile[3];

    for( int i; i < 3; i++ ){
	T[i] = new TChain("T");
	nfile[i] = T[i]->Add(filename[i]);

	data[i] = new remollRunData;
	data[i]->SetGenName(genname[i]);

	ana[i] = new remollAna(T[i], data[i]->GetGenName());
	ana[i]->SetNfile(nfile[i]);

	ana[i]->Loop(data[i]->GetGenName());

	ana[i]->Draw(true, data[i]->GetGenName());
	ana[i]->Write(data[i]->GetGenName());

    }

}

