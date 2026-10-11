// background_plotter.C
// This macro draws two sim results side by side on one bar plot, so we can see which component
// changed. It reads two background_counts.root files from analysis/background_fill.C and turns
// counts into a rate
// Moller rate % = 100 * count / (nPrimary / beamPerMoller)
// Each sim result is divided by its own nPrimary, so they do not need same number of events.
// how to run:
// build/reroot -l -b -q 'analysis/background_plotter.C("simA/background_counts.root", "simB/background_counts.root", "simA","simB","g","",-1,-1,3300,"compare_out")'
// arg1,2 = two simulation files
// arg3,4 = labels for the legend
// arg5 = "e" or "g"
// arg6 = "", "closed", or "open"
// arg7,8 = nPrimary each (-1 read from file)
// arg9 = beamPerMoller (3300)
// arg10 = output directory

void background_plotter(TString fileA, TString fileB,
			TString labelA ="A", TString labelB ="B",
			TString species = "g", // "e" or "g"
			TString sector = "",   // "", "closed" or "open"
			double nPrimaryA = -1, double nPrimaryB = -1,
			int beamPerMoller = 3300,
			TString outdir = "compare_out")
{
  gStyle->SetOptStat(0);
  gSystem->mkdir(outdir, kTRUE);

  int cbA = TColor::GetColor("#009E73");   // green = simA
  int cbB = TColor::GetColor("#CC79A7");   // purple = simB
  TString ssuf = (sector=="") ? "" : "_"+sector;

  //open ROOT files and take component counts
  TFile *fA = TFile::Open(fileA), *fB = TFile::Open(fileB);
  if(!fA||fA->IsZombie()||!fB||fB->IsZombie()){cout<<"Cannot open one of the inputs"<<endl; return; }
  TH1D *hA = (TH1D*)fA->Get("hComp_"+species+ssuf);
  TH1D *hB = (TH1D*)fB->Get("hComp_"+species+ssuf);
  if(!hA||!hB){cout<<"Missing hComp_"<<species<<ssuf<<" in one of the files"<<endl; return; }

  //each simulation is normalized by its own number of beam events
  if(nPrimaryA<=0){ TH1D*n=(TH1D*)fA->Get("hNprimary"); nPrimaryA = n?n->GetBinContent(1):0; }
  if(nPrimaryB<=0){ TH1D*n=(TH1D*)fB->Get("hNprimary"); nPrimaryB = n?n->GetBinContent(1):0; }
  double NmA = nPrimaryA/beamPerMoller;
  double NmB = nPrimaryB/beamPerMoller;
  if(NmA<=0||NmB<=0){cout<<"Bad nPrimary (A="<<nPrimaryA<<", B="<<nPrimaryB<<")"<<endl; return; }

  int nComp = hA->GetNbinsX();  // number of components

  TString spWord = (species=="g") ? "photons" : "e^{#pm}";
  TString secWord = (sector=="") ? "all sectors" : sector+" sectors";
  TString title = "Backgrounds, "+spWord+", E>1 MeV, pz>0, Ring 5, "+secWord;

  //three plots; mode 0 = all,  1 = no quartz, 2 = quartz only
  for(int mode=0; mode<3; mode++){

    //to count how many components this plot will have
    std::vector<int>keep;
    for(int c=0; c<nComp;c++){
      bool isQuartz = TString(hA->GetXaxis()->GetBinLabel(c+1)).BeginsWith("Quartz");
      if(mode==1 && isQuartz) continue; // everything except quartz
      if(mode==2 && !isQuartz) continue; // quartz only
      keep.push_back(c);   // number of bars/components
    }
    int n = keep.size();

    //   one histogram per simulation
    TH1D *bA = new TH1D(Form("bA%d",mode), title+";;Moller Rate %", n,0,n);
    TH1D *bB = new TH1D(Form("bB%d",mode), "", n,0,n);

    for(int k=0; k<n; k++){
      int c = keep[k];
      bA->SetBinContent(k+1, 100.0*hA->GetBinContent(c+1)/NmA);
      bB->SetBinContent(k+1, 100.0*hB->GetBinContent(c+1)/NmB);
      bA->GetXaxis()->SetBinLabel(k+1, hA->GetXaxis()->GetBinLabel(c+1));
    }

    double labSize = 0.022, txtSize=0.012;
    if(mode==1){labSize=0.028; txtSize=0.016; }
    if(mode==2){labSize=0.040; txtSize=0.026; }

    double ymax = TMath::Max(bA->GetMaximum(), bB->GetMaximum());
    bA->SetMinimum(0);
    bA->SetMaximum(1.18*ymax);
    bA->GetXaxis()->LabelsOption("v");
    bA->GetXaxis()->SetLabelSize(labSize);

    bA->SetFillColor(cbA); bA->SetBarWidth(0.42); bA->SetBarOffset(0.06);  //simA on the left
    bB->SetFillColor(cbB); bB->SetBarWidth(0.42); bB->SetBarOffset(0.52);  //simB on the right

    //draw
    TCanvas *c = new TCanvas(Form("c%d",mode),"compare",1700,850);
    c->SetLeftMargin(0.08); c->SetRightMargin(0.03);
    c->SetTopMargin(0.08); c->SetBottomMargin(0.22);
    bA->Draw("bar");
    bB->Draw("bar same");

    //print values on top of each bar
    TLatex tx; tx.SetTextAngle(90); tx.SetTextAlign(12); tx.SetTextSize(txtSize);
    for(int k=0;k<n;k++){
      double va = bA->GetBinContent(k+1), vb = bB->GetBinContent(k+1);
      tx.SetTextColor(cbA); tx.DrawLatex(k+0.27, va+0.01*ymax, Form("%.3f",va));
      tx.SetTextColor(cbB); tx.DrawLatex(k+0.73, vb+0.01*ymax, Form("%.3f",vb));
    }

    TLegend *lg = new TLegend(0.10, 0.80,0.22, 0.917);
    lg->SetBorderSize(0);
    lg->SetFillStyle(0);
    lg->AddEntry(bA,labelA,"f");
    lg->AddEntry(bB,labelB,"f");
    lg->Draw();

    TString tag = "_compare";
    if(mode==1) tag = "_compare_zoom";
    if(mode==2) tag = "_compare_quartz";
    c->SaveAs(outdir+"/background_"+species+ssuf+tag+".pdf");
  }
  cout<<"Wrote "<<labelA<<" vs "<<labelB<<" comparison to "<<outdir<<"/"<<endl;
}
