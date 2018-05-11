// compare_histos_in_two_files.C
//
// Purpose: Easily compare a set of 1D histograms in two identically structured ROOT files
//          by plotting the two histograms next to each other, plotting their ratio and
//          their difference.
//
// Usage:
//
// Interactively, load the script and run with two filenames as argument, e.g.
//   .L analysis/compare_histos_in_two_files.C
//   compare_histos_in_two_files("~/git/remoll_develop/rootfiles/tests/commit/analysis/test_moller.root","~/git/remoll/rootfiles/tests/commit/analysis/test_moller.root")
//
// From the command line, pass the arguments (in single quotes to make sure the
// double quotes are maintained), e.g.
//   root -l analysis/compare_histos_in_two_files.C'("~/git/remoll_develop/rootfiles/tests/commit/analysis/test_moller.root","~/git/remoll/rootfiles/tests/commit/analysis/test_moller.root")'
// Do not use the q flag or the canvases will close immediately.
//

#include "TFile.h"

void compare_histos_in_two_files(TString filename1, TString filename2) {
  TFile* file1 = new TFile(filename1);
  TFile* file2 = new TFile(filename2);

  TIter next1(file1->GetListOfKeys());
  TKey* key1;
  while ((key1 = (TKey*)next1())) {

    TString name = key1->GetName();
    cout << name << endl;

    TKey* key2 = file2->GetKey(name);
    if (!key2) continue;

    TClass* cl1 = gROOT->GetClass(key1->GetClassName());
    if (!cl1->InheritsFrom("TH1")) continue;
    TH1* h1 = (TH1*) key1->ReadObj();

    TClass* cl2 = gROOT->GetClass(key2->GetClassName());
    if (!cl2->InheritsFrom("TH1")) continue;
    TH1* h2 = (TH1*) key2->ReadObj();

    Float_t xmin = h1->GetXaxis()->GetXmin();
    Float_t xmax = h1->GetXaxis()->GetXmax();

    TCanvas* c = new TCanvas(name);
    c->Divide(2,2);
    c->cd(1);
    h1->Draw();
    c->cd(2);
    h2->Draw();

    c->cd(3);
    TH1* hdivide = (TH1*) h1->Clone("hdivide");
    hdivide->Divide(h2);
    hdivide->Draw();
    TLine *line1 = new TLine(xmin,1.0,xmax,1.0);
    line1->SetLineColor(kRed);
    line1->Draw();

    c->cd(4);
    TH1* hsubtract = (TH1*) h1->Clone("hsubtract");
    hsubtract->Add(h2,-1);
    hsubtract->Draw();
    TLine *line0 = new TLine(xmin,0.0,xmax,0.0);
    line0->SetLineColor(kRed);
    line0->Draw();

  }

}
