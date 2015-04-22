TCanvas *c1=new TCanvas("c1","c1",600,400);
string onm="y_F1check.pdf";
void F1xCheck()
{
  c1->Print(Form("%s[",onm.c_str()),"pdf");
  doAna("ev.Q2>0.495 && ev.Q2<0.505","../../Al_F1_q2_0.5.txt","0.495<Q2<0.505"); 
  doAna("ev.Q2>0.002 && ev.Q2<0.004","../../Al_F1_q2_0.003.txt","0.002<Q2<0.004"); 
  doAna("ev.Q2>1.9 && ev.Q2<2.1","../../Al_F1_q2_2.txt","1.9<Q2<2.1"); 
  c1->Print(Form("%s]",onm.c_str()),"pdf");
}

void doAna(string cuts,string f1data,string title){

  TGraph *gr=new TGraph(f1data.c_str());
  TFile *fin=TFile::Open("../output/remollAl_15deg_312GeV_100k_AisF1.root","READ");
  TTree *t=(TTree*)fin->Get("T");

  gr->SetTitle(title.c_str());
  gr->SetMarkerColor(2);
  gr->SetLineColor(2);
  gr->Draw("APL");
  t->Draw("ev.A/27:sqrt(ev.W2)",cuts.c_str(),"same");  
  fin->Close();
  c1->Print(Form("%s",onm.c_str()),"pdf");  
}
