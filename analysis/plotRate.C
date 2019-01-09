/*
Run with:

>root -l
>gSystem->Load("../build/libremollroot.so");
>.L plotRate.C

>doOne("/some/rootfile.root")
or
>plotRate("/some/list.lst")
*/
void updateMean(double &avg, double &uncert, double val, int n);
void doOne(string fnm, int ring5toggle=1, int print=0);

void plotRate(string lst, int ring5toggle=1){
  ifstream fin(lst);
  string data;
  while(getline(fin,data)){
    //cout<<data<<endl;
    doOne(data,ring5toggle,0);
  }
}

void doOne(string fnm,int ring5toggle=1, int print=0){

  TCanvas *c1;
  string onm("");
  if(print){
    c1=new TCanvas("c1","c1");
    string onm = Form("y_plotRate%s.pdf",fnm.substr(fnm.find("r"),fnm.rfind(".")).c_str());
    gStyle->SetOptStat("eMRoui");
    c1->Print(Form("%s[",onm.c_str()),"pdf");
  }

  TFile *fin = TFile::Open(fnm.c_str(),"READ");
  TTree *t = (TTree *)fin->Get("T");

  string rSelect="&& hit.r>0.6 && hit.r<1.2";//all det
  if(ring5toggle==1)
    rSelect="&& hit.r>0.935 && hit.r<1.1";

  long nEntries = t->GetEntries();
  int nLoop = nEntries / 1000000;
  double hitAvg(0), hitUncert(0), rateAvg(0), rateUncert(0);
  for(int i=0;i<nLoop;i++){
    TH1D *hr = new TH1D("hr",Form("rate %d",i),200,0,1e11);
    TH1D *hh = new TH1D("hh",Form("hits %d",i),200,0,1e11);
    t->Project("hh","rate",
	       Form("(hit.det==28 && hit.e>0.001 && hit.pid==11 %s)",rSelect.c_str()),
	       "",1000000,i*1000000);
    updateMean(hitAvg,hitUncert,hh->Integral(),i+1);
    if(print){
      hh->DrawCopy();
      gPad->SetLogy(1);
      gPad->SetGridy(1);
      c1->Print(onm.c_str(),"pdf");
    }

    t->Project("hr","rate",
	       Form("rate*(hit.det==28 && hit.e>0.001 && hit.pid==11 %s)",rSelect.c_str())
	       ,"h",1000000,i*1000000);
    updateMean(rateAvg,rateUncert,hr->Integral(),i+1);
    if(print){
      hr->DrawCopy();
      gPad->SetLogy(1);
      gPad->SetGridy(1);
      c1->Print(onm.c_str(),"pdf");
    }

    //cout<<i<<"\t"<<hitAvg<<"\t"<<hitUncert<<"\t"<<rateAvg<<"\t"<<rateUncert<<endl;
    delete hr;
    delete hh;
  }
  if(nLoop>=1){
    hitUncert = sqrt( hitUncert/(nLoop - 1) );
    rateUncert = sqrt( rateUncert/(nLoop - 1) );
  }else{
    hitUncert = 0;
    rateUncert = 0;
  }
  //cout<<"\t"<<hitAvg<<"\t"<<hitUncert<<"\t"<<rateAvg<<"\t"<<rateUncert<<endl;

  TH1D *h1=new TH1D("h1","hits ;rate[Hz]",200,0,1e11);
  TH1D *h2=new TH1D("h2","rate weighted ;rate[Hz]",200,0,1e11);
  int b1 = h1->GetXaxis()->FindBin(1e7);
  t->Project("h1","rate",Form("(hit.det==28 && hit.e>0.001 && hit.pid==11 %s)",rSelect.c_str()));
  double bf = h1->Integral(1,b1);
  double tot = h1->Integral();
  if(print){
    h1->SetTitle(Form("%s, %4.2f<1e7",h1->GetTitle(),bf/tot));
    h1->DrawCopy();
    gPad->SetLogy(1);
    gPad->SetGridy(1);
    c1->Print(onm.c_str(),"pdf");
  }

  t->Project("h2","rate",Form("rate*(hit.det==28 && hit.e>0.001 && hit.pid==11 %s)",rSelect.c_str()),"h");
  bf = h2->Integral(1,b1);
  tot = h2->Integral();
  if(print){
    h2->SetTitle(Form("%s, %4.2f<1e7",h2->GetTitle(),bf/tot));
    h2->DrawCopy("h");
    gPad->SetLogy(1);
    gPad->SetGridy(1);
    c1->Print(onm.c_str(),"pdf");
  }

  double factor = nEntries/1e6;
  cout<<fnm<<" \t"<<hitAvg*factor<<" \t"<<hitUncert*factor<<" \t"
      <<rateAvg*factor<<" \t"<<rateUncert*factor
      <<" \t"<<h1->Integral(0,201)<<" \t"<<h2->Integral(0,201)<<endl;
  delete h1,h2;
  fin->Close();
  if(print)
    c1->Print(Form("%s]",onm.c_str()),"pdf");

}

void updateMean(double &avg, double &uncert, double val, int n){
  double delta   = val - avg;
  double newMean = avg + delta/n;
  double delta2  = val - newMean;
  double newVar  = uncert + delta*delta2;

  //cout<<n<<"\t"<<val<<"\t"<<uncert<<"\t"<<delta<<"\t"<<newMean<<"\t"<<delta2<<"\t"<<newVar<<endl;
  avg = newMean;
  uncert = newVar;
}

