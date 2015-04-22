TGraph *grD;

void makeXsW2plot(){
  c1=new TCanvas("c1","c1");
  c1->Print("y_XsW2.pdf[","pdf");
  TGraph *g1=makeOne("../output/remollAl_75deg_210GeV_100k.root",1.3,2.7);
  g1->SetName("g1");
  g1->SetTitle("xs for 75 deg 2.10 GeV (1.3<Q2<2.7)");
  compareOne(g1,"~/moller/aluminium/data/mamyanDiffXsection/mam_210_75_13-27.dat");

  TGraph *g2=makeOne("../output/remollAl_15deg_312GeV_100k.root",0.2,0.3);
  g2->SetName("g2");
  g2->SetTitle("xs for 15 deg 3.12 GeV (0.2<Q2<0.3)");
  compareOne(g2,"~/moller/aluminium/data/mamyanDiffXsection/mam_312_15_02-03.dat");

  c1->Print("y_XsW2.pdf]","pdf");
  
}

TGraph *makeOne(string fn,float q1=1.3,float q2=2.7){
  TGraph *g=new TGraph(100);
  TFile *fin=TFile::Open(fn.c_str());
  TTree *t=(TTree*)fin->Get("T");

  t->Draw("ev.xs/ev.beamp:ev.W2",Form("ev.Q2>%f && ev.Q2<%f",q1,q2));
  c1->Print("y_XsW2.pdf","pdf");    
  gr=new TGraph(t->GetSelectedRows(),t->GetV2(),t->GetV1());

  double xl=TMath::MinElement(gr->GetN(),gr->GetX());
  double xh=TMath::MaxElement(gr->GetN(),gr->GetX());
  double xw=(xh-xl)/100.;
  double x[5000],y[5000];
  int n;
  for(int i=0;i<100;i++){
    n=0;
    double xv=xl+i*xw;
    for(int j=0;j<gr->GetN();j++)
      if(TMath::AreEqualAbs(xv,gr->GetX()[j],xw/2)){
	x[n]=gr->GetX()[j];
	y[n]=gr->GetY()[j];
	n++;
      }
    g->SetPoint(i,TMath::Mean(n,x),TMath::Mean(n,y));	
  }
  
  return g;
  fin->Close();
}

void compareOne(TGraph *g,string dt){
  grD=new TGraph(dt.c_str());
  double xl=TMath::MinElement(grD->GetN(),grD->GetX());
  double xh=TMath::MaxElement(grD->GetN(),grD->GetX());
  
  TF1 *f=new TF1("f",fitf,xl,xh,1);
  g->Draw("APL");
  grD->SetMarkerColor(2);
  grD->SetMarkerStyle(20);
  f->SetParameter(0,2.2e+19);
  g->Fit("f","","",xl,xh);
  double scale=f->GetParameter(0);
  //double scale=2.18902e+19;
  cout<<"Scale: " <<scale<<" "<<grD->GetN()<<endl;
  for(int i=0;i<grD->GetN();i++){
    grD->GetY()[i]*=scale;
    //cout<<i<<" "<<grD->GetY()[i]<<endl;
  }
  grD->Draw("P");
  c1->Print("y_XsW2.pdf","pdf");

}


Double_t fitf(Double_t *v, Double_t *par)
{ 
  return par[0]*grD->Eval(v[0]);
}
