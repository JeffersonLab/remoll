double drawOne(TCanvas*,TH1F*,int,int,double);
void printIntegral(TH1F*,int);
void drawZ0integOne(string);
void drawZ0integMD(string);

const int nZreg = 19;
const float zRegHigh[nZreg]={-3700,0,1000,3000,5000,7700,7900,12000,12500,18000,19050,22000,24000,26500,29200,29400,31400,32200,44990};
const string zRegNm[nZreg]={"tgt","tgtReg","coll1&2","USmag","coll3&Pb","usDSmag","coll5","dsDSmag","collar1","driftPipe","collar2","airUSMD","SM&Donut","finDetPipe","dumpNeckDown2App","dumpApp","App2NeckDown2","neckDown&Diff","dsDiffuser"};

void drawZ0integ(string fnm="/phenix/spin/phnxsp01/ciprian/mollerOut/beamGeoV2_radAnaV5.root"){
  //drawZ0integOne(fnm);
  drawZ0integMD(fnm);
}

void drawZ0integOne(string fnm="/phenix/spin/phnxsp01/ciprian/mollerOut/beamGeoV2_radAnaV5.root"){
  
  TFile *fin=TFile::Open(fnm.c_str(),"READ");
  auto *c1=new TCanvas();
  string part = "ep";
  string dmg = "Dmg0";
  TH1F *h=(TH1F*)fin->Get(Form("det28/d28_z0_R7_%s_%s_Erg0",part.c_str(),dmg.c_str()));
  printIntegral(h,0);
  double scale = drawOne(c1,h,0,1,0);
  int cls[4]={1,2,3,4};
  for(int i=1;i<4;i++){
    h=(TH1F*)fin->Get(Form("det28/d28_z0_R7_%s_%s_Erg%d",part.c_str(),dmg.c_str(),i));
    printIntegral(h,1);
    drawOne(c1,h,1,cls[i],scale);
  }

  fin->Close();  
}

void drawZ0integMD(string fnm="/phenix/spin/phnxsp01/ciprian/mollerOut/beamGeoV2_radAnaV5.root"){
  
  TFile *fin=TFile::Open(fnm.c_str(),"READ");
  auto *c1=new TCanvas();
  string part = "em";
  string dmg = "Dmg0";

  TH1F *h=(TH1F*)fin->Get(Form("det28/d28_z0_R1_%s_%s_Erg0",part.c_str(),dmg.c_str()));
  for(int j=1;j<7;j++){
    TH1F *h2=(TH1F*)fin->Get(Form("det28/d28_z0_R%d_%s_%s_Erg0",j,part.c_str(),dmg.c_str()));
    h->Add(h2);
  }
  printIntegral(h,0);
  double scale = drawOne(c1,h,0,1,0);
  int cls[4]={1,2,3,4};
  for(int i=1;i<4;i++){
    h=(TH1F*)fin->Get(Form("det28/d28_z0_R1_%s_%s_Erg%d",part.c_str(),dmg.c_str(),i));
    for(int j=1;j<7;j++){
      TH1F *h2=(TH1F*)fin->Get(Form("det28/d28_z0_R%d_%s_%s_Erg%d",j,part.c_str(),dmg.c_str(),i));
      h->Add(h2);
    }

    printIntegral(h,1);
    drawOne(c1,h,1,cls[i],scale);
  }

  fin->Close();  
}

double drawOne(TCanvas *c1,TH1F* h,int same, int cl, double inSc){
  double scale(0);
  c1->cd();
  if(same){
    h->SetLineColor(cl);
    h->Scale(inSc);
    h->DrawCopy("h&same");
  }else{
    scale = 1./ h->Integral();
    h->SetLineColor(cl);
    h->SetLineWidth(3);
    h->Scale(scale);
    //h->Scale(1./ h->Integral());
    h->DrawCopy("h");
    gPad->SetGridx(1);
    gPad->SetGridy(1);
    gPad->SetLogy(1);

    double max = h->GetMaximum();
    TLine *line=new TLine(0,0,1,1);
    line->SetLineWidth(2);
    line->SetLineColorAlpha(kOrange+5,75);
    for(int i=0;i<nZreg;i++){
      line->DrawLine(zRegHigh[i],0,zRegHigh[i],max);
    }
  }
  return scale;
}

void printIntegral(TH1F* h, int same){
  double tot = h->Integral();
  if(!same){
    cout<<"hName\t";
    for(int i=1;i<nZreg;i++)
      cout<<zRegNm[i]<<"\t";
    cout<<endl;
  }
  cout<<h->GetName()<<"V\t";
  for(int i=1;i<nZreg;i++){
    double zMinBin = 1;
    double zMaxBin = h->GetXaxis()->GetNbins();
    if(i!=1)
      zMinBin = h->GetXaxis()->FindBin(zRegHigh[i-1]);
    zMaxBin = h->GetXaxis()->FindBin(zRegHigh[i]);
    
    cout<<h->Integral(zMinBin,zMaxBin)<<"\t";
  }
  cout<<endl;

  cout<<h->GetName()<<"%\t";
  for(int i=1;i<nZreg;i++){
    double zMinBin = 1;
    double zMaxBin = h->GetXaxis()->GetNbins();
    if(i!=1)
      zMinBin = h->GetXaxis()->FindBin(zRegHigh[i-1]);
    zMaxBin = h->GetXaxis()->FindBin(zRegHigh[i]);
    
    cout<<h->Integral(zMinBin,zMaxBin)/tot<<"\t";
  }
  cout<<endl;

}
