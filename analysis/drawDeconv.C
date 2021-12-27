const int nRings = 7; // 6MD rings plus PMT region
const int nSec=3; //  //0,1,2 == closed, transition, open
double rMin[nRings][nSec],rMax[nRings][nSec];
int setSegmentation(int tileConf);

void drawDeconv(){
  int cls[7]={1,2,4,kMagenta+2,kMagenta-5,kMagenta-9,kOrange+10};
  string procNm[7]={"ee","epE","epI","eAlE","eAlQ","eAlI","pi"};
  const int procDr = 6;
  TFile *fin[procDr];
  for(int j=0;j<procDr;j++)
    fin[j]=TFile::Open(Form("deconv_210428_%s_tileConf15_procDeconvV1.root",procNm[j].c_str()),"READ");

  setSegmentation(3);
  auto *ln = new TLine(1,1,2,2);
  ln->SetLineColor(kMagenta);
  ln->SetLineStyle(2);
  ln->SetLineWidth(2);

  auto *c1=new TCanvas("c1","c1",1800,600);
  c1->Divide(3,2);
  for(int i=0;i<6;i++){
    cout<<i<<endl;
    TH1D *h[procDr];
    string hnm="rRate";
    if(i>2)
      hnm="rRateAsym";
    hnm += Form("_S%d_e1",i%3);
    double min(1e13),max(-1);
    for(int j=0;j<procDr;j++){
      h[j]=(TH1D*)fin[j]->Get(Form("det28/det28_%s",hnm.c_str()));
      h[j]->SetLineColor(cls[j]);
      h[j]->SetLineWidth(2);
      if(h[j]->GetMinimum() < min) min=h[j]->GetMinimum();
      if(h[j]->GetMaximum() > max) max=h[j]->GetMaximum();
    }

    c1->cd(i+1);
    double min2 = 5e3;
    if(i>2)
      min2 = 5e6;
    auto *frm = gPad->DrawFrame(600,min2,1200,max*1.1);
    frm->SetTitle(Form("%s;r [mm]",h[0]->GetTitle()));
    gPad->SetGridx(1);
    gPad->SetGridy(1);
    gPad->SetLogy(1);
    for(int j=0;j<6;j++)
      ln->DrawLine(rMin[j][i%3],min2,rMin[j][i%3],max*1.1);
    ln->DrawLine(rMax[5][i%3],min2,rMax[5][i%3],max*1.1);

    for(int j=0;j<3;j++){
      int clsW[3]={kAzure-2,kSpring, kAzure-9};
      string nmW[3]={"epI1","epI2","epI3"};

      h[j]->SetTitle(procNm[j].c_str());
      h[j]->DrawCopy("hist && same");

      for(int l=0;l<3 && j==2;l++){
	hnm="det28/det28_rRate";
	if(i>2)
	  hnm="det28/det28_rRateAsym";
	hnm += Form("_S%d_W%d",i%3,l);
	TH1D *hw = (TH1D*)fin[j]->Get(hnm.c_str());
	hw->SetTitle(nmW[l].c_str());
	hw->SetLineColor(clsW[l]);
	hw->SetLineWidth(3);
	hw->DrawCopy("hist && same");
      }
    }
  }

  c1->cd(1);
  gPad->BuildLegend();

  for(int j=0;j<procDr;j++)
    fin[j]->Close();
}


int setSegmentation(int tileConf){

  switch(tileConf){
  case 3:{
    // segmented from Sakib 210501
    double rm3[nRings][nSec]={
      { 650.0,  650.0,  650.0},
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 790.0,  790.0,  790.0},
      { 900.0,  900.0,  900.0},
      {1060.0, 1060.0, 1060.0},
      {1200.0, 1200.0, 1200.0}
    };
    double rM3[nRings][nSec]={
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 790.0,  790.0,  790.0},
      { 900.0,  900.0,  900.0},
      {1060.0, 1060.0, 1060.0},
      {1160.0, 1160.0, 1160.0},
      {1500.0, 1500.0, 1500.0}
    };
    cout<<"using tiling configuration from 2021-05-01 for parallel segmented magnets; uniform R5=16cm"<<endl;
    for(int i=0;i<nRings;i++)
      for(int j=0;j<nSec;j++){
	rMin[i][j]=rm3[i][j];
	rMax[i][j]=rM3[i][j];
      }
    return 1;
  }case 6:{
    double rm3[nRings][nSec]={
      { 650.0,  650.0,  650.0},
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 790.0,  790.0,  790.0},
      { 900.0,  900.0,  900.0},
      {1100.0, 1100.0, 1100.0},
      {1200.0, 1200.0, 1200.0}
    };
    double rM3[nRings][nSec]={
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 790.0,  790.0,  790.0},
      { 900.0,  900.0,  900.0},
      {1100.0, 1100.0, 1100.0},
      {1160.0, 1160.0, 1160.0},
      {1500.0, 1500.0, 1500.0}
    };
    cout<<"using tiling configuration from 2021-05-01 for parallel segmented magnets; uniform R5=20cm"<<endl;
    for(int i=0;i<nRings;i++)
      for(int j=0;j<nSec;j++){
	rMin[i][j]=rm3[i][j];
	rMax[i][j]=rM3[i][j];
      }
    return 1;
  }case 5:{
    double rm3[nRings][nSec]={
      { 650.0,  650.0,  650.0},
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 790.0,  790.0,  790.0},
      { 900.0,  900.0,  900.0},
      {1080.0, 1080.0, 1080.0},
      {1200.0, 1200.0, 1200.0}
    };
    double rM3[nRings][nSec]={
      { 690.0,  690.0,  690.0},
      { 735.0,  735.0,  735.0},
      { 790.0,  790.0,  790.0},
      { 900.0,  900.0,  900.0},
      {1080.0, 1080.0, 1080.0},
      {1160.0, 1160.0, 1160.0},
      {1500.0, 1500.0, 1500.0}
    };
    cout<<"using tiling configuration from 2021-05-01 for parallel segmented magnets; uniform R5=18cm"<<endl;
    for(int i=0;i<nRings;i++)
      for(int j=0;j<nSec;j++){
	rMin[i][j]=rm3[i][j];
	rMax[i][j]=rM3[i][j];
      }
    return 1;
  }case 2:{
     // // segmented from Sakib 200131
     double rm2[nRings][nSec]={
       { 640.0,  640.0,  640.0},
       { 680.0,  680.0,  680.0},
       { 750.0,  750.0,  750.0},
       { 855.0,  847.5,  825.0},
       { 935.0,  920.0,  875.0},
       {1075.0, 1080.0, 1090.0},
       {1200.0, 1200.0, 1200.0}
     };
     double rM2[nRings][nSec]={
       { 680.0,  680.0,  680.0},
       { 750.0,  750.0,  750.0},
       { 855.0,  847.5,  825.0},
       { 935.0,  920.0,  875.0},
       {1075.0, 1060.0, 1055.0},
       {1190.0, 1190.0, 1190.0},
       {1500.0, 1500.0, 1500.0}
     };
     cout<<"using tiling configuration from 2020-01-31 segmented magnets"<<endl;
     for(int i=0;i<nRings;i++)
       for(int j=0;j<nSec;j++){
	 rMin[i][j]=rm2[i][j];
	 rMax[i][j]=rM2[i][j];
       }
     return 1;
   }case 4:{
     double rm2[nRings][nSec]={
       { 640.0,  640.0,  640.0},
       { 680.0,  680.0,  680.0},
       { 750.0,  750.0,  750.0},
       { 855.0,  847.5,  825.0},
       { 935.0,  920.0,  875.0},
       {1075.0, 1080.0, 1090.0},
       {1200.0, 1200.0, 1200.0}
     };
     double rM2[nRings][nSec]={
       { 680.0,  680.0,  680.0},
       { 750.0,  750.0,  750.0},
       { 855.0,  847.5,  825.0},
       { 935.0,  920.0,  875.0},
       {1075.0, 1080.0, 1090.0},
       {1190.0, 1190.0, 1190.0},
       {1500.0, 1500.0, 1500.0}
     };
     cout<<"using tiling configuration from 2020-01-31 segmented magnets; with no gaps!"<<endl;
     for(int i=0;i<nRings;i++)
       for(int j=0;j<nSec;j++){
	 rMin[i][j]=rm2[i][j];
	 rMax[i][j]=rM2[i][j];
       }
     return 1;
   }case 1:{
      // hybrid rDef 200202
      double rm1[nRings][nSec]={
	{ 640.0,  640.0,  640.0},
	{ 680.0,  680.0,  680.0},
	{ 730.0,  730.0,  730.0},
	{ 805.0,  827.5,  835.0},
	{ 855.0,  900.0,  915.0},
	{1070.0, 1060.0, 1055.0},
	{1200.0, 1200.0, 1200.0}
      };
      double rM1[nRings][nSec]={
	{ 680.0,  680.0,  680.0},
	{ 730.0,  730.0,  730.0},
	{ 805.0,  827.5,  835.0},
	{ 855.0,  900.0,  915.0},
	{1070.0, 1060.0, 1055.0},
	{1170.0, 1170.0, 1170.0},
	{1500.0, 1500.0, 1500.0}
      };
      cout<<"using tiling configuration from 2020-02-02 for hybrid magnets"<<endl;
      for(int i=0;i<nRings;i++)
	for(int j=0;j<nSec;j++){
	  rMin[i][j]=rm1[i][j];
	  rMax[i][j]=rM1[i][j];
	}
      return 1;
    } default:{
	cout<<"tiling setting unknown: quitting!"<<endl;
	return 0;
      }

  }
}
