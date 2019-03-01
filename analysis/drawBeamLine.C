// Run using (for example):
//   build/reroot -l 'analysis/drawBeamLine.C("output/dump/remoll_beam_dump.root")'

const int nDet=7;
const int detNr[nDet]={533,534,535,536,537,538,539};
TH2D *h2D[nDet][6];
TH1D *h1D[nDet][4];

void initHist();
void writeHist(TString files);
void drawHist(TString files);

void drawBeamLine(const TString& files)
{
  TChain* t = new TChain("T");
  t->Add(files);

  std::vector<remollEventParticle_t>* parts = 0;
  std::vector<remollGenericDetectorHit_t>* hits = 0;

  t->SetBranchAddress("hit", &hits);
  t->SetBranchAddress("part", &parts);

  initHist();

  long nEntries = t->GetEntries();
  float currentProc=1,procStep=10;
  for (long iev = 0; iev < nEntries; iev++) {
    //if( float(i+1)/nEntries*100 > 51) continue;
    t->GetEntry(iev);
    if( float(iev+1)/nEntries*100 > currentProc){
      cout<<"at tree entry\t"<<iev<<"\t"<< float(iev+1)/nEntries*100<<" %"<<endl;
      currentProc+=procStep;
    }

    for (size_t ihit = 0; ihit < hits->size(); ihit++) {
      remollGenericDetectorHit_t hit = hits->at(ihit);
      if(hit.e < 1) continue; //MeV
      int found(-1);
      for(int i=0;i<nDet;i++)
	if(hit.det == detNr[i]){
	  found=i;
	  break;
	}
      if(found == -1) continue;

      if(hit.pz>0){
	h2D[found][0] -> Fill(hit.x,hit.y);
	h2D[found][2] -> Fill(hit.x,hit.y, hit.e);
	h1D[found][0] -> Fill(hit.r);
	h1D[found][2] -> Fill(hit.r, hit.e);
      }else{
	h2D[found][1] -> Fill(hit.x,hit.y);
	h2D[found][3] -> Fill(hit.x,hit.y, hit.e);
	h1D[found][1] -> Fill(hit.r);
	h1D[found][3] -> Fill(hit.r, hit.e);
	h2D[found][4] -> Fill(hit.vz,hit.vy);
	h2D[found][5] -> Fill(hit.vz,hit.vy, hit.e);
      }
    }

  }
  writeHist(files);

}

void initHist(){
  const int hLimit[nDet]={2000, 500, 300, 300, 300, 300, 500};
  const string hTit[nDet]={"hits pz>0 E>1","hits pz<0 E>1","hits*e pz>0 E>1","hits*e pz<0 E>1",
			   "hits source pz<0 E>1","hits*e source pz<0 E>1"};
  for(int i=0;i<nDet;i++)
    for(int j=0;j<6;j++){
      if(j<4){
	h2D[i][j]=new TH2D(Form("h2D_%d",i*nDet+j),
			   Form("det %d %s",detNr[i],hTit[j].c_str()),
			   200,-hLimit[i],hLimit[i],200,-hLimit[i],hLimit[i]);
	h1D[i][j]=new TH1D(Form("h1D_%d",i*nDet+j),
			   Form("det %d %s;r[mm]",detNr[i],hTit[j].c_str()),
			   200,0,hLimit[i]);
      }else{
	h2D[i][j]=new TH2D(Form("h2D_%d",i*nDet+j),
			   Form("det %d %s",detNr[i],hTit[j].c_str()),
			   200,31000,40000,200,-2000,2000);
      }
    }

}

void writeHist(TString files){
  TString fnm=files(0,files.Last('.'))+"_drawBeamline.root";
  TFile *fout=new TFile(fnm,"RECREATE");
  for(int i=0;i<nDet;i++)
    for(int j=0;j<6;j++){
      fout->cd();
      h2D[i][j]->Write();
      if(j<4)
	h1D[i][j]->Write();
    }

  fout->Close();
  delete fout;
  drawHist(fnm);
}

void drawHist(TString files){
  TCanvas *c1D[nDet],*c2D1[nDet], *c2D2[nDet];
  TH1D *h1;TH2D *h2;
  TFile *fin=TFile::Open(files,"READ");

  gStyle->SetOptStat("eMRi");
  for(int i=0;i<nDet;i++){
    c1D[i]=new TCanvas(Form("c1D_%d",i*nDet),Form("%d",detNr[i]),1400,800);
    c1D[i]->Divide(2,2);
    c2D1[i]=new TCanvas(Form("c2D1_%d",i*nDet),Form("%d",detNr[i]),1400,800);
    c2D1[i]->Divide(2,2);
    c2D2[i]=new TCanvas(Form("c2D2_%d",i*nDet),Form("%d",detNr[i]),1400,800);
    c2D2[i]->Divide(1,2);
    fin->cd();
    for(int j=0;j<6;j++)
      if(j<4){
	c2D1[i]->cd(j+1);
	//cout<<Form("h2D_%d",i*nDet+j)<<"\t"<<i<<"\t"<<j<<endl;
	h2=(TH2D*)fin->Get(Form("h2D_%d",i*nDet+j));
	if(h2)
	  h2->DrawCopy("colz");
	gPad->SetLogz(1);
	gPad->SetGridx(1);
	gPad->SetGridy(1);

	c1D[i]->cd(j+1);
	//cout<<Form("h1D_%d",i*nDet+j)<<"\t"<<i<<"\t"<<j<<endl;
	h1=(TH1D*)fin->Get(Form("h1D_%d",i*nDet+j));
	if(h1)
	  h1->DrawCopy("h");
	gPad->SetGridx(1);
	gPad->SetGridy(1);
      }else{
	c2D2[i]->cd(j-3);
	//cout<<Form("h2D_%d",i*nDet+j)<<"\t"<<i<<"\t"<<j<<endl;
	h2=(TH2D*)fin->Get(Form("h2D_%d",i*nDet+j));
	if(h2)
	  h2->DrawCopy("colz");
	gPad->SetLogz(1);
	gPad->SetGridx(1);
	gPad->SetGridy(1);
      }
  }
  fin->Close();
}
