#ifndef __FLATDEDETHISTO_H
#define __FLATDEDETHISTO_H

/* NOT MEANT TO BE USED INDEPENDENTLY

   INPUT: output file and  detector ID
*/
#include "anaConst.h"

std::vector<TH2F*> dHE_xy[nSpecies][nFB];
std::vector<TH1F*> dHE_vz[nSpecies][nFB];
std::map<int, int> dHE_ID2entry;

void initHisto_flatHE(TFile *fout,int detID, string detNm, int hRange, int vZmin=-30000, int vZmax=45000){

  fout->cd();
  if(!fout->GetDirectory(Form("det%d",detID),detNm.c_str()))
    fout->mkdir(Form("det%d",detID),detNm.c_str());
  fout->cd(Form("det%d",detID));

  dHE_ID2entry.insert(std::pair<int, int>(detID,dHE_ID2entry.size()));
  for(int k=0;k<nFB;k++)
    for(int i=0;i<nSpecies;i++){
      dHE_xy[i][k].push_back(new TH2F(Form("d%dHE_xy_%s_%s",detID,spH[i].c_str(),fbH[k].c_str()),
				      Form("hits for %s %s;x[mm];y[mm]",fbH[k].c_str(),spTit[i].c_str()),
				      800,-hRange,hRange,
				      800,-hRange,hRange));
      
      dHE_vz[i][k].push_back(new TH1F(Form("d%dHE_vz_%s_%s",detID,spH[i].c_str(),fbH[k].c_str()),
				      Form("hits for %s %s;x[mm];y[mm]",fbH[k].c_str(),spTit[i].c_str()),
				      2000,vZmin,vZmax));
    }
}


void fillHisto_flatHE(int detID, int sp, double pz,
			double xx, double yy, double vz){
  
  int det = dHE_ID2entry[detID];

  dHE_vz[sp][0][det]->Fill(vz);
  if(pz<0)
    dHE_vz[sp][2][det]->Fill(vz);
  else
    dHE_vz[sp][1][det]->Fill(vz);

  dHE_xy[sp][0][det]->Fill(xx,yy);
  if(pz<0)
    dHE_xy[sp][2][det]->Fill(xx,yy);
  else
    dHE_xy[sp][1][det]->Fill(xx,yy);
}

void writeOutput_flat(TFile *fout, int detID, double scaleFactor){

  int det = dHE_ID2entry[detID];
  fout->cd();
  fout->cd(Form("det%d",detID));
  for(int k=0;k<nFB;k++)
    for(int i=0;i<nSpecies;i++){
      dHE_vz[i][k][det]->Scale(scaleFactor);
      dHE_vz[i][k][det]->Write();
      
      dHE_xy[i][k][det]->Scale(scaleFactor);
      dHE_xy[i][k][det]->Write();
    }
}
#endif //__FLATDEDETHISTO_H
