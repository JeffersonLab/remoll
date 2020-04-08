#ifndef __BEAMLINEDETHISTO_H
#define __BEAMLINEDETHISTO_H

/* NOT MEANT TO BE USED INDEPENDENTLY

   INPUT: output file and  detector ID
*/
#include "anaConst.h"

std::vector<TH2D*> dBL_xy[nSpecies][nDmg];
std::vector<TH1D*> dBL_energy[nSpecies];
std::map<int, int> dBL_ID2entry;

void initHisto_beamLine(TFile *fout,int detID, string detNm){

  fout->cd();
  fout->mkdir(Form("det%d",detID),detNm.c_str());
  fout->cd(Form("det%d",detID));

  dBL_ID2entry.insert(std::pair<int, int>(detID,dBL_ID2entry.size()));

  for(int i=0;i<nSpecies;i++){
    dBL_energy[i].push_back(new TH1D(Form("d%d_energy_%s",detID,spH[i].c_str()),
				     Form("energy distribution %s",spH[i].c_str()),
				     121,-8,4.1));
    niceLogXBins(dBL_energy[i][dBL_ID2entry[detID]]);

    for(int j=0;j<nDmg;j++){
      dBL_xy[i][j].push_back(new TH2D(Form("d%d_xy_%s_Dmg%d",detID,spH[i].c_str(),j),
				      Form("%s for %s;x[mm];y[mm]",dmgTit[j].c_str(),spTit[i].c_str()),
				      800,-1300,1300,
				      800,-1300,1300));
    }
  }
}

void fillHisto_beamLine(int detID, int sp, double rdDmg[3],
			double xx, double yy, double kinE){
  
  int det = dBL_ID2entry[detID];

  dBL_energy[sp][det]->Fill(kinE);

  for(int kk=0;kk<nDmg;kk++){
    dBL_xy[sp][kk][det]->Fill(xx,yy,rdDmg[kk]);
  }
}

void writeOutput_beamLine(TFile *fout, int detID, double scaleFactor){

  int det = dBL_ID2entry[detID];
  fout->cd();
  fout->cd(Form("det%d",detID));
  for(int i=0;i<nSpecies;i++){
    dBL_energy[i][det]->Scale(scaleFactor);
    dBL_energy[i][det]->Write();

    for(int j=0;j<nDmg;j++){
	dBL_xy[i][j][det]->Scale(scaleFactor);
	dBL_xy[i][j][det]->Write();
      }
  }
}
#endif //__BEAMLINEDETHISTO_H
