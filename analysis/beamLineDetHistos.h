#ifndef __BEAMLINEDETHISTO_H
#define __BEAMLINEDETHISTO_H

/* NOT MEANT TO BE USED INDEPENDENTLY

   INPUT: output file and  detector ID
*/
#include "anaConst.h"

std::vector<TH2F*> dBL_xy[nSpecies][nDmg][nFB];
std::vector<TH1F*> dBL_r[nSpecies][nDmg][nFB];
std::vector<TH1F*> dBL_energy[nSpecies][nFB];
std::map<int, int> dBL_ID2entry;

void initHisto_beamLine(TFile *fout,int detID, string detNm){

  fout->cd();
  fout->mkdir(Form("det%d",detID),detNm.c_str());
  fout->cd(Form("det%d",detID));

  dBL_ID2entry.insert(std::pair<int, int>(detID,dBL_ID2entry.size()));
  for(int k=0;k<nFB;k++)
    for(int i=0;i<nSpecies;i++){
      dBL_energy[i][k].push_back(new TH1F(Form("d%d_energy_%s_%s",detID,fbH[k].c_str(),spH[i].c_str()),
					  Form("energy distribution %s %s",fbH[k].c_str(),spH[i].c_str()),
					  121,-8,4.1));
      niceLogXBins(dBL_energy[i][k][dBL_ID2entry[detID]]);
      
      for(int j=0;j<nDmg;j++){
	dBL_xy[i][j][k].push_back(new TH2F(Form("d%d_xy_%s_%s_Dmg%d",detID,spH[i].c_str(),fbH[k].c_str(),j),
					   Form("%s for %s %s;x[mm];y[mm]",dmgTit[j].c_str(),fbH[k].c_str(),spTit[i].c_str()),
					   800,-1300,1300,
					   800,-1300,1300));
	dBL_r[i][j][k].push_back(new TH1F(Form("d%d_r_%s_%s_Dmg%d",detID,spH[i].c_str(),fbH[k].c_str(),j),
					  Form("%s for %s %s;x[mm];y[mm]",dmgTit[j].c_str(),fbH[k].c_str(),spTit[i].c_str()),
					  800,0,1300));
      }
    }
}

void fillHisto_beamLine(int detID, int sp, double rdDmg[3],double pz,
			double xx, double yy, double kinE){
  
  int det = dBL_ID2entry[detID];
  double rr = sqrt(xx*xx+yy*yy);

  dBL_energy[sp][0][det]->Fill(kinE);
  if(pz<0)
    dBL_energy[sp][2][det]->Fill(kinE);
  else
    dBL_energy[sp][1][det]->Fill(kinE);

  for(int kk=0;kk<nDmg;kk++){
    dBL_xy[sp][kk][0][det]->Fill(xx,yy,rdDmg[kk]);
    if(pz<0)
      dBL_xy[sp][kk][2][det]->Fill(xx,yy,rdDmg[kk]);
    else
      dBL_xy[sp][kk][1][det]->Fill(xx,yy,rdDmg[kk]);

    dBL_r[sp][kk][0][det]->Fill(rr,rdDmg[kk]);
    if(pz<0)
      dBL_r[sp][kk][2][det]->Fill(rr,rdDmg[kk]);
    else
      dBL_r[sp][kk][1][det]->Fill(rr,rdDmg[kk]);

  }
}

void writeOutput_beamLine(TFile *fout, int detID, double scaleFactor){

  int det = dBL_ID2entry[detID];
  fout->cd();
  fout->cd(Form("det%d",detID));
  for(int k=0;k<nFB;k++)
    for(int i=0;i<nSpecies;i++){
      dBL_energy[i][k][det]->Scale(scaleFactor);
      dBL_energy[i][k][det]->Write();
      
      for(int j=0;j<nDmg;j++){
	dBL_xy[i][j][k][det]->Scale(scaleFactor);
	dBL_xy[i][j][k][det]->Write();
	dBL_r[i][j][k][det]->Scale(scaleFactor);
	dBL_r[i][j][k][det]->Write();
      }
    }
}
#endif //__BEAMLINEDETHISTO_H
