#ifndef __SPHEREDETHISTO_H
#define __SPHEREDETHISTO_H

/* NOT MEANT TO BE USED INDEPENDENTLY

   INPUT: output file and  detector ID
*/
#include "anaConst.h"

//theta, phi
std::vector<TH2F*> dSph_tp[nSpecies][nDmg][nFB];
//energy of upstream half
std::vector<TH1F*> dSph_energyUS[nSpecies][nFB];
//energy of downstream half
std::vector<TH1F*> dSph_energyDS[nSpecies][nFB];

std::map<int, int> dSph_ID2entry;

void initHisto_sphere(TFile *fout,int detID, string detNm){

  fout->cd();
  fout->mkdir(Form("det%d",detID),detNm.c_str());
  fout->cd(Form("det%d",detID));

  dSph_ID2entry.insert(std::pair<int, int>(detID,dSph_ID2entry.size()));
  for(int k=0;k<nFB;k++)
    for(int i=0;i<nSpecies;i++){
      dSph_energyUS[i][k].push_back(new TH1F(Form("d%d_energyUS_%s_%s",detID,fbH[k].c_str(),spH[i].c_str()),
					  Form("energy distribution first half %s %s",fbH[k].c_str(),spH[i].c_str()),
					  121,-8,4.1));
      niceLogXBins(dSph_energyUS[i][k][dSph_ID2entry[detID]]);

      dSph_energyDS[i][k].push_back(new TH1F(Form("d%d_energyDS_%s_%s",detID,fbH[k].c_str(),spH[i].c_str()),
					  Form("energy distribution second half %s %s",fbH[k].c_str(),spH[i].c_str()),
					  121,-8,4.1));
      niceLogXBins(dSph_energyDS[i][k][dSph_ID2entry[detID]]);
      
      for(int j=0;j<nDmg;j++){
	dSph_tp[i][j][k].push_back(new TH2F(Form("d%d_tp_%s_%s_Dmg%d",detID,spH[i].c_str(),fbH[k].c_str(),j),
					    Form("%s for %s %s (hit angles);theta[deg];phi[deg]",
						 dmgTit[j].c_str(),fbH[k].c_str(),spTit[i].c_str()),
					   800,0,180,
					   800,-180,180));
      }
    }
}

/// pz>0 for things going out of the sphere 
void fillHisto_sphere(int detID, int sp, double rdDmg[3],double pz,
		      double xx, double yy,double zz, double kinE){
  
  int det = dSph_ID2entry[detID];

  if(zz<0){
    dSph_energyUS[sp][0][det]->Fill(kinE);
    if(pz<0)
      dSph_energyUS[sp][2][det]->Fill(kinE);
    else
      dSph_energyUS[sp][1][det]->Fill(kinE);
  }else{
    dSph_energyDS[sp][0][det]->Fill(kinE);
    if(pz<0)
      dSph_energyDS[sp][2][det]->Fill(kinE);
    else
      dSph_energyDS[sp][1][det]->Fill(kinE);
  }

  double th = acos(zz/sqrt(xx*xx + yy*yy + zz*zz));
  double ph = atan2(yy,xx);
  ph = fmod(ph,180);

  for(int kk=0;kk<nDmg;kk++){
    dSph_tp[sp][kk][0][det]->Fill(th,ph,rdDmg[kk]);
    if(pz<0)
      dSph_tp[sp][kk][2][det]->Fill(th,ph,rdDmg[kk]);
    else
      dSph_tp[sp][kk][1][det]->Fill(th,ph,rdDmg[kk]);
  }
}

void writeOutput_sphere(TFile *fout, int detID, double scaleFactor){

  int det = dSph_ID2entry[detID];
  fout->cd();
  fout->cd(Form("det%d",detID));
  for(int k=0;k<nFB;k++)
    for(int i=0;i<nSpecies;i++){
      dSph_energyUS[i][k][det]->Scale(scaleFactor);
      dSph_energyUS[i][k][det]->Write();
      dSph_energyDS[i][k][det]->Scale(scaleFactor);
      dSph_energyDS[i][k][det]->Write();
      
      for(int j=0;j<nDmg;j++){
	dSph_tp[i][j][k][det]->Scale(scaleFactor);
	dSph_tp[i][j][k][det]->Write();
      }
    }
}
#endif //__SPHEREDETHISTO_H
