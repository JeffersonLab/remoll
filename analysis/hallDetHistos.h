#ifndef __HALLDETHISTO_H
#define __HALLDETHISTO_H

/* NOT MEANT TO BE USED INDEPENDENTLY

   INPUT: output file and  detector ID
*/
#include "anaConst.h"

TH2F *dHL_Roof_zx[nSpecies][nDmg];
TH2F *dHL_Roof_z0x0[nSpecies][nDmg];
TH1F *dHL_Roof_energy[nSpecies][nDmg];

TH2F *dHL_Wall_yPhi[nSpecies][nDmg];
TH2F *dHL_Wall_z0x0[nSpecies][nDmg];
TH1F *dHL_Wall_energy[nSpecies][nDmg];

void initHisto_hall(TFile *fout){

  fout->cd();
  fout->mkdir("hallDet","hallDetectors");
  fout->cd("hallDet");

  for(int k=0;k<nDmg;k++)
    for(int i=0;i<nSpecies;i++){
      dHL_Roof_energy[i][k] = new TH1F(Form("dHL_Roof_energy_%s_Dmg%d",spH[i].c_str(),k),
				       Form("energy distribution %s weighted by %s",spH[i].c_str(),dmgTit[k].c_str()),
				       121,-8,4.1);
      niceLogXBins(dHL_Roof_energy[i][k]);
      
      dHL_Wall_energy[i][k] = new TH1F(Form("dHL_Wall_energy_%s_Dmg%d",spH[i].c_str(),k),
				       Form("energy distribution %s weighted by %s",spH[i].c_str(),dmgTit[k].c_str()),
				       121,-8,4.1);
      niceLogXBins(dHL_Wall_energy[i][k]);
      
      dHL_Roof_zx[i][k] = new TH2F(Form("dHL_Roof_zx_%s_Dmg%d",spH[i].c_str(),k),
				   Form("%s for %s;z[mm];x[mm]",dmgTit[k].c_str(),spTit[i].c_str()),
				   2000,-27000,45000,
				   2000,-27000,27000);

      dHL_Roof_z0x0[i][k] = new TH2F(Form("dHL_Roof_z0x0_%s_Dmg%d",spH[i].c_str(),k),
				     Form("%s for %s;z0[mm];x0[mm]",dmgTit[k].c_str(),spTit[i].c_str()),
				     2000,-27000,45000,
				     2000,-27000,27000);

      dHL_Wall_yPhi[i][k] = new TH2F(Form("dHL_Wall_yPhi_%s_Dmg%d",spH[i].c_str(),k),
				     Form("%s for %s;#phi[mm];y[mm]",dmgTit[k].c_str(),spTit[i].c_str()),
				     2000,-pi,pi,//0 is positive z direction
				     2000,-4000,14000);

      dHL_Wall_z0x0[i][k] = new TH2F(Form("dHL_Wall_z0x0_%s_Dmg%d",spH[i].c_str(),k),
				     Form("%s for %s;z0[mm];x0[mm]",dmgTit[k].c_str(),spTit[i].c_str()),
				     2000,-27000,45000,
				     2000,-27000,27000);
    }
}

void fillHisto_hall(int detID, int sp, double rdDmg[3],
		    double xx, double yy, double zz,
		    double vx, double vy, double vz,
		    double kinE){
  
  if(detID==99){ ///hall wall det
    double phi = atan2(xx,zz);
    for(int i=0;i<nDmg;i++){
      dHL_Wall_energy[sp][i]->Fill(kinE,rdDmg[i]);
      dHL_Wall_yPhi[sp][i]->Fill(phi,yy,rdDmg[i]);
      dHL_Wall_z0x0[sp][i]->Fill(vz,vx,rdDmg[i]);
    }
  }else if(detID==101){ ///flat roof det  
    for(int i=0;i<nDmg;i++){
      dHL_Roof_energy[sp][i]->Fill(kinE,rdDmg[i]);
      dHL_Roof_zx[sp][i]->Fill(zz,xx,rdDmg[i]);
      dHL_Roof_z0x0[sp][i]->Fill(vz,vx,rdDmg[i]);
    }
  }
}

void writeOutput_hall(TFile *fout, double scaleFactor){

  fout->cd();
  fout->cd("hallDet");
  for(int i=0;i<nSpecies;i++)
    for(int j=0;j<nDmg;j++){
      dHL_Roof_energy[i][j]->Scale(scaleFactor);
      dHL_Roof_energy[i][j]->Write();

      dHL_Roof_zx[i][j]->Scale(scaleFactor);
      dHL_Roof_zx[i][j]->Write();

      dHL_Roof_z0x0[i][j]->Scale(scaleFactor);
      dHL_Roof_z0x0[i][j]->Write();


      dHL_Wall_energy[i][j]->Scale(scaleFactor);
      dHL_Wall_energy[i][j]->Write();      
  
      dHL_Wall_yPhi[i][j]->Scale(scaleFactor);
      dHL_Wall_yPhi[i][j]->Write();

      dHL_Wall_z0x0[i][j]->Scale(scaleFactor);
      dHL_Wall_z0x0[i][j]->Write();
    }
}
#endif //__HALLDETHISTO_H
