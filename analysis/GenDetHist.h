#ifndef __GENDETHIST_H
#define __GENDETHIST_H

/* NOT MEANT TO BE USED INDEPENDENTLY

  General detector
  define histograms (must have  unique names
  init and write;

  will need output file from main analysis macro
  each of these histograms will have to filled by the main analysis macro
*/
#include "anaConst.h"

TH1D *gendet_energy[nSpecies], *gendet_energyNIEL[nSpecies];

TH1D *gendet_z0[nSpecies][nDmg];
TH1D *gendet_z0HE[nSpecies][nDmg];

TH2D *gendet_xy[nSpecies][nDmg];
TH2D *gendet_z0r0[nSpecies][nDmg];
TH2D *gendet_z0x0[nSpecies][nDmg];

//source plots for different z cuts
TH2D *gendet_x0y0Zcut[nSpecies][nZcut];
TH2D *gendet_MDx0y0Zcut[nSpecies][nZcut];


const int nSecDet = 21; // 7(ring, including pmts) x 3 (sectors)

void initHisto_gendet(TFile *fout){
  //fout->mkdir("det28","main detector plane");
  fout->mkdir("gendet","Genearal detector");

  for(int i=0;i<nSpecies;i++){
    for(int k=0;k<nZcut;k++){
      gendet_x0y0Zcut[i][k]=new TH2D(Form("gendet_x0y0Zcut_%s_ZC%d",spH[i].c_str(),k),
				  Form("hits per electron for %s %s;x0[mm];y0[mm]",
				       spTit[i].c_str(),zCutTit[k].c_str()),
				  300,-5000,5000,
				  300,-5000,5000);
      gendet_MDx0y0Zcut[i][k]=new TH2D(Form("gendet_MDx0y0Zcut_%s_ZC%d",spH[i].c_str(),k),
				    Form("hits per electron for %s %s;x0[mm];y0[mm]",
					 spTit[i].c_str(),zCutTit[k].c_str()),
				    300,-5000,5000,
				    300,-5000,5000);
    }


      gendet_energy[i]=new TH1D(Form("gendet_energy_%s",spH[i].c_str()),
				Form("rate weighted for %s;E [MeV]",spTit[i].c_str()),
				121,-8,4.1);
      niceLogXBins(gendet_energy[i]);

      gendet_energyNIEL[i]=new TH1D(Form("gendet_energyNEIL_%s",spH[i].c_str()),
				    Form("rate weighted for %s;E [MeV]",spTit[i].c_str()),
				    121,-8,4.1);
      niceLogXBins(gendet_energyNIEL[i]);


      for(int k=0;k<nDmg;k++){
	gendet_z0[i][k]=new TH1D(Form("gendet_z0_%s_Dmg%d",spH[i].c_str(),k),
				 Form("%s weighted %s;z0[mm]",dmgTit[k].c_str(),spTit[i].c_str()),
				 2000,-6000,32000);
      
	gendet_z0HE[i][k]=new TH1D(Form("gendet_z0HE_%s_Dmg%d",spH[i].c_str(),k),
				   Form("%s weighted %s;z0HE[mm]",dmgTit[k].c_str(),spTit[i].c_str()),
				   2000,-6000,32000);
      

	gendet_xy[i][k]=new TH2D(Form("gendet_xy_%s_Dmg%d",spH[i].c_str(),k),
				 Form("%s for %s;x[mm];y[mm]",dmgTit[k].c_str(),spTit[i].c_str()),
				 800,-2000,2000,
				 800,-2000,2000);
      
      
	gendet_z0r0[i][k]=new TH2D(Form("gendet_z0r0_%s_Dmg%d",spH[i].c_str(),k),
				   Form("%s for %s;z0[mm];r0[mm]",dmgTit[k].c_str(),spTit[i].c_str()),
				   2000,-6000,32000,
				   200,0,3000);

	gendet_z0x0[i][k]=new TH2D(Form("gendet_z0x0_%s_Dmg%d",spH[i].c_str(),k),
				   Form("%s for %s;z0[mm];x0[mm]",dmgTit[k].c_str(),spTit[i].c_str()),
				   2000,-6000,32000,
				   200,-3000,3000);
      }
  }
}

void fillHisto_gendet(int sp, double rdDmg[3],
		     double xx, double yy, double vx0,
		     double vy0, double vz0, double rr, 
		     double kinE, int sector){
  
  double vr0=sqrt(vx0*vx0+vy0*vy0);
  for(int kk=0;kk<nDmg;kk++){
    gendet_z0[sp][kk]->Fill(vz0,rdDmg[kk]);
    if(kinE>10)
      gendet_z0HE[sp][kk]->Fill(vz0,rdDmg[kk]);
    
    gendet_xy[sp][kk]->Fill(xx,yy,rdDmg[kk]);
    gendet_z0r0[sp][kk]->Fill(vz0,vr0,rdDmg[kk]);
    gendet_z0x0[sp][kk]->Fill(vz0,vx0,rdDmg[kk]);
    
  }

  gendet_energy[sp]->Fill(kinE,rdDmg[0]);
  gendet_energyNIEL[sp]->Fill(kinE,rdDmg[2]);

    for(int ii=0;ii<nZcut;ii++)
      if(vz0>zCuts[ii][0] && vz0<zCuts[ii][1]){
	gendet_x0y0Zcut[sp][ii]->Fill(vx0,vy0,rdDmg[0]);
	if(rr>500 && rr<1500)
	  gendet_MDx0y0Zcut[sp][ii]->Fill(vx0,vy0,rdDmg[0]);
      }
}

void writeOutput_gendet(TFile *fout, double scaleFactor){
  fout->cd("gendet");
  for(int i=0;i<nSpecies;i++){
    for(int k=0;k<nZcut;k++){
      gendet_x0y0Zcut[i][k]->Scale(scaleFactor);
      gendet_x0y0Zcut[i][k]->Write();
      gendet_MDx0y0Zcut[i][k]->Scale(scaleFactor);
      gendet_MDx0y0Zcut[i][k]->Write();
    }

    
      gendet_energy[i]->Scale(scaleFactor);
      gendet_energy[i]->Write();

      gendet_energyNIEL[i]->Scale(scaleFactor);
      gendet_energyNIEL[i]->Write();

      for(int k=0;k<nDmg;k++){
	gendet_z0[i][k]->Scale(scaleFactor);
	gendet_z0[i][k]->Write();

	gendet_z0HE[i][k]->Scale(scaleFactor);
	gendet_z0HE[i][k]->Write();
      

	gendet_xy[i][k]->Scale(scaleFactor);
	gendet_xy[i][k]->Write();
      
      
	gendet_z0r0[i][k]->Scale(scaleFactor);
	gendet_z0r0[i][k]->Write();

	gendet_z0x0[i][k]->Scale(scaleFactor);
	gendet_z0x0[i][k]->Write();
      }
  }
}
#endif //__GENDETHIST_H
