#ifndef __DET28HISTO_H
#define __DET28HISTO_H

/* NOT MEANT TO BE USED INDEPENDENTLY

  DET 28: main detector plane; vacuum detector at 26.5m DS of tgt and 22m away from center of hall
  define histograms (must have  unique names
  init and write;

  will need output file from main analysis macro
  each of these histograms will have to filled by the main analysis macro
*/
#include "anaConst.h"

const int nRing=8; //all, and the actual 7 rings
TH1F *d28_energy[nSpecies][nRing][nFB], *d28_energyNIEL[nSpecies][nRing][nFB];

TH1F *d28_z0[nSpecies][nRing][nDmg][nErange];

TH2F *d28_xy[nSpecies][nRing][nDmg];
TH2F *d28_z0r0[nSpecies][nRing][nDmg];
TH2F *d28_z0x0[nSpecies][nRing][nDmg];

//source plots for different z cuts
TH2F *d28_x0y0Zcut[nSpecies][nZcut];
TH2F *d28_MDx0y0Zcut[nSpecies][nZcut];

//bins are different sectors and rings in det 28;
TH1F *d28_mdHits[nSpecies][nErange][nDmg];

const int nSecDet = 21; // 7(ring, including pmts) x 3 (sectors)

void initHisto_det28(TFile *fout){
  fout->cd();
  fout->mkdir("det28","main detector plane");
  fout->cd("det28");

  for(int i=0;i<nSpecies;i++){
    for(int k=0;k<nZcut;k++){
      d28_x0y0Zcut[i][k]=new TH2F(Form("d28_x0y0Zcut_%s_ZC%d",spH[i].c_str(),k),
				  Form("hits per electron for %s %s;x0[mm];y0[mm]",
				       spTit[i].c_str(),zCutTit[k].c_str()),
				  300,-5000,5000,
				  300,-5000,5000);
      d28_MDx0y0Zcut[i][k]=new TH2F(Form("d28_MDx0y0Zcut_%s_ZC%d",spH[i].c_str(),k),
				    Form("hits per electron for %s %s;x0[mm];y0[mm]",
					 spTit[i].c_str(),zCutTit[k].c_str()),
				    300,-5000,5000,
				    300,-5000,5000);
    }

    for(int k=0;k<nErange;k++)
      for(int j=0;j<nDmg;j++){
	d28_mdHits[i][k][j]=new TH1F(Form("d28_mdHits_%s_ER%d_Dmg%d",spH[i].c_str(),k,j),
				     Form("%s per electron for %s with %s",
					  dmgTit[j].c_str(),spTit[i].c_str(),eRgTit[k].c_str()),
				     nSecDet,0,nSecDet);

	const string secNm[3]={"closed","transition","open"};
	for(int kk=1;kk<=nSecDet;kk++){
	  int ring= (kk-1-(kk-1)%3)/3+1;
	  int sector = (kk-1)%3;
	  d28_mdHits[i][k][j]->GetXaxis()->SetBinLabel(kk,Form("R%d %s",ring,secNm[sector].c_str()));
      }	  
    }

    for(int j=0;j<nRing;j++){
      for(int k=0;k<nFB;k++){
	d28_energy[i][j][k]=new TH1F(Form("d28_energy_R%d_%s_%s",j,spH[i].c_str(),fbH[k].c_str()),
				     Form("rate weighted R%d for %s %s;E [MeV]",j,spTit[i].c_str(),fbH[k].c_str()),
				     121,-8,4.1);
	niceLogXBins(d28_energy[i][j][k]);

	d28_energyNIEL[i][j][k]=new TH1F(Form("d28_energyNIEL_R%d_%s_%s",j,spH[i].c_str(),fbH[k].c_str()),
				      Form("rate weighted R%d for %s %s;E [MeV]",j,spTit[i].c_str(),fbH[k].c_str()),
				      121,-8,4.1);
	niceLogXBins(d28_energyNIEL[i][j][k]);
      }


      for(int k=0;k<nDmg;k++){
	for(int e=0;e<nErange;e++){
	  d28_z0[i][j][k][e]=new TH1F(Form("d28_z0_R%d_%s_Dmg%d_Erg%d",j,spH[i].c_str(),k,e),
				      Form("%s weighted %s R%d %s;z0[mm]",dmgTit[k].c_str(),eRgTit[e].c_str(),j,spTit[i].c_str()),
				      3000,-6000,45000);
	}      

	d28_xy[i][j][k]=new TH2F(Form("d28_xy_R%d_%s_Dmg%d",j,spH[i].c_str(),k),
				 Form("%s R%d for %s;x[mm];y[mm]",dmgTit[k].c_str(),j,spTit[i].c_str()),
				 800,-2000,2000,
				 800,-2000,2000);
      
      
	d28_z0r0[i][j][k]=new TH2F(Form("d28_z0r0_R%d_%s_Dmg%d",j,spH[i].c_str(),k),
				   Form("%s R%d for %s;z0[mm];r0[mm]",dmgTit[k].c_str(),j,spTit[i].c_str()),
				   3000,-6000,45000,
				   400,0,6000);

	d28_z0x0[i][j][k]=new TH2F(Form("d28_z0x0_R%d_%s_Dmg%d",j,spH[i].c_str(),k),
				   Form("%s R%d for %s;z0[mm];x0[mm]",dmgTit[k].c_str(),j,spTit[i].c_str()),
				   3000,-6000,45000,
				   400,-6000,6000);
      }
    }
  }
}

void fillHisto_det28(int sp, int ring,double rdDmg[3],
		     double xx, double yy, double vx0,
		     double vy0, double vz0, double rr, 
		     double kinE, double pz, int sector){
  
  double vr0=sqrt(vx0*vx0+vy0*vy0);
  for(int kk=0;kk<nDmg;kk++){
    d28_z0[sp][ring][kk][0]->Fill(vz0,rdDmg[kk]);    
    for(int ll = 1; ll<nErange ; ll++)
      if(kinE<eRanges[ll] && kinE>=eRanges[ll-1])
	d28_z0[sp][ring][kk][ll]->Fill(vz0,rdDmg[kk]);    

    d28_xy[sp][ring][kk]->Fill(xx,yy,rdDmg[kk]);
    d28_z0r0[sp][ring][kk]->Fill(vz0,vr0,rdDmg[kk]);
    d28_z0x0[sp][ring][kk]->Fill(vz0,vx0,rdDmg[kk]);
    
  }

  d28_energy[sp][ring][0]->Fill(kinE,rdDmg[0]);
  d28_energyNIEL[sp][ring][0]->Fill(kinE,rdDmg[2]);
  if(pz>=0){
    d28_energy[sp][ring][1]->Fill(kinE,rdDmg[0]);
    d28_energyNIEL[sp][ring][1]->Fill(kinE,rdDmg[2]);
  }else{
    d28_energy[sp][ring][2]->Fill(kinE,rdDmg[0]);
    d28_energyNIEL[sp][ring][2]->Fill(kinE,rdDmg[2]);
  }


  if(ring==0){
    for(int ii=0;ii<nZcut;ii++)
      if(vz0>zCuts[ii][0] && vz0<zCuts[ii][1]){
	d28_x0y0Zcut[sp][ii]->Fill(vx0,vy0,rdDmg[0]);
	if(rr>500 && rr<1500)
	  d28_MDx0y0Zcut[sp][ii]->Fill(vx0,vy0,rdDmg[0]);
      }
  }else{
    int foundRing=ring-1;
    for(int kk=0;kk<nDmg;kk++){
      d28_mdHits[sp][0][kk]->SetBinContent(foundRing*3+sector+1,
					    rdDmg[kk] + d28_mdHits[sp][0][kk]->GetBinContent(foundRing*3+sector+1));

      for(int ll = 1; ll<nErange ; ll++)
	if(kinE<eRanges[ll] && kinE>=eRanges[ll-1])
	  d28_mdHits[sp][ll][kk]->SetBinContent(foundRing*3+sector+1,
						rdDmg[kk] + d28_mdHits[sp][ll][kk]->GetBinContent(foundRing*3+sector+1));
      
    }
  }
}

void writeOutput_det28(TFile *fout, double scaleFactor){
  fout->cd("det28");
  for(int i=0;i<nSpecies;i++){
    for(int k=0;k<nZcut;k++){
      d28_x0y0Zcut[i][k]->Scale(scaleFactor);
      d28_x0y0Zcut[i][k]->Write();
      d28_MDx0y0Zcut[i][k]->Scale(scaleFactor);
      d28_MDx0y0Zcut[i][k]->Write();
    }

    for(int k=0;k<nErange;k++)
      for(int j=0;j<nDmg;j++){
	d28_mdHits[i][k][j]->Scale(scaleFactor);
	d28_mdHits[i][k][j]->Write();
      }
    
    for(int j=0;j<nRing;j++){
      for(int k=0;k<nFB;k++){
	d28_energy[i][j][k]->Scale(scaleFactor);
	d28_energy[i][j][k]->Write();

	d28_energyNIEL[i][j][k]->Scale(scaleFactor);
	d28_energyNIEL[i][j][k]->Write();
      }
      for(int k=0;k<nDmg;k++){
	for(int e=0;e<nErange;e++){
	  d28_z0[i][j][k][e]->Scale(scaleFactor);
	  d28_z0[i][j][k][e]->Write();
	}

	d28_xy[i][j][k]->Scale(scaleFactor);
	d28_xy[i][j][k]->Write();
      
      
	d28_z0r0[i][j][k]->Scale(scaleFactor);
	d28_z0r0[i][j][k]->Write();

	d28_z0x0[i][j][k]->Scale(scaleFactor);
	d28_z0x0[i][j][k]->Write();
      }
    }
 
  }
}
#endif //__DET28HISTO_H
