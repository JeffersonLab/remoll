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

TH1F *gendet_energy[nSpecies], *gendet_energyNIEL[nSpecies];

TH1F *gendet_z0[nSpecies][nDmg];
TH1F *gendet_z0HE[nSpecies][nDmg];
TH1F *gendet_front, *gendet_back, *gendet_close, *gendet_far, *gendet_top;

TH2F *gendet_front_xy, *gendet_back_xy, *gendet_close_xy, *gendet_far_xy, *gendet_top_xy;//hit xy
TH2F *gendet_front_zx, *gendet_back_zx, *gendet_close_zx, *gendet_far_zx, *gendet_top_zx;//hit zx
TH2F *gendet_front_vzx, *gendet_back_vzx, *gendet_close_vzx, *gendet_far_vzx, *gendet_top_vzx;//vertex zx
TH2F *gendet_xy[nSpecies][nDmg];
TH2F *gendet_z0r0[nSpecies][nDmg];
TH2F *gendet_z0x0[nSpecies][nDmg];

//source plots for different z cuts
TH2F *gendet_x0y0Zcut[nSpecies][nZcut];


void initHisto_gendet(int detId, TFile *fout){
  //fout->mkdir("det28","main detector plane");
  fout->mkdir(Form("det%d",detId),Form("Genearal detector %d",detId));

  for(int i=0;i<nSpecies;i++){
    for(int k=0;k<nZcut;k++){
      gendet_x0y0Zcut[i][k]=new TH2F(Form("det%d_x0y0Zcut_%s_ZC%d",detId,spH[i].c_str(),k),
				  Form("hits per electron for %s %s;x0[mm];y0[mm]",
				       spTit[i].c_str(),zCutTit[k].c_str()),
				  450,-9000,9000,
				  450,-9000,9000);
    }


      gendet_energy[i]=new TH1F(Form("det%d_energy_%s",detId,spH[i].c_str()),
				Form("rate weighted for %s;E [MeV]",spTit[i].c_str()),
				121,-8,4.1);
      niceLogXBins(gendet_energy[i]);

      gendet_energyNIEL[i]=new TH1F(Form("det%d_energyNEIL_%s",detId,spH[i].c_str()),
				    Form("rate weighted for %s;E [MeV]",spTit[i].c_str()),
				    121,-8,4.1);
      niceLogXBins(gendet_energyNIEL[i]);


      for(int k=0;k<nDmg;k++){
	gendet_z0[i][k]=new TH1F(Form("det%d_z0_%s_Dmg%d",detId,spH[i].c_str(),k),
				 Form("%s weighted %s;z0[mm]",dmgTit[k].c_str(),spTit[i].c_str()),
				 2550,-6000,45000);
      
	gendet_z0HE[i][k]=new TH1F(Form("det%d_z0HE_%s_Dmg%d",detId,spH[i].c_str(),k),
				   Form("%s weighted %s;z0HE[mm]",dmgTit[k].c_str(),spTit[i].c_str()),
				   2550,-6000,45000);
      

	gendet_xy[i][k]=new TH2F(Form("det%d_xy_%s_Dmg%d",detId,spH[i].c_str(),k),
				 Form("%s for %s;x[mm];y[mm]",dmgTit[k].c_str(),spTit[i].c_str()),
				 450,-9000,9000,
				 450,-9000,9000);
      
      
	gendet_z0r0[i][k]=new TH2F(Form("det%d_z0r0_%s_Dmg%d",detId,spH[i].c_str(),k),
				   Form("%s for %s;z0[mm];r0[mm]",dmgTit[k].c_str(),spTit[i].c_str()),
				   2550,-6000,45000,
				   225,0,9000);

	gendet_z0x0[i][k]=new TH2F(Form("det%d_z0x0_%s_Dmg%d",detId,spH[i].c_str(),k),
				   Form("%s for %s;z0[mm];x0[mm]",dmgTit[k].c_str(),spTit[i].c_str()),
				   2550,-6000,45000,
				   450,-9000,9000);
      }
  }

  gendet_front = new TH1F("det_front","En on the front side of outer det",121,-8,4.1);
  niceLogXBins(gendet_front);


  gendet_back = new TH1F("det_back","En on the back side of outer det",121,-8,4.1);
  niceLogXBins(gendet_back);

  gendet_close = new TH1F("det_close","En on the close (to beam line) side of outer det",121,-8,4.1);
  niceLogXBins(gendet_close);

  gendet_far = new TH1F("det_far","En on the far(from beam) side of outer det",121,-8,4.1);
  niceLogXBins(gendet_far);

  gendet_top = new TH1F("det_top","En on the top side of outer det",121,-8,4.1);
  niceLogXBins(gendet_top);

  gendet_front_xy = new TH2F("det_front_xy","hit xy dist. on front face",450,-9000,9000,450,-3500,1000);
  gendet_back_xy = new TH2F("det_back_xy","hit xy dist. on back face",450,-9000,9000,450,-3500,1000);
  gendet_close_xy = new TH2F("det_close_xy","hit xy dist. on close face",450,-9000,9000,450,-3500,1000);
  gendet_far_xy = new TH2F("det_far_xy","hit xy dist. on far face",450,-9000,9000,450,-3500,1000);
  gendet_top_xy = new TH2F("det_top_xy","hit xy dist. on top face",450,-9000,9000,50,500,550);

  gendet_front_zx = new TH2F("det_front_zx","hit zx dist. on front face",400,18000,26000,450,-9000,9000);
  gendet_back_zx = new TH2F("det_back_zx","hit zx dist. on back face",400,18000,26000,450,-9000,9000);
  gendet_close_zx = new TH2F("det_close_zx","hit zx dist. on close face",400,18000,26000,450,-9000,9000);
  gendet_far_zx = new TH2F("det_far_zx","hit zx dist. on far face",400,18000,26000,450,-9000,9000);
  gendet_top_zx = new TH2F("det_top_zx","hit zx dist. on top face",400,18000,26000,450,-9000,9000);

  gendet_front_vzx = new TH2F("det_front_vzx","vertex zx dist. on front face",2550,-6000,45000,450,-9000,9000);
  gendet_back_vzx = new TH2F("det_back_vzx","vertex zx dist. on back face",2550,-6000,45000,450,-9000,9000);
  gendet_close_vzx = new TH2F("det_close_vzx","vertex zx dist. on close face",2550,-6000,45000,450,-9000,9000);
  gendet_far_vzx = new TH2F("det_far_vzx","vertex zx dist. on far face",2550,-6000,45000,450,-9000,9000);
  gendet_top_vzx = new TH2F("det_top_vzx","vertex zx dist. on top face",2550,-6000,45000,450,-9000,9000);
}

void fillHisto_gendet(int sp, double rdDmg[3],
		     double xx, double yy, double zz, double vx0,
		     double vy0, double vz0, double rr, 
		     double kinE, int sector, int det){
  
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
      }
  if(sp==3)
  {
     if(det==6667)
	{
	  if(xx>(5000-3040.559) && xx<(5000+70.710) && zz>(22000-3040.559) && zz<(22000+70.710) && yy<=529)//front det
            {
		gendet_front->Fill(kinE,rdDmg[0]);
		gendet_front_xy->Fill(xx,yy,rdDmg[0]);
		gendet_front_zx->Fill(zz,xx,rdDmg[0]);
		gendet_front_vzx->Fill(vz0,vx0,rdDmg[0]);
            }

	  if(xx>(5000-70.710) && xx<(5000+3040.559) && zz>(22000-70.710) && zz<(22000+3040.559) && yy<=529)//back det
            {
		gendet_back->Fill(kinE,rdDmg[0]);
		gendet_back_xy->Fill(xx,yy,rdDmg[0]);
		gendet_back_zx->Fill(zz,xx,rdDmg[0]);
		gendet_back_vzx->Fill(vz0,vx0,rdDmg[0]);
            }

	  if(xx>(5000-3040.559) && xx<(5000-70.710) && zz>(22000+70.710) && zz<(22000+3040.559) && yy<=529)//close to beamline det
            {
		gendet_close->Fill(kinE,rdDmg[0]);
		gendet_close_xy->Fill(xx,yy,rdDmg[0]);
		gendet_close_zx->Fill(zz,xx,rdDmg[0]);
		gendet_close_vzx->Fill(vz0,vx0,rdDmg[0]);
            }

	  if(xx>(5000+70.710) && xx<(5000+3040.559) && zz>(22000-3040.559) && zz<(22000-70.710) && yy<=529)//away from beamline det
            {
		gendet_far->Fill(kinE,rdDmg[0]);
		gendet_far_xy->Fill(xx,yy,rdDmg[0]);
		gendet_far_zx->Fill(zz,xx,rdDmg[0]);
		gendet_far_vzx->Fill(vz0,vx0,rdDmg[0]);
            }

	  if(yy>529)//top det
            {
		gendet_top->Fill(kinE,rdDmg[0]);
		gendet_top_xy->Fill(xx,yy,rdDmg[0]);
		gendet_top_zx->Fill(zz,xx,rdDmg[0]);
		gendet_top_vzx->Fill(vz0,vx0,rdDmg[0]);
            }

        }

     if(det==6669)
	{
	  if(xx<(-5000+3040.559) && xx>(-5000-70.710) && zz>(22000-3040.559) && zz<(22000+70.710) && yy<=529)//front det
            {
		gendet_front->Fill(kinE,rdDmg[0]);
		gendet_front_xy->Fill(xx,yy,rdDmg[0]);
		gendet_front_zx->Fill(zz,xx,rdDmg[0]);
		gendet_front_vzx->Fill(vz0,vx0,rdDmg[0]);
            }

	  if(xx<(-5000+70.710) && xx>(-5000-3040.559) && zz>(22000-70.710) && zz<(22000+3040.559) && yy<=529)//back det
            {
		gendet_back->Fill(kinE,rdDmg[0]);
		gendet_back_xy->Fill(xx,yy,rdDmg[0]);
		gendet_back_zx->Fill(zz,xx,rdDmg[0]);
		gendet_back_vzx->Fill(vz0,vx0,rdDmg[0]);
            }

	  if(xx<(-5000+3040.559) && xx>(-5000+70.710) && zz>(22000+70.710) && zz<(22000+3040.559) && yy<=529)//close to beamline det
            {
		gendet_close->Fill(kinE,rdDmg[0]);
		gendet_close_xy->Fill(xx,yy,rdDmg[0]);
		gendet_close_zx->Fill(zz,xx,rdDmg[0]);
		gendet_close_vzx->Fill(vz0,vx0,rdDmg[0]);
            }

	  if(xx<(-5000-70.710) && xx>(-5000-3040.559) && zz>(22000-3040.559) && zz<(22000-70.710) && yy<=529)//away from beamline det
            {
		gendet_far->Fill(kinE,rdDmg[0]);
		gendet_far_xy->Fill(xx,yy,rdDmg[0]);
		gendet_far_zx->Fill(zz,xx,rdDmg[0]);
		gendet_far_vzx->Fill(vz0,vx0,rdDmg[0]);
            }

	  if(yy>529)//top det
            {
		gendet_top->Fill(kinE,rdDmg[0]);
		gendet_top_xy->Fill(xx,yy,rdDmg[0]);
		gendet_top_zx->Fill(zz,xx,rdDmg[0]);
		gendet_top_vzx->Fill(vz0,vx0,rdDmg[0]);
            }

        }
  }

}

void writeOutput_gendet(int detId, TFile *fout, double scaleFactor){
  fout->cd(Form("det%d",detId));
  for(int i=0;i<nSpecies;i++){
    for(int k=0;k<nZcut;k++){
      gendet_x0y0Zcut[i][k]->Scale(scaleFactor);
      gendet_x0y0Zcut[i][k]->Write();
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
  gendet_front->Scale(scaleFactor);
  gendet_front->Write();

  gendet_front_xy->Scale(scaleFactor);
  gendet_front_xy->Write();

  gendet_front_zx->Scale(scaleFactor);
  gendet_front_zx->Write();

  gendet_front_vzx->Scale(scaleFactor);
  gendet_front_vzx->Write();

  gendet_back->Scale(scaleFactor);
  gendet_back->Write();

  gendet_back_xy->Scale(scaleFactor);
  gendet_back_xy->Write();

  gendet_back_zx->Scale(scaleFactor);
  gendet_back_zx->Write();

  gendet_back_vzx->Scale(scaleFactor);
  gendet_back_vzx->Write();

  gendet_close->Scale(scaleFactor);
  gendet_close->Write();

  gendet_close_xy->Scale(scaleFactor);
  gendet_close_xy->Write();

  gendet_close_zx->Scale(scaleFactor);
  gendet_close_zx->Write();

  gendet_close_vzx->Scale(scaleFactor);
  gendet_close_vzx->Write();

  gendet_far->Scale(scaleFactor);
  gendet_far->Write();

  gendet_far_xy->Scale(scaleFactor);
  gendet_far_xy->Write();

  gendet_far_zx->Scale(scaleFactor);
  gendet_far_zx->Write();

  gendet_far_vzx->Scale(scaleFactor);
  gendet_far_vzx->Write();

  gendet_top->Scale(scaleFactor);
  gendet_top->Write();

  gendet_top_xy->Scale(scaleFactor);
  gendet_top_xy->Write();

  gendet_top_zx->Scale(scaleFactor);
  gendet_top_zx->Write();

  gendet_top_vzx->Scale(scaleFactor);
  gendet_top_vzx->Write();

}
#endif //__GENDETHIST_H
