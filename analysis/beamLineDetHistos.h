#ifndef __BEAMLINEDETHISTO_H
#define __BEAMLINEDETHISTO_H

/* NOT MEANT TO BE USED INDEPENDENTLY

   INPUT: output file and  detector ID
*/
#include "anaConst.h"
#include "histogramUtilities.h"
#include <math.h>
class beamLineDetHistos {
 private:
  int histos; //sets which histograms are recorded

  //histos & 1 == 1
  std::vector<TH1F*> r[nSpecies][nDmg][nFB];
  std::vector<TH1F*> energy[nSpecies][nFB];
  std::map<int, int> ID2entry;
    
  //histos & 2 == 2
  std::vector<TH2F*> xy[nSpecies][nDmg][nFB];
  std::vector<TH2F*> vxz[nSpecies][nDmg][nFB];
  std::vector<TH2F*> vyz[nSpecies][nDmg][nFB];
  std::vector<TH2F*> vxy[nSpecies][nDmg][nFB];
  std::vector<TH2F*> rE[nSpecies][nDmg][nFB];
  std::vector<TH2F*> vxy_region1[nSpecies][nDmg][nFB];
  std::vector<TH2F*> vxy_region2[nSpecies][nDmg][nFB];
  std::vector<TH2F*> vxy_region3[nSpecies][nDmg][nFB];
  std::vector<TH2F*> vxy_region4[nSpecies][nDmg][nFB];
  std::vector<TH1F*> scatAng[nSpecies][nDmg][nFB];
  std::vector<TH1F*> scatAng_region1[nSpecies][nDmg][nFB];
  std::vector<TH1F*> scatAng_region2[nSpecies][nDmg][nFB];

 public:
  beamLineDetHistos(const int histCode=1) { histos=histCode;}
  void SetAnaDet(const int histCode=1) { histos=histCode;}
  ~beamLineDetHistos() {}
  void initHisto(TFile*,const int,const string,const string,const float,const int);
  void initHisto(TFile*,const int,const string,
		 const float, const float,
		 const float, const float,
		 const float, const float,
		 const string,const float, const int);
  void fillHisto(const int detID, const int sp, const double rdDmg[3], const double pz,
		 const float xx, const float yy, const float zz, const float kinE, const float rr,
		 const float vx, const float vy, const float vz, 
		 const int subDet);
  void writeOutput(TFile *fout, int detID, double scaleFactor,const int subDet);
};

void beamLineDetHistos::fillHisto(const int detID, const int sp, const double rdDmg[3], const double pz,
				  const float xx, const float yy, const float zz, const float kinE, const float rr,
				  const float vx, const float vy, const float vz,
				  const int subDet=0) {
  if (ID2entry.find(detID+10000*subDet) == ID2entry.end())
    return;

  int det = ID2entry[detID+10000*subDet];
    
  double vr = sqrt(pow(vx,2)+pow(vy,2));
    
  double ang = acos(zz/sqrt(xx*xx + yy*yy + zz*zz)) * 180/pi;
    
  if( (histos & 1) == 1){
    energy[sp][0][det]->Fill(kinE);
    if(pz<0)
      energy[sp][2][det]->Fill(kinE);
    else
      energy[sp][1][det]->Fill(kinE);
  }

  for(int kk=0;kk<nDmg;kk++){
    if( (histos & 1) == 1){
      xy[sp][kk][0][det]->Fill(xx,yy,rdDmg[kk]);
      if(pz<0)
	xy[sp][kk][2][det]->Fill(xx,yy,rdDmg[kk]);
      else
	xy[sp][kk][1][det]->Fill(xx,yy,rdDmg[kk]);
      
      r[sp][kk][0][det]->Fill(rr,rdDmg[kk]);
      if(pz<0)
	r[sp][kk][2][det]->Fill(rr,rdDmg[kk]);
      else
	r[sp][kk][1][det]->Fill(rr,rdDmg[kk]);
    
      if (detID == 5719 || detID == 5720) {

	scatAng[sp][kk][0][det]->Fill(ang,rdDmg[kk]);
	if(pz<0)
	  scatAng[sp][kk][2][det]->Fill(ang,rdDmg[kk]);
	else
	  scatAng[sp][kk][1][det]->Fill(ang,rdDmg[kk]);
        
        
        if (rr > 1120 && rr < 1160){
	  scatAng_region1[sp][kk][0][det]->Fill(ang,rdDmg[kk]);
	  if(pz<0)
	    scatAng_region1[sp][kk][2][det]->Fill(ang,rdDmg[kk]);
	  else
	    scatAng_region1[sp][kk][1][det]->Fill(ang,rdDmg[kk]);
               
        }
        
        if (rr > 1145 && rr < 1250){
	  scatAng_region2[sp][kk][0][det]->Fill(ang,rdDmg[kk]);
	  if(pz<0)
	    scatAng_region2[sp][kk][2][det]->Fill(ang,rdDmg[kk]);
	  else
	    scatAng_region2[sp][kk][1][det]->Fill(ang,rdDmg[kk]);
               
        }
      }
          
    }
    if( (histos & 2) == 2){

      vxz[sp][kk][0][det]->Fill(vz,vx,rdDmg[kk]);
      if(pz<0)
	vxz[sp][kk][2][det]->Fill(vz,vx,rdDmg[kk]);
      else
	vxz[sp][kk][1][det]->Fill(vz,vx,rdDmg[kk]);

      vyz[sp][kk][0][det]->Fill(vz,vy,rdDmg[kk]);
      if(pz<0)
	vyz[sp][kk][2][det]->Fill(vz,vy,rdDmg[kk]);
      else
	vyz[sp][kk][1][det]->Fill(vz,vy,rdDmg[kk]);
        
      if (detID == 5719 || detID == 5720) {
	
	rE[sp][kk][0][det]->Fill(kinE,rr,rdDmg[kk]);
	if(pz<0)
	  rE[sp][kk][2][det]->Fill(kinE,rr,rdDmg[kk]);
	else
	  rE[sp][kk][1][det]->Fill(kinE,rr,rdDmg[kk]);
    
	vxy[sp][kk][0][det]->Fill(vx,vy,rdDmg[kk]);
	if(pz<0)
	  vxy[sp][kk][2][det]->Fill(vx,vy,rdDmg[kk]);
	else
	  vxy[sp][kk][1][det]->Fill(vx,vy,rdDmg[kk]);
        
	if(vz > 4500 && vz < 19335) {
	  vxy_region1[sp][kk][0][det]->Fill(vx,vy,rdDmg[kk]);
	  if(pz<0)
	    vxy_region1[sp][kk][2][det]->Fill(vx,vy,rdDmg[kk]);
	  else
	    vxy_region1[sp][kk][1][det]->Fill(vx,vy,rdDmg[kk]);
	}

	if(vz > -5200 && vz < -3800) {
	  vxy_region2[sp][kk][0][det]->Fill(vx,vy,rdDmg[kk]);
	  if(pz<0)
	    vxy_region2[sp][kk][2][det]->Fill(vx,vy,rdDmg[kk]);
	  else
	    vxy_region2[sp][kk][1][det]->Fill(vx,vy,rdDmg[kk]);
	}

	if(vz > 150 && vz < 918) {
	  vxy_region3[sp][kk][0][det]->Fill(vx,vy,rdDmg[kk]);
	  if(pz<0)
	    vxy_region3[sp][kk][2][det]->Fill(vx,vy,rdDmg[kk]);
	  else
	    vxy_region3[sp][kk][1][det]->Fill(vx,vy,rdDmg[kk]);
	}
            
	if(vz > 3600 && vz < 4200) {
	  vxy_region4[sp][kk][0][det]->Fill(vx,vy,rdDmg[kk]);
	  if(pz<0)
	    vxy_region4[sp][kk][2][det]->Fill(vx,vy,rdDmg[kk]);
	  else
	    vxy_region4[sp][kk][1][det]->Fill(vx,vy,rdDmg[kk]);
	}
      }
    }
  }
}

void beamLineDetHistos::initHisto(TFile *fout, const int detID, const string detNm, 
				  const float vxRgMin , const float vxRgMax,
				  const float vyRgMin , const float vyRgMax, 
				  const float vzRgMin , const float vzRgMax,
				  const string postfix = "", const float range=2000, const int subDet=0 ){

  fout->cd();
  if(!fout->GetDirectory(Form("det%d",detID)))
    fout->mkdir(Form("det%d",detID), detNm.c_str());
  fout->cd(Form("det%d",detID));
  ID2entry.insert(std::pair<int, int>(detID+10000*subDet,ID2entry.size()));
  for(int k=0;k<nFB;k++)
    for(int i=0;i<nSpecies;i++){
      if( (histos & 1) == 1){
	energy[i][k].push_back(new TH1F(Form("d%d_%s_energy_%s_%s", detID, postfix.c_str(), fbH[k].c_str(), spH[i].c_str()),
					Form("energy distribution %s %s", fbH[k].c_str(), spH[i].c_str()),
					141, -8, 6.1));
	niceLogXBins(energy[i][k][ID2entry[detID+10000*subDet]]);
      }	
      for(int j=0;j<nDmg;j++){
	if( (histos & 1) == 1){
          xy[i][j][k].push_back(new TH2F(Form("d%d_%s_xy_%s_%s_Dmg%d",detID, postfix.c_str(), spH[i].c_str(),fbH[k].c_str(),j),
					 Form("%s for %s %s;x[mm];y[mm]",dmgTit[j].c_str(),fbH[k].c_str(),spTit[i].c_str()),
					 1000, -range, range,
					 1000, -range, range));
        
	  r[i][j][k].push_back(new TH1F(Form("d%d_%s_r_%s_%s_Dmg%d",detID, postfix.c_str(), spH[i].c_str(),fbH[k].c_str(),j),
					Form("%s for %s %s;r[mm];",dmgTit[j].c_str(),fbH[k].c_str(),spTit[i].c_str()),
					1000, 0, range));
        
	  if (detID == 5719 || detID == 5720){
            scatAng[i][j][k].push_back(new TH1F(Form("d%d_%s_ang_%s_%s_Dmg%d",detID, postfix.c_str(), spH[i].c_str(),fbH[k].c_str(),j),
						Form("%s for %s %s;ang[#theta];",dmgTit[j].c_str(),fbH[k].c_str(),spTit[i].c_str()),
						1000, 0, 60));
            scatAng_region1[i][j][k].push_back(new TH1F(Form("d%d_%s_ang_r1_%s_%s_Dmg%d",detID, postfix.c_str(), spH[i].c_str(),fbH[k].c_str(),j),
							Form("%s for %s %s, R1;ang[#theta];",dmgTit[j].c_str(),fbH[k].c_str(),spTit[i].c_str()),
							1000, 0, 10));
            scatAng_region2[i][j][k].push_back(new TH1F(Form("d%d_%s_ang_r2_%s_%s_Dmg%d",detID, postfix.c_str(), spH[i].c_str(),fbH[k].c_str(),j),
							Form("%s for %s %s, R2;ang[#theta];",dmgTit[j].c_str(),fbH[k].c_str(),spTit[i].c_str()),
							1000, 0, 10));
	  }
     
	}
	if( (histos & 2) == 2 ){ //source histograms
	  vxz[i][j][k].push_back(new TH2F(Form("d%d_%s_vxz_%s_%s_Dmg%d",detID, postfix.c_str(), spH[i].c_str(),fbH[k].c_str(),j),
					  Form("%s for %s %s;vz[mm];vx[mm]",dmgTit[j].c_str(),fbH[k].c_str(),spTit[i].c_str()),
					  1000, vzRgMin, vzRgMax,
					  1000, vxRgMin, vxRgMax));
	  vyz[i][j][k].push_back(new TH2F(Form("d%d_%s_vyz_%s_%s_Dmg%d",detID, postfix.c_str(), spH[i].c_str(),fbH[k].c_str(),j),
					  Form("%s for %s %s;vz[mm];vy[mm]",dmgTit[j].c_str(),fbH[k].c_str(),spTit[i].c_str()),
					  1000, vzRgMin, vzRgMax,
					  1000, vyRgMin, vyRgMax));
	
	  if (detID == 5719 || detID == 5720){
            rE[i][j][k].push_back(new TH2F(Form("d%d_%s_rE_%s_%s_Dmg%d",detID, postfix.c_str(), spH[i].c_str(),fbH[k].c_str(),j),
					   Form("%s for %s %s;Energy[MeV];r[mm]",dmgTit[j].c_str(),fbH[k].c_str(),spTit[i].c_str()),
					   141, -8, 6.1,
					   1000, 0, 8000));
            TwoDniceLogXBins(rE[i][j][k][ID2entry[detID+10000*subDet]]);

	    vxy[i][j][k].push_back(new TH2F(Form("d%d_%s_vxy_%s_%s_Dmg%d",detID, postfix.c_str(), spH[i].c_str(),fbH[k].c_str(),j),
					    Form("%s for %s %s;vx[mm];vy[mm]",dmgTit[j].c_str(),fbH[k].c_str(),spTit[i].c_str()),
					    1000, vxRgMin, vxRgMax,
					    1000, vyRgMin, vyRgMax));
	  
            vxy_region1[i][j][k].push_back(new TH2F(Form("d%d_%s_vxy_r1_%s_%s_Dmg%d",detID, postfix.c_str(), spH[i].c_str(),fbH[k].c_str(),j),
						    Form("%s for %s %s, r1;vx[mm];vy[mm]",dmgTit[j].c_str(),fbH[k].c_str(),spTit[i].c_str()),
						    1000, vxRgMin, vxRgMax,
						    1000, vyRgMin, vyRgMax));
            
            vxy_region2[i][j][k].push_back(new TH2F(Form("d%d_%s_vxy_r2_%s_%s_Dmg%d",detID, postfix.c_str(), spH[i].c_str(),fbH[k].c_str(),j),
						    Form("%s for %s %s, r2;vx[mm];vy[mm]",dmgTit[j].c_str(),fbH[k].c_str(),spTit[i].c_str()),
						    1000, vxRgMin, vxRgMax,
						    1000, vyRgMin, vyRgMax));
 
            vxy_region3[i][j][k].push_back(new TH2F(Form("d%d_%s_vxy_r3_%s_%s_Dmg%d",detID, postfix.c_str(), spH[i].c_str(),fbH[k].c_str(),j),
						    Form("%s for %s %s, r3;vx[mm];vy[mm]",dmgTit[j].c_str(),fbH[k].c_str(),spTit[i].c_str()),
						    1000, vxRgMin, vxRgMax,
						    1000, vyRgMin, vyRgMax));
            
            vxy_region4[i][j][k].push_back(new TH2F(Form("d%d_%s_vxy_r4_%s_%s_Dmg%d",detID, postfix.c_str(), spH[i].c_str(),fbH[k].c_str(),j),
						    Form("%s for %s %s, r4;vx[mm];vy[mm]",dmgTit[j].c_str(),fbH[k].c_str(),spTit[i].c_str()),
						    1000, vxRgMin, vxRgMax,
						    1000, vyRgMin, vyRgMax));
	  }
	}
      }
    }
}

void beamLineDetHistos::initHisto(TFile *fout, const int detID, const string detNm, const string postfix = "", 
				  const float range=2000, const int subDet=0){

  fout->cd();
  if(!fout->GetDirectory(Form("det%d",detID)))
    fout->mkdir(Form("det%d",detID), detNm.c_str());
  fout->cd(Form("det%d",detID));
  ID2entry.insert(std::pair<int, int>(detID+10000*subDet,ID2entry.size()));

  for(int k=0;k<nFB;k++)
    for(int i=0;i<nSpecies;i++){
      energy[i][k].push_back(new TH1F(Form("d%d_%s_energy_%s_%s", detID, postfix.c_str(), fbH[k].c_str(), spH[i].c_str()),
				      Form("energy distribution %s %s", fbH[k].c_str(), spH[i].c_str()),
				      141, -8, 6.1));
      niceLogXBins(energy[i][k][ID2entry[detID+10000*subDet]]);
      
      for(int j=0;j<nDmg;j++){
	xy[i][j][k].push_back(new TH2F(Form("d%d_%s_xy_%s_%s_Dmg%d",detID, postfix.c_str(), spH[i].c_str(),fbH[k].c_str(),j),
				       Form("%s for %s %s;x[mm];y[mm]",dmgTit[j].c_str(),fbH[k].c_str(),spTit[i].c_str()),
				       1000, -range, range,
				       1000, -range, range));

	r[i][j][k].push_back(new TH1F(Form("d%d_%s_r_%s_%s_Dmg%d",detID, postfix.c_str(), spH[i].c_str(),fbH[k].c_str(),j),
				      Form("%s for %s %s;r[mm];",dmgTit[j].c_str(),fbH[k].c_str(),spTit[i].c_str()),
				      1000, 0, range));
          
      }
    }
}

void beamLineDetHistos::writeOutput(TFile *fout, int detID, double scaleFactor,const int subDet=0) {
  if (ID2entry.find(detID+10000*subDet) == ID2entry.end())
    return;

  int det = ID2entry[detID+10000*subDet];
  fout->cd();
  fout->cd(Form("det%d",detID));
    
  for(int k=0;k<nFB;k++)
    for(int i=0;i<nSpecies;i++){
      if( (histos & 1) == 1){
	energy[i][k][det]->Scale(scaleFactor);
	energy[i][k][det]->Write();
      }

      for(int j=0;j<nDmg;j++){
	if( (histos & 1) == 1){
	  xy[i][j][k][det]->Scale(scaleFactor);
	  xy[i][j][k][det]->Write();
	  r[i][j][k][det]->Scale(scaleFactor);
	  r[i][j][k][det]->Write();
     
	  if (detID == 5719 || detID == 5720) {
            scatAng[i][j][k][det]->Scale(scaleFactor);
            scatAng[i][j][k][det]->Write();
            scatAng_region1[i][j][k][det]->Scale(scaleFactor);
            scatAng_region1[i][j][k][det]->Write();
            scatAng_region2[i][j][k][det]->Scale(scaleFactor);
            scatAng_region2[i][j][k][det]->Write();
	  }
	}
	if( (histos & 2) == 2){
	  vxz[i][j][k][det]->Scale(scaleFactor);
	  vxz[i][j][k][det]->Write();
	  vyz[i][j][k][det]->Scale(scaleFactor);
	  vyz[i][j][k][det]->Write();
        
	  if (detID == 5719 || detID == 5720) {
	    rE[i][j][k][det]->Scale(scaleFactor);
	    rE[i][j][k][det]->Write();
	    vxy[i][j][k][det]->Scale(scaleFactor);
	    vxy[i][j][k][det]->Write();

	    vxy_region1[i][j][k][det]->Scale(scaleFactor);
	    vxy_region1[i][j][k][det]->Write();
        
	    vxy_region2[i][j][k][det]->Scale(scaleFactor);
	    vxy_region2[i][j][k][det]->Write();
 
	    vxy_region3[i][j][k][det]->Scale(scaleFactor);
	    vxy_region3[i][j][k][det]->Write();
  
	    vxy_region4[i][j][k][det]->Scale(scaleFactor);
	    vxy_region4[i][j][k][det]->Write();
	  }
	}
      }
    }

}
#endif //__BEAMLINEDETHISTO_H
