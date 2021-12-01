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
  std::vector<TH2F*> vrz[nSpecies][nDmg][nFB];
  std::vector<TH2F*> rE[nSpecies][nDmg][nFB];

  std::vector<TH2F*> dXY[nDmg-1][nFB];
  std::vector<TH2F*> XY[nDmg][nFB];
  std::vector<TH1F*> denergy[nFB];
  std::vector<TH2F*> dXY_all[nDmg-1][nFB];
  std::vector<TH1F*> denergy_all[nFB];

 public:
  beamLineDetHistos(const int histCode=1) { histos=histCode;}
  void SetAnaDet(const int histCode=1) { histos=histCode;}
  ~beamLineDetHistos() {}
  void initHisto_detX(TFile*,const int,const string);
  void initHisto_Botdet(TFile*,const int,const string,const string,const float,const int);
  void initHisto(TFile*,const int,const string,const string,const float,const int);
  void initHisto(TFile*,const int,const string,
		 const float, const float,
		 const float, const float,
		 const float, const float,
		 const string,const float, const int);
  void fillHisto(const int detID, const int sp, const double rdDmg[3], const double pz,
		 const float xx, const float yy, const float kinE, 
		 const float vx, const float vy, const float vz, 
		 const int subDet);
  void fillHisto(const int detID, const int sp, const double rdDmg[3], const double pz,
		 const float xx, const float yy, const float kinE, const float rr,
		 const float vx, const float vy, const float vz, 
		 const int subDet);
  void fillHisto(const int detID, const int sp, const double rdDmg[3], const double pz,
		 const float xx, const float yy, const float zz, const float kinE, const float rr,
		 const float vx, const float vy, const float vz,
		 const int subDet);
  void fillHisto_Botdet(const int detID, const double rdDmg[3], const double pz,
			const float xx, const float yy, const float kinE, const float rr,
			const float vx, const float vy, const float vz,
			const int subDet);
  void fillHisto_detX(const int detID, const double rdDmg[3], const double pz,
		      const float xx, const float yy, const float kinE, const float rr);
  void fillHisto(const int detID, const int sp, const double rdDmg[3], const double pz,
		 const double xx, const double yy, const double kinE, const int subDet);
  void writeOutput(TFile *fout, int detID, double scaleFactor,const int subDet);
  void writeOutput_detX(TFile *fout,int detID, double scaleFactor);
  void writeOutput_Botdet(TFile *fout, int detID, double scaleFactor,const int subDet);

};

void beamLineDetHistos::fillHisto(const int detID, const int sp, const double rdDmg[3], const double pz,
				  const double xx, const double yy, const double kinE, const int subDet=0) {
  if (ID2entry.find(detID+10000*subDet) == ID2entry.end()) 
    return;

  int det = ID2entry[detID+10000*subDet];
  double rr = sqrt(xx*xx+yy*yy);

  energy[sp][0][det]->Fill(kinE);
  if(pz<0)
    energy[sp][2][det]->Fill(kinE);
  else
    energy[sp][1][det]->Fill(kinE);

  for(int kk=0;kk<nDmg;kk++){
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
  }
}

void beamLineDetHistos::fillHisto(const int detID, const int sp, const double rdDmg[3], const double pz,
				  const float xx, const float yy, const float kinE,
				  const float vx, const float vy, const float vz,
				  const int subDet=0) {
  if (ID2entry.find(detID+10000*subDet) == ID2entry.end()) 
    return;

  int det = ID2entry[detID+10000*subDet];
  double rr = sqrt(xx*xx+yy*yy);
  double vr = sqrt(pow(vx,2)+pow(vy,2));

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
   
      if (detID == 5619 || detID == 5620 || detID == 5714 || detID == 5719 || detID == 5720) {
	vrz[sp][kk][0][det]->Fill(vz,vr,rdDmg[kk]);
	if(pz<0)
	  vrz[sp][kk][2][det]->Fill(vz,vr,rdDmg[kk]);
	else
	  vrz[sp][kk][1][det]->Fill(vz,vr,rdDmg[kk]);
      }
      if (detID == 5719 || detID == 5720) {
	rE[sp][kk][0][det]->Fill(kinE,rr,rdDmg[kk]);
	if(pz<0)
	  rE[sp][kk][2][det]->Fill(kinE,rr,rdDmg[kk]);
	else
	  rE[sp][kk][1][det]->Fill(kinE,rr,rdDmg[kk]);
      }
        
    }
  }
}
void beamLineDetHistos::fillHisto(const int detID, const int sp, const double rdDmg[3], const double pz,
				  const float xx, const float yy, const float kinE, const float rr,
				  const float vx, const float vy, const float vz,
				  const int subDet=0) {
  if (ID2entry.find(detID+10000*subDet) == ID2entry.end()) 
    return;

  int det = ID2entry[detID+10000*subDet];
    
  double vr = sqrt(pow(vx,2)+pow(vy,2));
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
        
      if (detID == 5619 || detID == 5620 || detID == 5714 || detID == 5719 || detID == 5720) {
	vrz[sp][kk][0][det]->Fill(vz,vr,rdDmg[kk]);
	if(pz<0)
	  vrz[sp][kk][2][det]->Fill(vz,vr,rdDmg[kk]);
	else
	  vrz[sp][kk][1][det]->Fill(vz,vr,rdDmg[kk]);
      }
      if (detID == 5719 || detID == 5720) {
	rE[sp][kk][0][det]->Fill(kinE,rr,rdDmg[kk]);
	if(pz<0)
	  rE[sp][kk][2][det]->Fill(kinE,rr,rdDmg[kk]);
	else
	  rE[sp][kk][1][det]->Fill(kinE,rr,rdDmg[kk]);
      }
      
    }
      
  }
}
void beamLineDetHistos::fillHisto(const int detID, const int sp, const double rdDmg[3], const double pz,
				  const float xx, const float yy, const float zz, const float kinE, const float rr,
				  const float vx, const float vy, const float vz,
				  const int subDet=0) {
  if (ID2entry.find(detID+10000*subDet) == ID2entry.end())
    return;

  int det = ID2entry[detID+10000*subDet];
    
  double vr = sqrt(pow(vx,2)+pow(vy,2));
  
  //double ang = acos(zz/sqrt(xx*xx + yy*yy + zz*zz)) * 180/pi;
    
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
        
      if (detID == 5619 || detID == 5620 || detID == 5714 || detID == 5719 || detID == 5720) {
	vrz[sp][kk][0][det]->Fill(vz,vr,rdDmg[kk]);
	if(pz<0)
	  vrz[sp][kk][2][det]->Fill(vz,vr,rdDmg[kk]);
	else
	  vrz[sp][kk][1][det]->Fill(vz,vr,rdDmg[kk]);
      }
      if (detID == 5719 || detID == 5720) {
	rE[sp][kk][0][det]->Fill(kinE,rr,rdDmg[kk]);
	if(pz<0)
	  rE[sp][kk][2][det]->Fill(kinE,rr,rdDmg[kk]);
	else
	  rE[sp][kk][1][det]->Fill(kinE,rr,rdDmg[kk]);
      }
      
    }
      
  }
}


void beamLineDetHistos::fillHisto_Botdet(const int detID, const double rdDmg[3], const double pz,
					 const float xx, const float yy, const float kinE, const float rr,
					 const float vx, const float vy, const float vz,
					 const int subDet=0) {
  if (ID2entry.find(detID+10000*subDet) == ID2entry.end())
    return;

  int det = ID2entry[detID+10000*subDet];
  for(int kk=0;kk<nDmg;kk++){
    XY[kk][0][det]->Fill(xx,yy,rdDmg[kk]);
    if(pz<0)
      XY[kk][2][det]->Fill(xx,yy,rdDmg[kk]);
    else
      XY[kk][1][det]->Fill(xx,yy,rdDmg[kk]);
  }
}

void beamLineDetHistos::fillHisto_detX(const int detID, const double rdDmg[3], const double pz,
				       const float xx, const float yy, const float kinE, const float rr) {

  if (ID2entry.find(detID) == ID2entry.end())
    return;

  int det = ID2entry[detID];
  for(int kk=0;kk<nDmg-1;kk++){
    if (detID==28) {
      denergy_all[0][det]->Fill(kinE); dXY_all[kk][0][det]->Fill(xx,yy,rdDmg[kk]);
      if((rr>600 && rr<1200) && kinE>1 ) { denergy[0][det]->Fill(kinE); dXY[kk][0][det]->Fill(xx,yy,rdDmg[kk]);}
    }
    else {denergy[0][det]->Fill(kinE); dXY[kk][0][det]->Fill(xx,yy,rdDmg[kk]);}
    if(pz<0){
      if (detID==28) {
	denergy_all[2][det]->Fill(kinE); dXY_all[kk][2][det]->Fill(xx,yy,rdDmg[kk]);
	if((rr>600 && rr<1200) && kinE>1) { denergy[2][det]->Fill(kinE); dXY[kk][2][det]->Fill(xx,yy,rdDmg[kk]);}
      }
      else { denergy[2][det]->Fill(kinE); dXY[kk][2][det]->Fill(xx,yy,rdDmg[kk]);}
    }
    else{
      if (detID==28) {
	denergy_all[1][det]->Fill(kinE); dXY_all[kk][1][det]->Fill(xx,yy,rdDmg[kk]);
	if((rr>600 && rr<1200) && kinE>1) { denergy[1][det]->Fill(kinE); dXY[kk][1][det]->Fill(xx,yy,rdDmg[kk]);}
      }
      else { denergy[1][det]->Fill(kinE); dXY[kk][1][det]->Fill(xx,yy,rdDmg[kk]);}
              
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
        
	  if (detID == 5619 || detID == 5620 || detID == 5714 || detID == 5719 || detID == 5720) vrz[i][j][k].push_back(new TH2F(Form("d%d_%s_vrz_%s_%s_Dmg%d",detID, postfix.c_str(), spH[i].c_str(),fbH[k].c_str(),j),
																 Form("%s for %s %s;vz[mm];vr[mm]",dmgTit[j].c_str(),fbH[k].c_str(),spTit[i].c_str()),
																 1000, vzRgMin, vzRgMax,
																 1000, 0, 8000));
	
	  if (detID == 5719 || detID == 5720){ rE[i][j][k].push_back(new TH2F(Form("d%d_%s_rE_%s_%s_Dmg%d",detID, postfix.c_str(), spH[i].c_str(),fbH[k].c_str(),j),
									      Form("%s for %s %s;Energy[MeV];r[mm]",dmgTit[j].c_str(),fbH[k].c_str(),spTit[i].c_str()),
									      141, -8, 6.1,
									      1000, 0, 8000));
            TwoDniceLogXBins(rE[i][j][k][ID2entry[detID+10000*subDet]]);

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

void beamLineDetHistos::writeOutput_det5614(TFile *fout, int detID, double scaleFactor,const int subDet=0) {
  if (ID2entry.find(detID+10000*subDet) == ID2entry.end())
    return;
  int det = ID2entry[detID+10000*subDet];
  fout->cd();
  fout->cd(Form("det%d",detID));
  for(int k=0;k<nFB;k++)
    for(int j=0;j<nDmg;j++){
      XY[j][k][det]->Scale(scaleFactor);
      XY[j][k][det]->Write();
    }
    

}
void  beamLineDetHistos::writeOutput_detX(TFile *fout,int detID, double scaleFactor){
  if (ID2entry.find(detID) == ID2entry.end())
    return;
  int det = ID2entry[detID];
  fout->cd(Form("det%d",detID));
  for(int k=0;k<nFB;k++){
    for(int i=0;i<nDmg-1;i++){
      dXY[i][k][det]->Scale(scaleFactor);
      dXY[i][k][det]->Write();
    }
  }
}
void beamLineDetHistos::initHisto_Botdet(TFile *fout, const int detID, const string detNm, const string postfix = "",
					 const float range=2000, const int subDet=0){
  fout->cd();
  fout->cd(Form("det%d",detID));
  ID2entry.insert(std::pair<int, int>(detID+10000*subDet,ID2entry.size()));
  for(int k=0;k<nFB;k++)
    for(int j=0;j<nDmg;j++){
      XY[j][k].push_back(new TH2F(Form("d%d_%s_XY_%s_Dmg%d",detID, postfix.c_str(),fbH[k].c_str(),j),
				  Form("%s for %s;x[mm];y[mm]",dmgTit[j].c_str(),fbH[k].c_str()),
				  1000, -range, range,
				  1000, -range, range));
    }
}
void beamLineDetHistos::initHisto_detX(TFile *fout, const int detID, const string detNm){
  fout->cd();
  if(!fout->GetDirectory(Form("det%d",detID)))
    fout->mkdir(Form("det%d",detID), detNm.c_str());
  fout->cd(Form("det%d",detID));
  ID2entry.insert(std::pair<int, int>(detID,ID2entry.size()));
  const int subDet=0;
  for(int k=0;k<nFB;k++){
        
    denergy[k].push_back(new TH1F(Form("d%d__energy_%s", detID, fbH[k].c_str()),
				  Form("energy distribution %s", fbH[k].c_str()),
				  141, -8, 6.1));
    niceLogXBins(denergy[k][ID2entry[detID+10000*subDet]]);
        
    if (detID==28) {    denergy_all[k].push_back(new TH1F(Form("d%d__energy_all_%s", detID, fbH[k].c_str()),
							  Form("energy distribution %s", fbH[k].c_str()),
							  141, -8, 6.1));
      niceLogXBins(denergy_all[k][ID2entry[detID+10000*subDet]]);}

    for(int j=0;j<nDmg-1;j++){
      dXY[j][k].push_back(new TH2F(Form("d%d_XY_%s_Dmg%d",detID, fbH[k].c_str(),j),
				   Form("%s hits for %s;x[mm];y[mm]",dmgTit[j].c_str(), fbH[k].c_str()),
				   4000, -8000, 8000,
				   4000, -8000, 8000));
        
      if (detID==28)     dXY_all[j][k].push_back(new TH2F(Form("d%d_XY_all_%s_Dmg%d",detID, fbH[k].c_str(),j),
							  Form("%s hits for %s;x[mm];y[mm]",dmgTit[j].c_str(), fbH[k].c_str()),
							  4000, -8000, 8000,
							  4000, -8000, 8000));
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
     
	}
	if( (histos & 2) == 2){
	  vxz[i][j][k][det]->Scale(scaleFactor);
	  vxz[i][j][k][det]->Write();
	  vyz[i][j][k][det]->Scale(scaleFactor);
	  vyz[i][j][k][det]->Write();
	  if (detID == 5619 || detID == 5620 || detID == 5714 || detID == 5719 || detID == 5720) {
	    vrz[i][j][k][det]->Scale(scaleFactor);
	    vrz[i][j][k][det]->Write();
	  }
	  if (detID == 5719 || detID == 5720) {
	    rE[i][j][k][det]->Scale(scaleFactor);
	    rE[i][j][k][det]->Write();
	  }
	}
      }
    }

}

void beamLineDetHistos::writeOutput_Botdet(TFile *fout, int detID, double scaleFactor,const int subDet=0) {
  if (ID2entry.find(detID+10000*subDet) == ID2entry.end())
    return;
  int det = ID2entry[detID+10000*subDet];
  fout->cd();
  fout->cd(Form("det%d",detID));
  for(int k=0;k<nFB;k++)
    for(int j=0;j<nDmg;j++){
      XY[j][k][det]->Scale(scaleFactor);
      XY[j][k][det]->Write();
    }
    

}
void  beamLineDetHistos::writeOutput_detX(TFile *fout,int detID, double scaleFactor){
  if (ID2entry.find(detID) == ID2entry.end())
    return;
  int det = ID2entry[detID];
  fout->cd(Form("det%d",detID));
  for(int k=0;k<nFB;k++){
    denergy[k][det]->Scale(scaleFactor);
    denergy[k][det]->Write();
    if (detID==28) {   denergy_all[k][det]->Scale(scaleFactor);
      denergy_all[k][det]->Write();}
    for(int i=0;i<nDmg-1;i++){
      dXY[i][k][det]->Scale(scaleFactor);
      dXY[i][k][det]->Write();
      if (detID==28) {  dXY_all[i][k][det]->Scale(scaleFactor);
	dXY_all[i][k][det]->Write();}
    }
  }
}
#endif //__BEAMLINEDETHISTO_H
