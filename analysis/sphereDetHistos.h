#ifndef __SPHEREDETHISTO_H
#define __SPHEREDETHISTO_H

/* NOT MEANT TO BE USED INDEPENDENTLY

   INPUT: output file and  detector ID
*/
#include "anaConst.h"

class sphereDetHistos {
  private:
    //theta, phi
    std::vector<TH2F*> tp[nSpecies][nDmg][nFB];
    //energy of upstream half
    std::vector<TH1F*> energyUS[nSpecies][nFB];
    //energy of downstream half
    std::vector<TH1F*> energyDS[nSpecies][nFB];

    std::map<int, int> ID2entry;

  public:
    sphereDetHistos() {}
    ~sphereDetHistos() {}
    void initHisto(TFile *fout,int detID, const char * detNm);
    void fillHisto(int detID, int sp, double rdDmg[3],double pz,
		   double xx, double yy,double zz, double kinE);
    void writeOutput(TFile *fout, int detID, double scaleFactor);
};

void sphereDetHistos::initHisto(TFile *fout,int detID, const char * detNm){
  fout->cd();
  if (!fout->GetDirectory(Form("det%d", detID)))
    fout->mkdir(Form("det%d",detID),detNm);
  fout->cd(Form("det%d",detID));

  ID2entry.insert(std::pair<int, int>(detID, ID2entry.size()));
  for(int k=0;k<nFB;k++)
    for(int i=0;i<nSpecies;i++){
      energyUS[i][k].push_back(new TH1F(Form("d%d_energyUS_%s_%s",detID,fbH[k].c_str(),spH[i].c_str()),
					  Form("energy distribution first half %s %s",fbH[k].c_str(),spH[i].c_str()),
					  121,-8,4.1));
      niceLogXBins(energyUS[i][k][ID2entry[detID]]);

      energyDS[i][k].push_back(new TH1F(Form("d%d_energyDS_%s_%s",detID,fbH[k].c_str(),spH[i].c_str()),
					  Form("energy distribution second half %s %s",fbH[k].c_str(),spH[i].c_str()),
					  121,-8,4.1));
      niceLogXBins(energyDS[i][k][ID2entry[detID]]);
      
      for(int j=0;j<nDmg;j++){
	tp[i][j][k].push_back(new TH2F(Form("d%d_tp_%s_%s_Dmg%d",detID,spH[i].c_str(),fbH[k].c_str(),j),
					    Form("%s for %s %s (hit angles);theta[deg];phi[deg]",
						 dmgTit[j].c_str(),fbH[k].c_str(),spTit[i].c_str()),
					   800,0,180,
					   800,-180,180));
      }
    }
}

/// pz>0 for things going out of the sphere 
void sphereDetHistos::fillHisto(int detID, int sp, double rdDmg[3],double pz,
		      double xx, double yy,double zz, double kinE){
  if (ID2entry.find(detID) == ID2entry.end()) 
    return;

  int det = ID2entry[detID];
  if(zz<0){
    energyUS[sp][0][det]->Fill(kinE);
    if(pz<0)
      energyUS[sp][2][det]->Fill(kinE);
    else
      energyUS[sp][1][det]->Fill(kinE);
  }else{
    energyDS[sp][0][det]->Fill(kinE);
    if(pz<0)
      energyDS[sp][2][det]->Fill(kinE);
    else
      energyDS[sp][1][det]->Fill(kinE);
  }

  double th = acos(zz/sqrt(xx*xx + yy*yy + zz*zz))/pi*180;
  double ph = atan2(yy,xx)/pi*180;
  ph = fmod(ph,180);

  for(int kk=0;kk<nDmg;kk++){
    tp[sp][kk][0][det]->Fill(th,ph,rdDmg[kk]);
    if(pz<0)
      tp[sp][kk][2][det]->Fill(th,ph,rdDmg[kk]);
    else
      tp[sp][kk][1][det]->Fill(th,ph,rdDmg[kk]);
  }
}

void sphereDetHistos::writeOutput(TFile *fout, int detID, double scaleFactor){
  if (ID2entry.find(detID) == ID2entry.end()) 
    return;

  int det = ID2entry[detID];
  fout->cd();
  fout->cd(Form("det%d",detID));
  for(int k=0;k<nFB;k++)
    for(int i=0;i<nSpecies;i++){
      energyUS[i][k][det]->Scale(scaleFactor);
      energyUS[i][k][det]->Write();
      energyDS[i][k][det]->Scale(scaleFactor);
      energyDS[i][k][det]->Write();
      
      for(int j=0;j<nDmg;j++){
        tp[i][j][k][det]->Scale(scaleFactor);
        tp[i][j][k][det]->Write();
      }
    }
}
#endif //__SPHEREDETHISTO_H
