#ifndef __FLATDEDETHISTO_H
#define __FLATDEDETHISTO_H

/* NOT MEANT TO BE USED INDEPENDENTLY

   INPUT: output file and  detector ID
*/
#include "anaConst.h"

class flatHEdetHistos {
  private:
    std::vector<TH2F*> xy[nSpecies][nFB];
    std::vector<TH1F*> vz[nSpecies][nFB];
    std::vector<TH2F*> vxz[nSpecies][nFB];
    std::vector<TH2F*> vyz[nSpecies][nFB];
    std::map<int, int> ID2entry;

  public:
    flatHEdetHistos() {}
    ~flatHEdetHistos() {}
    void initHisto( TFile *fout, int detID, const char * detNm, const char * cut,
		    int hRange, int vZmin, int vZmax);
    void fillHisto( int detID, int sp, double pz,
		    double xx, double yy, 
		    double vxx,double vyy,double vzz);
    void writeOutput(TFile *fout, int detID, double scaleFactor);
};

void flatHEdetHistos::initHisto(TFile *fout, int detID, const char * detNm, const char * cut = "",
				int hRange=4000, int vZmin=-30000, int vZmax=45000) {
  fout->cd();
  if(!fout->GetDirectory(Form("det%d",detID)))
    fout->mkdir(Form("det%d",detID),detNm);
  fout->cd(Form("det%d",detID));

  ID2entry.insert(std::pair<int, int>(detID,ID2entry.size()));
  for(int k=0;k<nFB;k++)
    for(int i=0;i<nSpecies;i++){
      string dir = fbH[k];
      if ((detID == 5555 || detID == 5556) && k==0)
	dir.replace(dir.find("PZ"), 2, "PY");

      xy[i][k].push_back(new TH2F(Form("d%dHE%s_xy_%s_%s", detID, cut, spH[i].c_str(), dir.c_str()),
				  Form("hits for %s %s;x[mm];y[mm]", dir.c_str(), spTit[i].c_str()),
				  800, -hRange, hRange,
				  800, -hRange, hRange));
      
      vz[i][k].push_back(new TH1F(Form("d%dHE%s_vz_%s_%s", detID, cut, spH[i].c_str(), dir.c_str()),
				  Form("hits for %s %s;vz[mm];", dir.c_str(), spTit[i].c_str()),
				  1000, vZmin, vZmax));

      vxz[i][k].push_back(new TH2F(Form("d%dHE%s_vxz_%s_%s", detID, cut, spH[i].c_str(), dir.c_str()),
				   Form("hits for %s %s;vz[mm];vx[mm]", dir.c_str(), spTit[i].c_str()),
				   1000, vZmin, vZmax,
				   1000, -3000, 3000));
      vyz[i][k].push_back(new TH2F(Form("d%dHE%s_vyz_%s_%s", detID, cut, spH[i].c_str(), dir.c_str()),
				   Form("hits for %s %s;vz[mm];vy[mm]", dir.c_str(), spTit[i].c_str()),
				   1000, vZmin, vZmax,
				   1000, -3000, 3000));
    }
}


void flatHEdetHistos::fillHisto(int detID, int sp, double pz, double xx, double yy, 
				double vxx, double vyy, double vzz) {
  if (ID2entry.find(detID) == ID2entry.end()) 
    return;

  int det = ID2entry[detID];
  vz[sp][0][det]->Fill(vzz);
  vxz[sp][0][det]->Fill(vzz,vxx);
  vyz[sp][0][det]->Fill(vzz,vyy);
  if(pz<0){
    vz[sp][2][det]->Fill(vzz);
    vxz[sp][2][det]->Fill(vzz,vxx);
    vyz[sp][2][det]->Fill(vzz,vyy);
  }else{
    vz[sp][1][det]->Fill(vzz);
    vxz[sp][1][det]->Fill(vzz,vxx);
    vyz[sp][1][det]->Fill(vzz,vyy);
  }

  xy[sp][0][det]->Fill(xx,yy);
  if(pz<0)
    xy[sp][2][det]->Fill(xx,yy);
  else
    xy[sp][1][det]->Fill(xx,yy);
}

void flatHEdetHistos::writeOutput(TFile *fout, int detID, double scaleFactor){
  if (ID2entry.find(detID) == ID2entry.end()) 
    return;

  int det = ID2entry[detID];
  fout->cd();
  fout->cd(Form("det%d",detID));
  for(int k=0;k<nFB;k++)
    for(int i=0;i<nSpecies;i++){
      vz[i][k][det]->Scale(scaleFactor);
      vz[i][k][det]->Write();

      vxz[i][k][det]->Scale(scaleFactor);
      vxz[i][k][det]->Write();
      
      vyz[i][k][det]->Scale(scaleFactor);
      vyz[i][k][det]->Write();
      
      xy[i][k][det]->Scale(scaleFactor);
      xy[i][k][det]->Write();
    }
}
#endif //__FLATDEDETHISTO_H
