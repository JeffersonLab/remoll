#ifndef __FLATHEDSDETHISTO_H
#define __FLATHEDSDETHISTO_H

/* NOT MEANT TO BE USED INDEPENDENTLY

   INPUT: output file and  detector ID
*/
#include "anaConst.h"

class flatHEdsDetHistos {
  private:
    std::vector<TH2F*> xy[nSpecies][nFB];
    std::vector<TH1F*> vz[nSpecies][nFB];
    std::map<int, int> ID2entry;

  public:
    flatHEdsDetHistos() {}
    ~flatHEdsDetHistos() {}
    void initHisto( TFile *fout, int detID, const char * detNm, 
		    int hRange, int vZmin, int vZmax);
    void fillHisto( int detID, int sp, double pz,
		    double xx, double yy, double zz);
    void writeOutput(TFile *fout, int detID, double scaleFactor);
};

void flatHEdsDetHistos::initHisto(TFile *fout, int detID, const char * detNm, 
		    int hRange, int vZmin=-30000, int vZmax=45000) {
  fout->cd();
  if(!fout->GetDirectory(Form("det%d",detID)))
    fout->mkdir(Form("det%d",detID),detNm);
  fout->cd(Form("det%d",detID));

  ID2entry.insert(std::pair<int, int>(detID,ID2entry.size()));
  for(int k=0;k<nFB;k++)
    for(int i=0;i<nSpecies;i++){
      xy[i][k].push_back(new TH2F(Form("d%dHEds_xy_%s_%s", detID, spH[i].c_str(), fbH[k].c_str()),
				  Form("hits for %s %s;x[mm];y[mm]", fbH[k].c_str(), spTit[i].c_str()),
				  800, -hRange, hRange,
				  800, -hRange, hRange));
      
      vz[i][k].push_back(new TH1F(Form("d%dHEds_vz_%s_%s",detID, spH[i].c_str(), fbH[k].c_str()),
				  Form("hits for %s %s;vz[mm];", fbH[k].c_str(), spTit[i].c_str()),
				  2000, vZmin, vZmax));
    }
}


void flatHEdsDetHistos::fillHisto(int detID, int sp, double pz, double xx, double yy, double zz) {
  if (ID2entry.find(detID) == ID2entry.end()) 
    return;

  int det = ID2entry[detID];
  vz[sp][0][det]->Fill(zz);
  if(pz<0)
    vz[sp][2][det]->Fill(zz);
  else
    vz[sp][1][det]->Fill(zz);

  xy[sp][0][det]->Fill(xx,yy);
  if(pz<0)
    xy[sp][2][det]->Fill(xx,yy);
  else
    xy[sp][1][det]->Fill(xx,yy);
}

void flatHEdsDetHistos::writeOutput(TFile *fout, int detID, double scaleFactor){
  if (ID2entry.find(detID) == ID2entry.end()) 
    return;

  int det = ID2entry[detID];
  fout->cd();
  fout->cd(Form("det%d",detID));
  for(int k=0;k<nFB;k++)
    for(int i=0;i<nSpecies;i++){
      vz[i][k][det]->Scale(scaleFactor);
      vz[i][k][det]->Write();
      
      xy[i][k][det]->Scale(scaleFactor);
      xy[i][k][det]->Write();
    }
}
#endif //__FLATHEDSDETHISTO_H
