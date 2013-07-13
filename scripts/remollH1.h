#ifndef remollH1_h
#define remollH1_h

#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>
#include <TH1F.h>
#include <TCanvas.h>
#include <TString.h>
#include <vector>
#include <math.h>
#include <TPaveLabel.h>


class remollAna;

class remollH1 {
    public:
  //        enum Rcut_t {kNoCut, kCut1, kCut2, kCut3, kCut4, kCut5, kCut6};
        enum Rcut_t {kNoCut, kCut};
	enum Sect_t {kSect1, kSect2, kSect3, kAll};
	enum Vert_t {kUp, kMid, kDown, kFull};

	enum Index_t {kSingle, kEvent, kHit};


	remollH1(Double_t *v, Index_t idx, remollAna *t, Double_t min, Double_t max, const char *name, const char *title, const char *, const char *);
	~remollH1();

	void Fill(const char *);
	void Draw(Rcut_t rcut, const char *);
	void Write(const char *);

	//	const char *GetHistName(){ return fName; }

    private:
	// Use this to cut on variables locally
	Double_t *fVar;  // Primary variable
	Index_t   fIdx;

	remollAna *fT;

	Int_t    fNbin;
	Double_t fMin;
	Double_t fMax;

	TString fName;
	TString fTitle;

	//	Double_t fRmin[7], fRmax[7];
	Double_t fRmin, fRmax;


	//	TH1F *fH[7][4][4]; // Rcut, vertex, sector separated plots plots
	TH1F *fH[2][4][4]; // Rcut, vertex, sector separated plots plots

	void FillOne( TH1F *, Int_t, Int_t );

};

#endif // #ifdef remollH1_h
