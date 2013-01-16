#ifndef remollIO_HH
#define remollIO_HH

#include "TROOT.h"
#include "TObject.h"
#include "G4Run.hh"
#include "remolltypes.hh"

class TFile;
class TTree;

class remollGenericDetectorHit;
class remollGenericDetectorSum;

#define __IO_MAXHIT 10000

// Units for output
#define __E_UNIT GeV
#define __L_UNIT m
#define __T_UNIT ns

class remollIO {
    public:
	 remollIO();
	~remollIO();

	void SetFilename(const char *fn){strcpy(fFilename, fn);}

	void FillTree();
	void Flush();
	void WriteTree();


	void InitializeTree();
    private:
	TFile *fFile;
	TTree *fTree;

	char fFilename[255];

	//  Interfaces and buffers to the tree
	//  This is potentially going to get very long...

	//  GenericDetectorHit
    public:
	void AddGenericDetectorHit(remollGenericDetectorHit *);
    private:
	Int_t fNGenDetHit;
	Int_t fGenDetHit_det[__IO_MAXHIT];
	Int_t fGenDetHit_id[__IO_MAXHIT];

	Int_t fGenDetHit_trid[__IO_MAXHIT];
	Int_t fGenDetHit_pid[__IO_MAXHIT];
	Int_t fGenDetHit_gen[__IO_MAXHIT];
	Int_t fGenDetHit_mtrid[__IO_MAXHIT];

	Double_t fGenDetHit_X[__IO_MAXHIT];
	Double_t fGenDetHit_Y[__IO_MAXHIT];
	Double_t fGenDetHit_Z[__IO_MAXHIT];

	Double_t fGenDetHit_Px[__IO_MAXHIT];
	Double_t fGenDetHit_Py[__IO_MAXHIT];
	Double_t fGenDetHit_Pz[__IO_MAXHIT];
	Double_t fGenDetHit_P[__IO_MAXHIT];
	Double_t fGenDetHit_E[__IO_MAXHIT];
	Double_t fGenDetHit_M[__IO_MAXHIT];

	Double_t fGenDetHit_Vx[__IO_MAXHIT];
	Double_t fGenDetHit_Vy[__IO_MAXHIT];
	Double_t fGenDetHit_Vz[__IO_MAXHIT];

	//  GenericDetectorSum
    public:
	void AddGenericDetectorSum(remollGenericDetectorSum *);
    private:
	Int_t fNGenDetSum;
	Int_t fGenDetSum_det[__IO_MAXHIT];
	Int_t fGenDetSum_id[__IO_MAXHIT];
	Double_t fGenDetSum_edep[__IO_MAXHIT];
};

#endif//remollIO_HH
