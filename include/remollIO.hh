#ifndef remollIO_HH
#define remollIO_HH

#include "TROOT.h"
#include "TObject.h"
#include "G4Run.hh"
#include "remolltypes.hh"
#include "remollEvent.hh"
#include "G4PhysicalConstants.hh"
//#include "PhysicalConstants.h"

#include "G4String.hh"

class TFile;
class TTree;

class remollGenericDetectorHit;
class remollGenericDetectorSum;
class remollEvent;

#include <xercesc/dom/DOMElement.hpp>


#define __IO_MAXHIT 10000
#define __FILENAMELEN 255

// Units for output
#define __E_UNIT GeV
#define __L_UNIT m
#define __T_UNIT ns
#define __ANG_UNIT rad
#define __ASYMM_SCALE 1e-9 // ppb


class remollIO {
    public:
	 remollIO();
	~remollIO();

	void SetFilename(G4String  fn);
	G4String GetFilename(){return fFilename;}

	void FillTree();
	void Flush();
	void WriteTree();

	void WriteRun();

	void InitializeTree();

	void GrabGDMLFiles( G4String fn );

    private:
	TFile *fFile;
	TTree *fTree;

	char fFilename[__FILENAMELEN];
	
	//remollEvent *fEvent; // NEW

	std::vector<G4String>       fGDMLFileNames;

	void SearchGDMLforFiles(G4String );
	void TraverseChildren(  xercesc::DOMElement * );

	//  Interfaces and buffers to the tree
	//  This is potentially going to get very long...

	// Event data
    public:
	void SetEventData(remollEvent *);
	//void IOSetEvent( remollEvent *remollEvt ){ fEvent = remollEvt; } // NEW
	//remollEvent* IOGetEvent(){return fEvent;} //NEW
    private:
	Int_t fNEvPart;

	Double_t fEvRate;
	Double_t fEvEffXS;
	Double_t fEvAsym;
	Double_t fEvmAsym;
	Double_t fEvBeamP;
	Double_t fEvQ2;
	Double_t fEvW2;
	Double_t fEvThCoM;

	Double_t fBmX;
	Double_t fBmY;
	Double_t fBmZ;
	Double_t fBmdX;
	Double_t fBmdY;
	Double_t fBmdZ;
	Double_t fBmth;
	Double_t fBmph;

	Int_t fEvPID[__IO_MAXHIT];

	Double_t fEvPart_X[__IO_MAXHIT];
	Double_t fEvPart_Y[__IO_MAXHIT];
	Double_t fEvPart_Z[__IO_MAXHIT];
	Double_t fEvPart_Px[__IO_MAXHIT];
	Double_t fEvPart_Py[__IO_MAXHIT];
	Double_t fEvPart_Pz[__IO_MAXHIT];
	Double_t fEvPart_Th[__IO_MAXHIT];
	Double_t fEvPart_Ph[__IO_MAXHIT];
	Double_t fEvPart_P[__IO_MAXHIT];
	Double_t fEvPart_tPx[__IO_MAXHIT];
	Double_t fEvPart_tPy[__IO_MAXHIT];
	Double_t fEvPart_tPz[__IO_MAXHIT];

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
	Double_t fGenDetHit_R[__IO_MAXHIT];
	Double_t fGenDetHit_Ph[__IO_MAXHIT];

	Double_t fGenDetHit_Px[__IO_MAXHIT];
	Double_t fGenDetHit_Py[__IO_MAXHIT];
	Double_t fGenDetHit_Pz[__IO_MAXHIT];
	Double_t fGenDetHit_P[__IO_MAXHIT];
	Double_t fGenDetHit_E[__IO_MAXHIT];
	Double_t fGenDetHit_M[__IO_MAXHIT];

	Double_t fGenDetHit_Vx[__IO_MAXHIT];
	Double_t fGenDetHit_Vy[__IO_MAXHIT];
	Double_t fGenDetHit_Vz[__IO_MAXHIT];

	Double_t fGenDetHit_Edep[__IO_MAXHIT];

  // new tracking Last Significant Non-Detector Hit information

	Double_t fGenDetHit_Lx[__IO_MAXHIT];
	Double_t fGenDetHit_Ly[__IO_MAXHIT];
	Double_t fGenDetHit_Lz[__IO_MAXHIT];
	
//	Double_t fHitEvPart_LPx[__IO_MAXHIT];
//	Double_t fHitEvPart_LPy[__IO_MAXHIT];
//	Double_t fHitEvPart_LPz[__IO_MAXHIT];
	
	Double_t fGenDetHit_LdE[__IO_MAXHIT];
	Double_t fGenDetHit_LdTh[__IO_MAXHIT];


	Int_t fCollCut;


	//  GenericDetectorSum
    public:
	void AddGenericDetectorSum(remollGenericDetectorSum *);
    private:
	Int_t fNGenDetSum;
	Int_t fGenDetSum_det[__IO_MAXHIT];
	Int_t fGenDetSum_id[__IO_MAXHIT];
	Double_t fGenDetSum_edep[__IO_MAXHIT];
	Int_t fGenDetSum_pid[__IO_MAXHIT];
	Double_t fGenDetSum_x[__IO_MAXHIT];
	Double_t fGenDetSum_y[__IO_MAXHIT];
	Double_t fGenDetSum_z[__IO_MAXHIT];
};

#endif//remollIO_HH
