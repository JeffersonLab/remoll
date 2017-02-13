#ifndef remollIO_HH
#define remollIO_HH

#include "TROOT.h"
#include "TObject.h"
#include "G4Run.hh"
#include "remolltypes.hh"

#include "G4String.hh"

#include <vector>

class TFile;
class TTree;

class remollGenericDetectorHit;
class remollGenericDetectorSum;
class remollEvent;

#include <xercesc/dom/DOMElement.hpp>


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

	std::vector<G4String>       fGDMLFileNames;

	void SearchGDMLforFiles(G4String );
	void TraverseChildren(  xercesc::DOMElement * );

	//  Interfaces and buffers to the tree
	//  This is potentially going to get very long...

	// Event data
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

        // Event particles
    public:
        void SetEventData(remollEvent *);
    private:
        std::vector<remollEventParticle_t> fEvPart;

	//  GenericDetectorHit
    public:
	void AddGenericDetectorHit(remollGenericDetectorHit *);
    private:
	std::vector<remollGenericDetectorHit_t> fGenDetHit;

	Int_t fCollCut;

	//  GenericDetectorSum
    public:
	void AddGenericDetectorSum(remollGenericDetectorSum *);
    private:
        std::vector<remollGenericDetectorSum_t> fGenDetSum;
};

#endif//remollIO_HH
