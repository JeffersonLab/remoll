#ifndef remollIO_HH
#define remollIO_HH

#include "TROOT.h"
#include "TObject.h"

#include "G4Run.hh"

#include "remolltypes.hh"
#include "remollSystemOfUnits.hh"

#include "G4String.hh"

#include <vector>

class TFile;
class TTree;

class G4GenericMessenger;

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
    private:
        // Singleton pointer
        static remollIO* gInstance;
        // Private constructor
        remollIO();

    public:
        // Public destructor
        virtual ~remollIO();
        // Static instance getter
        static remollIO* GetInstance();

	void SetFilename(const G4String& name) { fFilename = name; }
	G4String GetFilename() const { return fFilename; }

	void FillTree();
	void Flush();
	void WriteTree();

	void WriteRun();

	void InitializeTree();

	void GrabGDMLFiles( G4String fn );

    private:
	TFile *fFile;
	TTree *fTree;

        G4GenericMessenger* fMessenger;

        G4String fFilename;

	std::vector<G4String> fGDMLFileNames;

	void SearchGDMLforFiles(G4String );
	void TraverseChildren(  xercesc::DOMElement * );

	//  Interfaces and buffers to the tree
	//  This is potentially going to get very long...

	// Event data
    public:
	void SetEventSeed(const G4String& seed);
	void SetEventData(const remollEvent *);
    private:

	// Units
	remollUnits_t fUnits;

	// Event data
	Double_t fEvRate;
	TString fEvSeed;
	remollEvent_t fEv;
	remollBeamTarget_t fBm;

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
