#ifndef remollIO_HH
#define remollIO_HH

#include "TROOT.h"
#include "TObject.h"

#include "G4Run.hh"
#include "G4Threading.hh"
#include "G4AutoLock.hh"

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

namespace { G4Mutex remollIOMutex = G4MUTEX_INITIALIZER; }

//FIXME: forward declares not possible since xerces uses a
// namespace alias and requires upstream knowledge or a pre-
// processor directive, which in turn requires another header
// so there's no gain...
//#include <xercesc/dom/DOMElement.hpp>
// or
#include <xercesc/util/XercesDefs.hpp>
XERCES_CPP_NAMESPACE_BEGIN
class DOMElement;
XERCES_CPP_NAMESPACE_END

#define __FILENAMELEN 255

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

        void RegisterDetector(G4String lvname, G4String sdname, G4int no) {
          G4AutoLock lock(&remollIOMutex);
          fDetNos.push_back(no);
          fDetLVNames += (fDetLVNames.size() > 0? ":": "") + lvname + "/I";
          fDetSDNames += (fDetSDNames.size() > 0? ":": "") + sdname + "/I";
        }

    private:
	TFile *fFile;
	TTree *fTree;

        G4GenericMessenger* fMessenger;

        G4String fFilename;

	std::vector<G4String> fGDMLFileNames;
	std::vector<G4String> fXMLFileNames;

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

        // Detectors
        std::vector<Int_t> fDetNos;
        G4String fDetLVNames;
        G4String fDetSDNames;

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

	//  GenericDetectorSum
    public:
	void AddGenericDetectorSum(remollGenericDetectorSum *);
    private:
        std::vector<remollGenericDetectorSum_t> fGenDetSum;
};

#endif//remollIO_HH
