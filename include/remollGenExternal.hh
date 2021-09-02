/*
 * remollGenExternal.hh
 *
 *  Created on: Mar 17, 2017
 *      Author: wdconinc
 */

#ifndef __REMOLLGENEXTERNAL_HH
#define __REMOLLGENEXTERNAL_HH

/*!
 * External event generator
 *
 * This event generator reads events from an external ROOT file, structured
 * like the remoll output ROOT files, and throws hits stored in a particular
 * detector ID (to be specified through a messenger class)
 *
*/

// Base class headers
#include "remollVEventGen.hh"

// System headers
#include <vector>

#include "G4AutoLock.hh"
#include "Randomize.hh"

// Forward declarations
class TFile;
class TTree;
struct remollEvent_t;
struct remollGenericDetectorHit_t;

// Class definition
class remollGenExternal : public remollVEventGen {

    public:
        remollGenExternal();
        virtual ~remollGenExternal();

        void SetGenExternalFile(G4String& filename);
        void SetGenExternalZOffset(G4double tempzOffset) {
            fzOffset = tempzOffset;
        }
        void SetGenExternalDetID(const G4int detid) {
            fDetectorID = detid;
        }
        void SetGenExternalEntry(const G4int firstEventID){
            fEntry = ((firstEventID >= 0)? firstEventID : G4RandFlat::shoot(fEntries));
        }

    private:
        void SamplePhysics(remollVertex *, remollEvent *);
        // External event file and tree, entry number
        TFile* fFile;
        TTree* fTree;
        Int_t fEntry, fEntries;

        // Event and hit structures
        remollEvent_t* fEvent;
        std::vector<remollGenericDetectorHit_t>* fHit;
        G4double fzOffset;
  G4double rate;

        // Detector ID to consider
        G4int fDetectorID;
        G4int fLoopID;

};

#endif //__REMOLLGENEXTERNAL_HH
