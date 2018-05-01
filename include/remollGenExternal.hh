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
        void SetGenExternalDetID(const G4int detid) {
          fDetectorID = detid;
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

        // Detector ID to consider
        G4int fDetectorID;

};

#endif //__REMOLLGENEXTERNAL_HH
