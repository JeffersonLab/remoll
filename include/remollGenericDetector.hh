#ifndef __REMOLLGENERICDETECTOR_HH
#define __REMOLLGENERICDETECTOR_HH

#include "G4VSensitiveDetector.hh"
#include "remollGenericDetectorHit.hh"
#include "remollGenericDetectorSum.hh"

#include <map>

/*! 
      Default detector class.  This will record information on:

      - Primary generated hit information
      - Secondary hit information on particles not produced within the volume
      - Calorimetric data (total energy deposited)

      This requires two hit collections, one for "hits" and one for "sum"
      Summing occurs uniquely over copyIDs
*/

class G4HCofThisEvent;
class G4Step;
class G4TouchableHistory;

class remollGenericDetector : public G4VSensitiveDetector {
    public:
	remollGenericDetector( G4String name, G4int detnum );
	virtual ~remollGenericDetector();

	virtual void Initialize(G4HCofThisEvent*);
	virtual G4bool ProcessHits(G4Step*,G4TouchableHistory*);
	virtual void EndOfEvent(G4HCofThisEvent*);

    private:
	remollGenericDetectorHitsCollection *fHitColl;
	remollGenericDetectorSumCollection  *fSumColl;
	G4int fHCID, fSCID;

	std::map<int, remollGenericDetectorSum *> fSumMap;

	G4bool fTrackSecondaries;
	G4int fDetNo;
};

#endif//__REMOLLGENERICDETECTOR_HH
