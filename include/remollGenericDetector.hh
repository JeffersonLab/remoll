#ifndef __REMOLLGENERICDETECTOR_HH
#define __REMOLLGENERICDETECTOR_HH

#include "G4VSensitiveDetector.hh"

// Included to avoid forward declaration of collection typedef
#include "remollGenericDetectorHit.hh"
#include "remollGenericDetectorSum.hh"

#include <map>
#include <list>

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
class G4GenericMessenger;

class remollGenericDetectorSum;

class remollGenericDetector : public G4VSensitiveDetector {
    private:
        static G4GenericMessenger* fStaticMessenger;
        static std::list<remollGenericDetector*> fGenericDetectors;
        static void InsertGenericDetector(remollGenericDetector* det) {
          fGenericDetectors.push_back(det);
          fGenericDetectors.sort(isBefore);
        }
        static void EraseGenericDetector(remollGenericDetector* det) {
          fGenericDetectors.remove(det);
        }
        void PrintAll() {
          for (std::list<remollGenericDetector*>::iterator
            it  = fGenericDetectors.begin();
            it != fGenericDetectors.end();
            it++) {
              (*it)->PrintEnabled();
          }
        }
        void SetAllEnabled() {
          for (std::list<remollGenericDetector*>::iterator
            it  = fGenericDetectors.begin();
            it != fGenericDetectors.end();
            it++) {
              (*it)->SetEnabled();
          }
        }
        void SetAllDisabled() {
          for (std::list<remollGenericDetector*>::iterator
            it  = fGenericDetectors.begin();
            it != fGenericDetectors.end();
            it++) {
              (*it)->SetDisabled();
          }
        }

        static bool isBefore(const remollGenericDetector* left, const remollGenericDetector* right) {
          return (left? (right? (left->fDetNo < right->fDetNo): false): true);
        }

    public:
	remollGenericDetector( G4String name, G4int detnum );
	virtual ~remollGenericDetector();

	virtual void Initialize(G4HCofThisEvent*);
	virtual G4bool ProcessHits(G4Step*,G4TouchableHistory*);
	virtual void EndOfEvent(G4HCofThisEvent*);

	virtual void SetDetectorType(G4String det_type) {
          if (det_type.compareTo("charged",G4String::ignoreCase) == 0) {
            G4cout << SensitiveDetectorName << " detects charged particles" << G4endl;
            fDetectOpticalPhotons = false;
            fDetectLowEnergyNeutrals = false;
          }
	  if (det_type.compareTo("lowenergyneutral",G4String::ignoreCase) == 0) {
            G4cout << SensitiveDetectorName << " detects low energy neutrals" << G4endl;
	    fDetectLowEnergyNeutrals = true;
	  }
          if (det_type.compareTo("opticalphoton",G4String::ignoreCase) == 0) {
            G4cout << SensitiveDetectorName << " detects optical photons" << G4endl;
            fDetectOpticalPhotons = true;
            fDetectLowEnergyNeutrals = true;
          }
	}

        void SetEnabled(G4bool flag = true) {
          fEnabled = flag; PrintEnabled();
        };
        void SetDisabled(G4bool flag = true) {
          fEnabled = !flag; PrintEnabled();
        };
        void PrintEnabled() const {
          G4cout << "Det " << GetName() << " (" << fDetNo << ") "
                 << (fEnabled? "enabled" : "disabled") << G4endl;
        };

    private:
	remollGenericDetectorHitCollection *fHitColl;
	remollGenericDetectorSumCollection *fSumColl;

	G4int fHCID, fSCID;

	std::map<int, remollGenericDetectorSum *> fSumMap;

        G4bool fDetectSecondaries;
	G4bool fDetectOpticalPhotons;
        G4bool fDetectLowEnergyNeutrals;

	G4int fDetNo;

	G4bool fEnabled;

	G4GenericMessenger* fMessenger;

};

#endif//__REMOLLGENERICDETECTOR_HH
