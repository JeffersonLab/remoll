#ifndef __REMOLLVTRACKINGACTION_HH
#define __REMOLLVTRACKINGACTION_HH

#include "remollVTrackingAction.hh"
#include "G4TrackingManager.hh"
#include "G4UserTrackingAction.hh"
#include "G4Track.hh"
#include "G4TrackVector.hh"
#include "remollVUserTrackInformation.hh"

// Guide obtained from http://geant4.slac.stanford.edu/Tips/event/1.html

class remollVTrackingAction : public G4UserTrackingAction {
  public:
    remollVTrackingAction();
   ~remollVTrackingAction();
    
    void PreUserTrackingAction(const G4Track* aTrack);
    void PostUserTrackingAction(const G4Track* aTrack);

};

#endif//__REMOLLVTRACKINGACTION_HH
