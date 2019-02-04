#ifndef remollTrackingAction_h
#define remollTrackingAction_h 1

// geant4 includes
#include "G4Types.hh"
#include "G4UserTrackingAction.hh"

// forward declares
class G4GenericMessenger;

class remollTrackingAction : public G4UserTrackingAction
{
  public:
    remollTrackingAction();
    virtual ~remollTrackingAction();

    void  PreUserTrackingAction(const G4Track* aTrack);
    void PostUserTrackingAction(const G4Track* aTrack);

  private:
    G4GenericMessenger* fMessenger;
    G4int fTrackingFlag;
};

#endif
