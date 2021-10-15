#ifndef remollTrackingAction_h
#define remollTrackingAction_h 1

// geant4 includes
#include "G4Types.hh"
#include "G4GenericMessenger.hh"
#include "G4UserTrackingAction.hh"

class remollTrackingAction : public G4UserTrackingAction
{
  public:
    remollTrackingAction();
    virtual ~remollTrackingAction() = default;

    void  PreUserTrackingAction(const G4Track* aTrack);
    void PostUserTrackingAction(const G4Track* aTrack);

  private:
    G4GenericMessenger fMessenger{
        this,
        "/remoll/tracking/",
        "Remoll tracking properties"};

    G4int fTrackingFlag{3};
};

#endif
