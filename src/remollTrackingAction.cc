#include "remollTrackingAction.hh"

// remoll includes
#include "remollUserTrackInformation.hh"

// geant4 includes
#include "G4Version.hh"
#include "G4TrackingManager.hh"
#include "G4OpticalPhoton.hh"
#include "G4GenericMessenger.hh"

remollTrackingAction::remollTrackingAction()
: fMessenger(0),fTrackingFlag(3)
{
  fMessenger = new G4GenericMessenger(this,"/remoll/tracking/","Remoll tracking properties");
  fMessenger->DeclareProperty("set",fTrackingFlag)
    .SetGuidance("Select tracking flag")
    .SetGuidance(" 0 : Track primary electrons only")
    .SetGuidance(" 1 : Track primary electrons and optical photons only")
    .SetGuidance(" 2 : Track all particles except optical photons")
    .SetGuidance(" 3 : Track all particles")
    .SetParameterName("flag",false)
    .SetRange("flag >=0 && flag <= 3")
    .SetStates(G4State_PreInit,G4State_Idle);
}

remollTrackingAction::~remollTrackingAction()
{
  delete fMessenger;
}

void remollTrackingAction::PreUserTrackingAction(const G4Track* aTrack)
{
  G4VUserTrackInformation* usertrackinfo = aTrack->GetUserInformation();
  if (! usertrackinfo) {
    #if G4VERSION_NUMBER >= 1030
    aTrack->SetUserInformation(new remollUserTrackInformation());
    #else
    const_cast<G4Track*>(aTrack)->SetUserInformation(new remollUserTrackInformation());
    #endif
  }

  // Track primary electron only
  if (fTrackingFlag == 0) {
    if (aTrack->GetParentID() == 0) {
    } else {
        fpTrackingManager->EventAborted();
      return;
    }
  }

  // Track primary electron and optical photon
  else if (fTrackingFlag == 1) {
    if ((aTrack->GetParentID() == 0) || (aTrack->GetDefinition() == G4OpticalPhoton::OpticalPhotonDefinition()))
    {
    } else {
      fpTrackingManager->EventAborted();
      return;
    }
  }

  // Track primary electron and secondaries except optical photon
  else if (fTrackingFlag == 2) {
    if (aTrack->GetDefinition() != G4OpticalPhoton::OpticalPhotonDefinition())
    {
    } else {
      fpTrackingManager->EventAborted();
      return;
    }
  }

  // Track all particles
  else {
  }
}

void remollTrackingAction::PostUserTrackingAction(const G4Track* aTrack)
{
  G4VUserTrackInformation* usertrackinfo = aTrack->GetUserInformation();
  if (usertrackinfo) {
    remollUserTrackInformation* remollusertrackinfo =
        dynamic_cast<remollUserTrackInformation*>(usertrackinfo);
    if (remollusertrackinfo) {
      G4StepStatus stepstatus = aTrack->GetStep()->GetPostStepPoint()->GetStepStatus();
      remollusertrackinfo->SetStepStatus(stepstatus);
    }
  }
}
