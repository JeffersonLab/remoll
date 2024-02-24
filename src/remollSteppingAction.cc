#include "remollSteppingAction.hh"

#include "G4Step.hh"
#include "G4Track.hh"
#include "G4Material.hh"
#include "G4MaterialTable.hh"

remollSteppingAction::remollSteppingAction()
{
}

remollSteppingAction::~remollSteppingAction()
{
}

void remollSteppingAction::UserSteppingAction(const G4Step *aStep)
{
  G4StepPoint*          thePrePoint  = aStep->GetPreStepPoint();
  G4VPhysicalVolume*    thePrePV     = thePrePoint->GetPhysicalVolume();
  G4StepPoint*          thePostPoint = aStep->GetPostStepPoint();
  G4VPhysicalVolume*    thePostPV    = thePostPoint->GetPhysicalVolume();

  if(!thePostPV || !thePrePV)	return;
      
  //G4cout<<">>> "<<pv->GetName()<<G4endl;
  //std::cin.ignore();
  if (((thePrePV->GetName()).contains("acceptance") || (thePostPV->GetName()).contains("acceptance")))
    if (aStep->GetTrack()->GetKineticEnergy() < 2*CLHEP::GeV) 
      aStep->GetTrack()->SetTrackStatus(fStopAndKill);

}
