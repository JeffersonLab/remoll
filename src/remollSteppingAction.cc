#include "remollSteppingAction.hh"
//#include "remollSteppingActionMessenger.hh"

#include "G4VVisManager.hh"
#include "G4Polyline.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"
#include "G4SteppingManager.hh"

remollSteppingAction::remollSteppingAction()
:drawFlag(false)
{
///  new remollSteppingActionMessenger(this);

    fEnableKryptonite = true;
}

void remollSteppingAction::UserSteppingAction(const G4Step *aStep) {
  G4Track* fTrack = aStep->GetTrack();
  G4Material* material = fTrack->GetMaterial();
  
  // // return if the track is dead
  // if(aStep->GetTrack()->GetTrackStatus()!=fAlive){
  //   fTrack->SetTrackStatus(fKillTrackAndSecondaries);
  //   G4cout << "** remollSteppingAction::UserSteppingAction(). Track in the geometry boundry. Terminating track... **" << G4endl;
  //   return;
  // };
  
  // Don't continue in these materials
  if( (   material->GetName()=="Tungsten" 
	  ||  material->GetName()=="Pb"
	  ||  material->GetName()=="Copper" )
      && fEnableKryptonite
      ){
    fTrack->SetTrackStatus(fKillTrackAndSecondaries);
  }
}
