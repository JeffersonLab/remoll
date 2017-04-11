#include "remollVTrackingAction.hh"
#include "G4TrackingManager.hh"
#include "G4Track.hh"
#include "G4TrackVector.hh"
#include "remollVUserTrackInformation.hh"

// Guide obtained from http://geant4.slac.stanford.edu/Tips/event/1.html

remollVTrackingAction::remollVTrackingAction()
{;}

remollVTrackingAction::~remollVTrackingAction()
{;}

void remollVTrackingAction::PreUserTrackingAction(const G4Track* aTrack)
{
  //if(aTrack->GetParentID()==0 && aTrack->GetUserInformation()==0)
  remollVUserTrackInformation* anInfo = new remollVUserTrackInformation();//aTrack);
  //G4cout << "creating new particle tracking info and setting its initial parameters to null values " << G4endl;
  //anInfo->SetLastSigVert( 0.0, 0.0, 0.0, ( 0.0, 0.0, 0.0 ) );
  G4Track* theTrack = (G4Track*)aTrack;
  theTrack->SetUserInformation(anInfo);
}

void remollVTrackingAction::PostUserTrackingAction(const G4Track* aTrack)
{
//  G4TrackVector* secondaries = fpTrackingManager->GimmeSecondaries();
//  if(secondaries)
// {
//    remollVUserTrackInformation* info = (remollVUserTrackInformation*)(aTrack->GetUserInformation());
//    size_t nSeco = secondaries->size();
//    if(nSeco>0)
//    {
//      for(size_t i=0;i<nSeco;i++)
//      { 
//        remollVUserTrackInformation* infoNew = new remollVUserTrackInformation(info);
//        (*secondaries)[i]->SetUserInformation(infoNew);
//      }
//    }
//  }
}
