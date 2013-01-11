#include "remollEventAction.hh"

#include "G4Event.hh"
#include "G4EventManager.hh"
#include "G4HCofThisEvent.hh"
#include "G4VHitsCollection.hh"
#include "G4TrajectoryContainer.hh"
#include "G4Trajectory.hh"
#include "G4VVisManager.hh"
#include "G4SDManager.hh"
#include "G4UImanager.hh"
#include "G4ios.hh"

#include "remollIO.hh"


remollEventAction::remollEventAction()
{

}

remollEventAction::~remollEventAction()
{;}


void remollEventAction::BeginOfEventAction(const G4Event*ev) {
    if( (ev->GetEventID() % 1000) == 0 ){
	printf("Event %8d\r", ev->GetEventID() );
	fflush(stdout);
    }

    return;
}

void remollEventAction::EndOfEventAction(const G4Event* evt )
{
  G4SDManager * SDman = G4SDManager::GetSDMpointer();
  G4String colNam;

  // Set IO data here
//  fIO->SetTrackData(trdata);
//  fIO->SetHitData(hitdata);
  fIO->FillTree();

  return;
}



