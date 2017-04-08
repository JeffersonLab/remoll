#include "remollEventAction.hh"
#include "remollGenericDetectorHit.hh"
#include "remollGenericDetectorSum.hh"

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


remollEventAction::remollEventAction() {
}

remollEventAction::~remollEventAction(){
}


void remollEventAction::BeginOfEventAction(const G4Event* ev){
  // Start timer at event 0

  
  if (ev->GetEventID() == 0) fTimer.Start();
  // Pretty ongoing status
  if ((ev->GetEventID() % 1000) == 0) {
    // Stop timer (running timer cannot be read)
    fTimer.Stop();
    // Print event number
    G4cout << "Event " << ev->GetEventID();
    // Only print duration per event when meaningful (avoid division by zero)
    if (ev->GetEventID() > 0)
      G4cout << " (" << std::setprecision(3) << std::fixed
        << 1000.*fTimer.GetRealElapsed()/1000.0 << " ms/event)";
    // Carriage return without newline
    G4cout << "\r" << std::flush;
    // Start timer again
    fTimer.Start();
  }
}

void remollEventAction::EndOfEventAction(const G4Event* evt ) {
  //G4SDManager   *SDman = G4SDManager::GetSDMpointer();
  G4HCofThisEvent *HCE = evt->GetHCofThisEvent();

  G4VHitsCollection *thiscol;

  // Traverse all hit collections, sort by output type
  for( int hcidx = 0; hcidx < HCE->GetCapacity(); hcidx++ ){
    thiscol = HCE->GetHC(hcidx);
    if(thiscol){ // This is NULL if nothing is stored
      // Dyanmic cast to test types, process however see fit and feed to IO
      
      ////  Generic Detector Hits ///////////////////////////////////
      if( remollGenericDetectorHitsCollection *thiscast = 
	        dynamic_cast<remollGenericDetectorHitsCollection *>(thiscol)){
	      for( unsigned int hidx = 0; hidx < thiscast->GetSize(); hidx++ ){
	        fIO->AddGenericDetectorHit((remollGenericDetectorHit *) 
				      thiscast->GetHit(hidx) );	  
	      }
      }
      
      ////  Generic Detector Sum ////////////////////////////////////
      if( remollGenericDetectorSumCollection *thiscast = 
	         dynamic_cast<remollGenericDetectorSumCollection *>(thiscol)){
	      for( unsigned int hidx = 0; hidx < thiscast->GetSize(); hidx++ ){
	        fIO->AddGenericDetectorSum((remollGenericDetectorSum *) 
				      thiscast->GetHit(hidx) );
	      }
      } 
    }
  }

  // Fill tree and reset buffers
  fIO->FillTree(); // This is the key moment where the branches get appended too.
  fIO->Flush();

  return;
}
