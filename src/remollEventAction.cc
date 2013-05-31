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
#include "remollTrackReconstruct.hh"

remollEventAction::remollEventAction() {
}

remollEventAction::~remollEventAction(){
}


void remollEventAction::BeginOfEventAction(const G4Event*ev) {
    // Pretty ongoing status with flush
    if( (ev->GetEventID() % 1000) == 0 ){
	printf("Event %8d\r", ev->GetEventID() );
	fflush(stdout);
    }

    return;
}

void remollEventAction::EndOfEventAction(const G4Event* evt ) {
  //G4SDManager   *SDman = G4SDManager::GetSDMpointer();
  G4HCofThisEvent *HCE = evt->GetHCofThisEvent();
  
  G4VHitsCollection *thiscol;

  /////////////////////
  // for track reconstruction 
  std::vector <remollTrackReconstruct*> rTrack;
  const G4int nGEMS=4;
  for(G4int i=0;i<nGEMS;i++)
    rTrack.push_back(new remollTrackReconstruct());
  /////////////////////
  
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
				     thiscast->GetHit(hidx));
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

      //////////////////////////////////////////////
      // now the track reconstruction -- rupesh, 17May, 2013
      ////  store the tracks to reconstruction vectors
      if( remollGenericDetectorHitsCollection *thiscast = 
	  dynamic_cast<remollGenericDetectorHitsCollection *>(thiscol)){
	for( unsigned int hidx = 0; hidx < thiscast->GetSize(); hidx++ ){
	  
	  remollGenericDetectorHit *currentHit = 
	    (remollGenericDetectorHit *) thiscast->GetHit(hidx);

	  if(currentHit->fDetID >= 501 && currentHit->fDetID <= 505){
	    //	    G4cout << "Det ID for hit: " <<currentHit->fDetID << G4endl;
	    rTrack[0]->AddHit(currentHit);
	  } else if(currentHit->fDetID >= 506 && currentHit->fDetID <= 510){
	    //	    G4cout << "Det ID for hit: " <<currentHit->fDetID << G4endl;
	    rTrack[1]->AddHit(currentHit);	    
	  } else if(currentHit->fDetID >= 511 && currentHit->fDetID <= 515){
	    //	    G4cout << "Det ID for hit: " <<currentHit->fDetID << G4endl;
	    rTrack[2]->AddHit(currentHit);	    
	  }else if(currentHit->fDetID >= 516 && currentHit->fDetID <= 520){
	    //	    G4cout << "Det ID for hit: " <<currentHit->fDetID << G4endl;
	    rTrack[3]->AddHit(currentHit);
	  }
	}
      }
      //////////////////////////////////////////////
    }
  }

  //////////////////////////////////////////////
  ////  reconstruct tracks, and store them into rootfile
  for(G4int i=0;i<nGEMS;i++){
    if(rTrack[i]->GetTrackHitSize()>0){ 

      //      G4cout <<"\n rTrack[" << i << "].size(): "<< rTrack[i]->GetTrackHitSize() << G4endl;    

      //      rTrack[i]->PrintOriginalTrackHitInfo();

      rTrack[i]->ReconstructTrack();

      //      rTrack[i]->PrintReconstructedTrackHitInfo();

      // now get the recontructed track vars to store it into rootfile
      std::vector<remollGenericDetectorHit*> rRecHit = rTrack[i]->GetReconstructedTrack();
      
      for(G4int j=0;j<rRecHit.size();j++)
	fIO->AddGenericDetectorHit((remollGenericDetectorHit *) rRecHit[j]);
    }
  }
  //////////////////////////////////////////////
  
  // Fill tree and reset buffers
  fIO->FillTree();
  fIO->Flush();
  
  return;
}



