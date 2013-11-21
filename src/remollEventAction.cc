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

  rTrack = new remollTrackReconstruct();
  
  // Traverse all hit collections, sort by output type
  for( int hcidx = 0; hcidx < HCE->GetCapacity(); hcidx++ ){
    thiscol = HCE->GetHC(hcidx);
    if(thiscol){ // This is NULL if nothing is stored
      // Dyanmic cast to test types, process however see fit and feed to IO

      Bool_t det200cut=1;
      Bool_t det201cut=1;

      ////  Generic Detector Hits ///////////////////////////////////
      if( remollGenericDetectorHitsCollection *thiscast = 
	  dynamic_cast<remollGenericDetectorHitsCollection *>(thiscol)){
	for( unsigned int hidx = 0; hidx < thiscast->GetSize(); hidx++ ){

	  remollGenericDetectorHit *currentHit = 
	    (remollGenericDetectorHit *) thiscast->GetHit(hidx);

	  //// flag hits from inside the inner radius of coll
	  //   these events need to be discarded
	  //   these could potentially end up in det, and contaminate data
	  if(currentHit->fDetID == 200 && currentHit->f3X.perp()/m < 0.03)
	    det200cut=0;
	  
	  if(currentHit->fDetID == 201 && currentHit->f3X.perp()/m < 0.05)
	    det201cut=0;
	}
      }

      ////  Generic Detector Hits ///////////////////////////////////
      if( remollGenericDetectorHitsCollection *thiscast = 
	  dynamic_cast<remollGenericDetectorHitsCollection *>(thiscol)){
	for( unsigned int hidx = 0; hidx < thiscast->GetSize(); hidx++ ){

	  remollGenericDetectorHit *currentHit = 
	    (remollGenericDetectorHit *) thiscast->GetHit(hidx);

	  if(det200cut && det201cut){
	    ////  store GEM hits for track reconstruction
	    if(currentHit->fDetID >= 501 && currentHit->fDetID <= 505){
	      rTrack->AddHit(currentHit);
	    }
	    // non-GEM hits
	    else fIO->AddGenericDetectorHit(currentHit);
	  }
	}
      }
      
      ////  Generic Detector Sum ////////////////////////////////////
      if( remollGenericDetectorSumCollection *thiscast = 
	  dynamic_cast<remollGenericDetectorSumCollection *>(thiscol)){
	for( unsigned int hidx = 0; hidx < thiscast->GetSize(); hidx++ ){
	  if(det200cut && det201cut)
	    fIO->AddGenericDetectorSum(
				     (remollGenericDetectorSum *) thiscast->GetHit(hidx) );
	}
      }
     
    }
  }

  ////  reconstruct tracks, and store them into rootfile
  if(rTrack->GetTrackHitSize()>0){ 
      
    rTrack->ReconstructTrack();

    std::vector<remollGenericDetectorHit*> rRecHit = rTrack->GetTrack();
      
    for(G4int j=0;j<rRecHit.size();j++)
      fIO->AddGenericDetectorHit((remollGenericDetectorHit *) rRecHit[j]);
  }
  
  delete rTrack;  

  // Fill tree and reset buffers
  fIO->FillTree();
  fIO->Flush();

  return;
}



