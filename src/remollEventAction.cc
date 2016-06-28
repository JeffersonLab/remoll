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


#include "G4ParticleTable.hh"
#include "G4SystemOfUnits.hh"

#include "remollIO.hh"

#include <iostream>
#include <fstream>
using namespace std;

ofstream myfile2;

remollEventAction::remollEventAction() {
}

remollEventAction::~remollEventAction(){
}


void remollEventAction::BeginOfEventAction(const G4Event*ev) {
    // Pretty ongoing status with flush

    // Start timer at event 0
    if (ev->GetEventID() == 0) fTimer.Start();
    // Pretty ongoing status

    myfile2.open ("position_output.txt", ios::app);
    myfile2 << ev->GetEventID()+1 << " \n";
     
    if( (ev->GetEventID() % 1000) == 0 ){
	printf("Event %8d\r", ev->GetEventID() );
	fflush(stdout);
 
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

    myfile2.close();
    return;
}

void remollEventAction::EndOfEventAction(const G4Event* evt ) {
  //G4SDManager   *SDman = G4SDManager::GetSDMpointer();
  G4HCofThisEvent *HCE = evt->GetHCofThisEvent();
  int goodParticle = 0;	
  int collimatorHit = 0;
  double finalEnergy, finalMomentum;
  
  remollGenericDetectorHit *thisHit;
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
			thisHit = dynamic_cast<remollGenericDetectorHit *>(thiscast->GetHit(hidx));
//			if(thisHit->fTrID == 1 && thisHit->fDetID == 28 && thisHit->fP/__E_UNIT > 2.) {

//                      Detector number 28 is the placeholder moller ring. See Geometry folder for dimensions.
			// fTrID == 1 refers to the original parent particle. Peripheral tracking needs more.
			// if(thisHit->fTrID == 1 && thisHit->fDetID == 28) {
			if(thisHit->fDetID == 28) {
					goodParticle = 1;
					finalEnergy = thisHit->fE/__E_UNIT;
					finalMomentum = thisHit->fP/__E_UNIT;
//					myfile2.open ("position_output.txt", ios::app);
//					myfile2 << -29 << "     \t" << -29 << "     \t" << -29 << "\n";
//					myfile2.close();  
				
      			}
			// fTrID == 1 refers to the original parent particle. Peripheral tracking needs more.
			// if(thisHit->fTrID == 1 && thisHit->fDetID == 200) {
			if(thisHit->fDetID == 200) {
					collimatorHit = 1;
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
}

}
// Fill tree and reset buffers
  fIO->FillTree();
  fIO->Flush();

	myfile2.open ("position_output.txt", ios::app);
	if (goodParticle == 1) {  // && collimatorHit == 1) {  // Use collimatorHit==1 if we want to ensure that Moller scattered particles going through the acceptance defining collimator are the only things getting through.
	//if (detectorHit == 1) {
//		myfile2 << -29 << "     \t" << -29 << "     \t" << -29 << "     \t" << -29 << "     \t" << -29 << "\n";
		myfile2 << -29 << "\t" << -29 << "\t" << -29 << "\n";
		myfile2 << finalEnergy << "\t" << finalMomentum << "\t" << " \n";
	} else {
//		myfile2 << -37 << "     \t" << -37 << "     \t" << -37 << "     \t" << -37 << "     \t" << -37 << "\n";
		myfile2 << -37 << "\t" << -37 << "\t" << -37 << "\n";
//		myfile2 << finalEnergy << "     \t" << finalMomentum << "     \t" << " \n";
//		-37 means bad: -29 means good particle.
	}
	myfile2.close();

  return;
}
