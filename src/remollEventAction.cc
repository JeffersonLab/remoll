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

ofstream Secondmyfile1;
ofstream Secondmyfile2;
ofstream Secondmyfile3;
ofstream Secondmyfile4;
ofstream Secondmyfile5;
ofstream Secondmyfile6;
ofstream Secondmyfile7;
ofstream Secondmyfile8;
ofstream Secondmyfile9;

remollEventAction::remollEventAction() {
}

remollEventAction::~remollEventAction(){
}


void remollEventAction::BeginOfEventAction(const G4Event*ev) {
    // Pretty ongoing status with flush

    // Start timer at event 0
    if (ev->GetEventID() == 0) fTimer.Start();
    // Pretty ongoing status

    Secondmyfile1.open ("parent_position_output.txt", ios::app);
    Secondmyfile1 << ev->GetEventID()+1 << " \n";
    Secondmyfile1.close();
    Secondmyfile2.open ("1st_daughter_position_output.txt", ios::app);
    Secondmyfile2 << ev->GetEventID()+1 << " \n";
    Secondmyfile2.close();
    Secondmyfile3.open ("2nd_daughter_position_output.txt", ios::app);
    Secondmyfile3 << ev->GetEventID()+1 << " \n";
    Secondmyfile3.close();
    Secondmyfile4.open ("3rd_daughter_position_output.txt", ios::app);
    Secondmyfile4 << ev->GetEventID()+1 << " \n";
    Secondmyfile4.close();
    Secondmyfile5.open ("4th_daughter_position_output.txt", ios::app);
    Secondmyfile5 << ev->GetEventID()+1 << " \n";
    Secondmyfile5.close();
    Secondmyfile6.open ("5th_daughter_position_output.txt", ios::app);
    Secondmyfile6 << ev->GetEventID()+1 << " \n";
    Secondmyfile6.close();
    Secondmyfile7.open ("6th_daughter_position_output.txt", ios::app);
    Secondmyfile7 << ev->GetEventID()+1 << " \n";
    Secondmyfile7.close();
    Secondmyfile8.open ("7th_daughter_position_output.txt", ios::app);
    Secondmyfile8 << ev->GetEventID()+1 << " \n";
    Secondmyfile8.close();
    Secondmyfile9.open ("8th_daughter_position_output.txt", ios::app);
    Secondmyfile9 << ev->GetEventID()+1 << " \n";
    Secondmyfile9.close();
     
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

    return;
}

void remollEventAction::EndOfEventAction(const G4Event* evt ) {
  //G4SDManager   *SDman = G4SDManager::GetSDMpointer();
  G4HCofThisEvent *HCE = evt->GetHCofThisEvent();
  int goodParticle = 0;	
  int collimatorHit = 0;
  int generation = 0;
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
			
			generation = thisHit->fTrID; //store the number corresponding to the generation of the particle. 1 = parent, 2 = 1st daughter particle, etc.
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

	Secondmyfile1.open ("parent_position_output.txt", ios::app);
	// if (goodParticle == 1 && collimatorHit == 1) {  // Use collimatorHit==1 if we want to ensure that Moller scattered particles going through the acceptance defining collimator are the only things getting through.
	if (goodParticle == 1 && generation == 1 ) {  
	//if (detectorHit == 1) {
//		Secondmyfile1 << -29 << "     \t" << -29 << "     \t" << -29 << "     \t" << -29 << "     \t" << -29 << "\n";
		Secondmyfile1 << -29 << "\t" << -29 << "\t" << -29 << "\n";
		Secondmyfile1 << finalEnergy << "\t" << finalMomentum << "\t" << " \n";
	} else {
//		Secondmyfile1 << -37 << "     \t" << -37 << "     \t" << -37 << "     \t" << -37 << "     \t" << -37 << "\n";
		Secondmyfile1 << -37 << "\t" << -37 << "\t" << -37 << "\n";
//		Secondmyfile1 << finalEnergy << "     \t" << finalMomentum << "     \t" << " \n";
//		-37 means bad: -29 means good particle.
	}
	Secondmyfile1.close();

	Secondmyfile2.open ("1st_daughter_position_output.txt", ios::app);
	if (goodParticle == 1 && generation == 2) {  
		Secondmyfile2 << -29 << "\t" << -29 << "\t" << -29 << "\n";
		Secondmyfile2 << finalEnergy << "\t" << finalMomentum << "\t" << " \n";
	} else {
		Secondmyfile2 << -37 << "\t" << -37 << "\t" << -37 << "\n";
	}
	Secondmyfile2.close();

	Secondmyfile3.open ("2nd_daughter_position_output.txt", ios::app);
	if (goodParticle == 1 && generation == 3) {  
		Secondmyfile3 << -29 << "\t" << -29 << "\t" << -29 << "\n";
		Secondmyfile3 << finalEnergy << "\t" << finalMomentum << "\t" << " \n";
	} else {
		Secondmyfile3 << -37 << "\t" << -37 << "\t" << -37 << "\n";
	}
	Secondmyfile3.close();
	
	Secondmyfile4.open ("3rd_daughter_position_output.txt", ios::app);
	if (goodParticle == 1 && generation == 4) {  
		Secondmyfile4 << -29 << "\t" << -29 << "\t" << -29 << "\n";
		Secondmyfile4 << finalEnergy << "\t" << finalMomentum << "\t" << " \n";
	} else {
		Secondmyfile4 << -37 << "\t" << -37 << "\t" << -37 << "\n";
	}
	Secondmyfile4.close();

	Secondmyfile5.open ("4th_daughter_position_output.txt", ios::app);
	if (goodParticle == 1 && generation == 5) {  
		Secondmyfile5 << -29 << "\t" << -29 << "\t" << -29 << "\n";
		Secondmyfile5 << finalEnergy << "\t" << finalMomentum << "\t" << " \n";
	} else {
		Secondmyfile5 << -37 << "\t" << -37 << "\t" << -37 << "\n";
	}
	Secondmyfile5.close();

	Secondmyfile6.open ("5th_daughter_position_output.txt", ios::app);
	if (goodParticle == 1 && generation == 6) {  
		Secondmyfile6 << -29 << "\t" << -29 << "\t" << -29 << "\n";
		Secondmyfile6 << finalEnergy << "\t" << finalMomentum << "\t" << " \n";
	} else {
		Secondmyfile6 << -37 << "\t" << -37 << "\t" << -37 << "\n";
	}
	Secondmyfile6.close();

	Secondmyfile7.open ("6th_daughter_position_output.txt", ios::app);
	if (goodParticle == 1 && generation == 7) {  
		Secondmyfile7 << -29 << "\t" << -29 << "\t" << -29 << "\n";
		Secondmyfile7 << finalEnergy << "\t" << finalMomentum << "\t" << " \n";
	} else {
		Secondmyfile7 << -37 << "\t" << -37 << "\t" << -37 << "\n";
	}
	Secondmyfile7.close();

	Secondmyfile8.open ("7th_daughter_position_output.txt", ios::app);
	if (goodParticle == 1 && generation == 8) {  
		Secondmyfile8 << -29 << "\t" << -29 << "\t" << -29 << "\n";
		Secondmyfile8 << finalEnergy << "\t" << finalMomentum << "\t" << " \n";
	} else {
		Secondmyfile8 << -37 << "\t" << -37 << "\t" << -37 << "\n";
	}
	Secondmyfile8.close();

	Secondmyfile9.open ("8th_daughter_position_output.txt", ios::app);
	if (goodParticle == 1 && generation == 9) {  
		Secondmyfile9 << -29 << "\t" << -29 << "\t" << -29 << "\n";
		Secondmyfile9 << finalEnergy << "\t" << finalMomentum << "\t" << " \n";
	} else {
		Secondmyfile9 << -37 << "\t" << -37 << "\t" << -37 << "\n";
	}
	Secondmyfile9.close();


  return;
}
