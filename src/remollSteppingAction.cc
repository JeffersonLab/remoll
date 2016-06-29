#include "remollSteppingAction.hh"
//#include "remollSteppingActionMessenger.hh"

#include "G4VVisManager.hh"
#include "G4Polyline.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"
#include "G4SteppingManager.hh"
#include "G4Event.hh"
#include <iostream>
#include <fstream>
using namespace std;

ofstream myfile1;
ofstream myfile2;
ofstream myfile3;
ofstream myfile4;
ofstream myfile5;
ofstream myfile6;
ofstream myfile7;
ofstream myfile8;
ofstream myfile9;

remollSteppingAction::remollSteppingAction()
:drawFlag(false)
{
///  new remollSteppingActionMessenger(this);

    fEnableKryptonite = true;
}


void remollSteppingAction::UserSteppingAction(const G4Step *aStep) {
    
    G4Track* fTrack = aStep->GetTrack();
    G4Material* material = fTrack->GetMaterial();

    myfile1.open ("parent_position_output.txt", ios::app);
    myfile2.open ("1st_daughter_position_output.txt", ios::app);
    myfile3.open ("2nd_daughter_position_output.txt", ios::app);
    myfile4.open ("3rd_daughter_position_output.txt", ios::app);
    myfile5.open ("4th_daughter_position_output.txt", ios::app);
    myfile6.open ("5th_daughter_position_output.txt", ios::app);
    myfile7.open ("6th_daughter_position_output.txt", ios::app);
    myfile8.open ("7th_daughter_position_output.txt", ios::app);
    myfile9.open ("8th_daughter_position_output.txt", ios::app);
  


    if (fTrack->GetTrackID() == 1) {
    	G4ThreeVector pos = fTrack->GetPosition();
	double xPos = pos.getX();
	double yPos = pos.getY();
	double zPos = pos.getZ();
//	double magP = fTrack->GetMomentum().mag()/GeV;
//	double ke = fTrack->GetKineticEnergy()/GeV;
//	myfile << xPos << "     \t" << yPos << "     \t" << zPos << "     \t" <<  magP << "     \t" << ke << " \n"; // evNum << std::endl;  
	myfile1 << xPos << "\t" << yPos << "\t" << zPos << " \n"; // evNum << std::endl;
  }

    if (fTrack->GetTrackID() == 2) {
    	G4ThreeVector pos = fTrack->GetPosition();
	double xPos = pos.getX();
	double yPos = pos.getY();
	double zPos = pos.getZ();
	myfile2 << xPos << "\t" << yPos << "\t" << zPos << " \n"; 
  }

    if (fTrack->GetTrackID() == 3) {
    	G4ThreeVector pos = fTrack->GetPosition();
	double xPos = pos.getX();
	double yPos = pos.getY();
	double zPos = pos.getZ();
	myfile3 << xPos << "\t" << yPos << "\t" << zPos << " \n"; 
  }

    if (fTrack->GetTrackID() == 4) {
    	G4ThreeVector pos = fTrack->GetPosition();
	double xPos = pos.getX();
	double yPos = pos.getY();
	double zPos = pos.getZ();
	myfile4 << xPos << "\t" << yPos << "\t" << zPos << " \n"; 
  }

    if (fTrack->GetTrackID() == 5) {
    	G4ThreeVector pos = fTrack->GetPosition();
	double xPos = pos.getX();
	double yPos = pos.getY();
	double zPos = pos.getZ();
	myfile5 << xPos << "\t" << yPos << "\t" << zPos << " \n"; 
  }

    if (fTrack->GetTrackID() == 6) {
    	G4ThreeVector pos = fTrack->GetPosition();
	double xPos = pos.getX();
	double yPos = pos.getY();
	double zPos = pos.getZ();
	myfile6 << xPos << "\t" << yPos << "\t" << zPos << " \n"; 
  }

    if (fTrack->GetTrackID() == 7) {
    	G4ThreeVector pos = fTrack->GetPosition();
	double xPos = pos.getX();
	double yPos = pos.getY();
	double zPos = pos.getZ();
	myfile7 << xPos << "\t" << yPos << "\t" << zPos << " \n"; 
  }

    if (fTrack->GetTrackID() == 8) {
    	G4ThreeVector pos = fTrack->GetPosition();
	double xPos = pos.getX();
	double yPos = pos.getY();
	double zPos = pos.getZ();
	myfile8 << xPos << "\t" << yPos << "\t" << zPos << " \n"; 
  }

    if (fTrack->GetTrackID() == 9) {
    	G4ThreeVector pos = fTrack->GetPosition();
	double xPos = pos.getX();
	double yPos = pos.getY();
	double zPos = pos.getZ();
	myfile9 << xPos << "\t" << yPos << "\t" << zPos << " \n"; 
  }


    // Don't continue in these materials
    

   if( (   material->GetName()=="Tungsten" 
        ||  material->GetName()=="Pb"
	||  material->GetName()=="Copper" )
	    && fEnableKryptonite
	){
	fTrack->SetTrackStatus(fKillTrackAndSecondaries);
    }


   myfile1.close();
   myfile2.close();
   myfile3.close();
   myfile4.close();
   myfile5.close();
   myfile6.close();
   myfile7.close();
   myfile8.close();
   myfile9.close();
}



