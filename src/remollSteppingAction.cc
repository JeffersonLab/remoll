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

ofstream myfile;

remollSteppingAction::remollSteppingAction()
:drawFlag(false)
{
///  new remollSteppingActionMessenger(this);

    fEnableKryptonite = true;
}


void remollSteppingAction::UserSteppingAction(const G4Step *aStep) {
    
    G4Track* fTrack = aStep->GetTrack();
    G4Material* material = fTrack->GetMaterial();

    myfile.open ("position_output.txt", ios::app);
  

    if (fTrack->GetTrackID() == 1) {
    	G4ThreeVector pos = fTrack->GetPosition();
	double xPos = pos.getX();
	double yPos = pos.getY();
	double zPos = pos.getZ();
//	double magP = fTrack->GetMomentum().mag()/GeV;
//	double ke = fTrack->GetKineticEnergy()/GeV;
//	myfile << xPos << "     \t" << yPos << "     \t" << zPos << "     \t" <<  magP << "     \t" << ke << " \n"; // evNum << std::endl;  
	myfile << xPos << "\t" << yPos << "\t" << zPos << " \n"; // evNum << std::endl;
  
  }

    // Don't continue in these materials
    

   if( (   material->GetName()=="Tungsten" 
        ||  material->GetName()=="Pb"
	||  material->GetName()=="Copper" )
	    && fEnableKryptonite
	){
	fTrack->SetTrackStatus(fKillTrackAndSecondaries);
    }


   myfile.close();
}



