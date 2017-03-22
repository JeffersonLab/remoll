#include "remollSteppingAction.hh"
//#include "remollSteppingActionMessenger.hh"

#include "G4VVisManager.hh"
#include "G4Polyline.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"
#include "G4SteppingManager.hh"
#include "G4PhysicalConstants.hh"
#include "G4SystemOfUnits.hh" // be sure to get the units right...
#include "G4Event.hh" // NEW
#include "remollEvent.hh" // The remollEvent.hh include the fPartLastMom definition
#include "remollIO.hh" // let the IO see stepping action
#include <math.h>

remollSteppingAction::remollSteppingAction()
:drawFlag(false)
{
///  new remollSteppingActionMessenger(this);

    fEnableKryptonite = true;
}

void remollSteppingAction::UserSteppingAction(const G4Step *aStep) {
 
    //G4int evID = fIO->IOGetEventID(); // NEW	
    //G4Event* ev = new G4Event(evID); // NEW
    remollEvent* ev = fIO->IOGetEvent();

    G4Track* fTrack = aStep->GetTrack();
    G4Material* material = fTrack->GetMaterial();
    
    //G4double mass = fTrack->GetDefinition()->GetPDGMass();


    // Check the last momentum against the current momentum
    G4ThreeVector mom_direction = fTrack->GetMomentumDirection();
    G4ThreeVector old_momentum = (fTrack->GetMomentum() - aStep->GetDeltaMomentum())/GeV;
    G4ThreeVector old_direction = old_momentum.unit();
    //G4ThreeVector old_direction = fPartLastMom.unit();
    
    //G4double old_energy = fPartOldEnergy;
    //G4double new_energy = fTrack->GetKineticEnergy()/GeV;

    G4double deltaEnergy = aStep->GetDeltaEnergy()/GeV;
    G4double deltaAngle = mom_direction.theta(old_direction)/deg;
    //G4double deltaEnergy = abs(new_energy - old_energy);

    // IF Statements dealing with whether these delta E and Angle are sufficient to warrant storing the current position and deltas in temporary storage for the IO to pick up or get replaced further along in the steppingAction.
    // Make these cuts dynamical and determined by macros
    if( (deltaEnergy > 0.0001) && (deltaAngle > 1.0) ) { // Consider adding in material based cuts as well
	G4int size = ev->fPartPos.size()-1;
        ev->fPartDeltaE[size] = deltaEnergy;
	ev->fPartDeltaTh[size] = deltaAngle;
	ev->fPartLastPos[size] = fTrack->GetPosition();
    }

    // Update the lastMomentum vector to be the current momentum at the end of our step evaluation
    //fPartLastMom = mom; // make sure this equivalence is ok.
    //fPartLastEnergy = new_energy;
    
    /////////////////
    
    
    
    
    /*
    //Now the stepping length is set to zero for Kryptonite metrial and have introduced a new mechanism to properly kill track by depositing the energy into the volume. 
    //Therefore we no longer needs to artificially kill tracks in the UserSteppingAction : Rakitha Tue Oct 14 10:32:32 EDT 2014

    /////////////////////////No Longer in Use//////////////////////////////

    // Don't continue in these materials
    if( (   material->GetName()=="Tungsten" 
        ||  material->GetName()=="Pb"
	||  material->GetName()=="Copper" )
	    && fEnableKryptonite
	){
      // fTrack->SetTrackStatus(fStopAndKill); // kill the current track
      fTrack->SetTrackStatus(fKillTrackAndSecondaries); // kill the current track and also associated secondaries
    }
    //stop and kill in Kryptonite materials : Rakitha Wed Sep 17 10:21:58 EDT 2014
    if (material->GetName()=="Kryptonite" ){
      fTrack->SetTrackStatus(fStopAndKill);
    }


    */

}


