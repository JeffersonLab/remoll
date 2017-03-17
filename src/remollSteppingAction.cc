#include "remollSteppingAction.hh"
//#include "remollSteppingActionMessenger.hh"

#include "G4VVisManager.hh"
#include "G4Polyline.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"
#include "G4SteppingManager.hh"
#include "G4PhysicalConstants.hh"
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
    G4Track* fTrack = aStep->GetTrack();
    G4Material* material = fTrack->GetMaterial();
    G4double mass = fTrack->GetDefinition()->GetPDGMass();


    // Check the last momentum against the current momentum
    G4ThreeVector mom = fTrack->GetMomentum();
    
    double deltaAngle;
    double deltaEnergy;

    deltaEnergy = abs(sqrt((mass*mass)+(mom.mag()*mom.mag()))-(sqrt((mass*mass)+(fPartLastMom().mag()*fPartLastMom().mag()))));
    deltaAngle = acos(((mom.x()*fPartLastMom.x())+(mom.y()*fPartLastMom.y())+(mom.z()*fPartLastMom.z()))/(mom().mag()*fPartLastMom().mag()))*(180./pi);

    // IF Statements dealing with whether these delta E and Angle are sufficient to warrant storing the current position and deltas in temporary storage for the IO to pick up or get replaced further along in the steppingAction.
    // Make these cuts dynamical and determined by macros
    if( (deltaEnergy > 0.01) && (deltaAngle > 1.0) ) { // Consider adding in material based cuts as well
	fPartDeltaE = deltaEnergy;
	fPartDeltaTh = deltaAngle;
	fPartLastPos = fTrack->GetPosition();
    }

    // Update the lastMomentum vector to be the current momentum at the end of our step evaluation
    fPartLastMom = mom; // make sure this equivalence is ok.

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


