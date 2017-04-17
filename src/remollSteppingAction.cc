#include "remollSteppingAction.hh"
//#include "remollSteppingActionMessenger.hh"
#include "remollVUserTrackInformation.hh"


#include "G4VVisManager.hh"
#include "G4Polyline.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"
#include "G4SteppingManager.hh"
#include "G4PhysicalConstants.hh"
#include "G4SystemOfUnits.hh" // be sure to get the units right...
#include "G4Event.hh"
#include "remollEvent.hh"
#include "remollIO.hh"
#include <math.h>

remollSteppingAction::remollSteppingAction()
:drawFlag(false)
{
///  new remollSteppingActionMessenger(this);

  fEnableKryptonite = true;
}

void remollSteppingAction::UserSteppingAction(const G4Step *aStep) {

  G4StepPoint *prestep = aStep->GetPreStepPoint();
  G4StepPoint *poststep = aStep->GetPostStepPoint();
  G4Track* fTrack = aStep->GetTrack();
  //G4Material* material = fTrack->GetMaterial();
  G4String material = fTrack->GetMaterial()->GetName();


  if( ( material != "VacuumColl" ) && ( material != "VacuumTarg" ) && ( material != "Vacuum" ) && ( material != "Air" ) ) {

  //G4cout << "material = " << material->GetName() << G4endl;
    //G4int id = fTrack->GetTrackID(); // NEW Just for showing textoutput
    //G4double mass = fTrack->GetDefinition()->GetPDGMass();

    // Check the last momentum against the current momentum
    G4ThreeVector mom_direction = poststep->GetMomentumDirection(); //G4ThreeVector mom_direction = fTrack->GetMomentumDirection();
    G4ThreeVector old_direction = prestep->GetMomentumDirection(); //(fTrack->GetMomentum() - aStep->GetDeltaMomentum())/GeV;
      
    G4double deltaEnergy = aStep->GetDeltaEnergy()/GeV;
    G4double deltaEnergyDep = aStep->GetTotalEnergyDeposit()/GeV;
    G4double deltaAngle = mom_direction.theta(old_direction)/deg;
      
    //G4cout << G4endl << "Current delta E and Th variables, and position " << G4endl << G4endl;
    //G4cout << deltaEnergy << " = E[" << id << "]/" << __E_UNIT << G4endl;
    //G4cout << deltaEnergyDep << " = EDep[" << id << "]/" << __E_UNIT << G4endl;
    //G4cout << deltaAngle << " = Th[" << id << "]/" << __ANG_UNIT << G4endl;
    //G4cout << fTrack->GetPosition() << " = Position[" << id << "]/" << __L_UNIT << G4endl;

    /*
    G4cout << "step track id = " << id << G4endl;
    */

    // IF Statements dealing with whether these delta E and Angle are sufficient to warrant storing the current position and deltas in temporary storage for the IO to pick up or get replaced further along in the steppingAction.
    // Make these cuts dynamical and determined by macros
    //if( (abs(deltaEnergy) > 0.001) && (deltaAngle > 0.001) ) { // Consider adding in material based cuts as well
    //G4cout << "fAbs(deltaEnergy) = " << fabs(deltaEnergy) << ", and fAbs(deltaAngle) = " << fabs(deltaAngle) << G4endl; 
    if( (fabs(deltaEnergy) >= 0.001) && (fabs(deltaAngle) >= 0.001) ) { // Consider adding in material based cuts as well
    /*G4cout << G4endl << "Significant change detected: " << G4endl << G4endl;
      G4cout << deltaEnergy << " = fPartDeltaE[" << id << "]" << " GeV" << G4endl;
      G4cout << deltaEnergyDep << " = fPartDeltaEDep[" << id << "]" << " GeV" << G4endl;
      G4cout << deltaAngle << " = fPartDeltaTh[" << id << "]" << " degrees" << G4endl;
      G4cout << fTrack->GetPosition().x() << " = fPartLastPos.x()[" << id << "]" << " millimeters" << G4endl;
      G4cout << fTrack->GetPosition().y() << " = fPartLastPos.y()[" << id << "]" << " millimeters" << G4endl;
      G4cout << fTrack->GetPosition().z() << " = fPartLastPos.z()[" << id << "]" << " millimeters" << G4endl;
      */

      remollVUserTrackInformation* info = (remollVUserTrackInformation*)(fTrack->GetUserInformation());
      info->SetLastSigVert( deltaEnergy, deltaEnergyDep, deltaAngle, fTrack->GetPosition() );
    }
  }
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
