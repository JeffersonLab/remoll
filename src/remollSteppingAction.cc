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
 

  G4Track* fTrack = aStep->GetTrack();
  G4Material* material = fTrack->GetMaterial();
  G4int id = fTrack->GetTrackID(); 
  //G4double mass = fTrack->GetDefinition()->GetPDGMass();


  // Check the last momentum against the current momentum
  G4ThreeVector mom_direction = fTrack->GetMomentumDirection();
  G4ThreeVector old_momentum = (fTrack->GetMomentum() - aStep->GetDeltaMomentum())/GeV;
  G4ThreeVector old_direction = old_momentum.unit();
  //G4ThreeVector old_direction = fPartLastMom.unit();
    
  //G4double old_energy = fPartOldEnergy;
  //G4double new_energy = fTrack->GetKineticEnergy()/GeV;

  G4double deltaEnergy = -1.0*aStep->GetDeltaEnergy()/GeV;
  G4double deltaAngle = mom_direction.theta(old_direction)/deg;
  //G4double deltaEnergy = abs(new_energy - old_energy);

  // IF Statements dealing with whether these delta E and Angle are sufficient to warrant storing the current position and deltas in temporary storage for the IO to pick up or get replaced further along in the steppingAction.
  // Make these cuts dynamical and determined by macros
  //G4cout << "deltaEnergy = " << deltaEnergy << G4endl;
  //G4cout << "deltaAngle = " << deltaAngle << G4endl;
  if( (abs(deltaEnergy) > 0.001) && (deltaAngle > 0.001) ) { // Consider adding in material based cuts as well
    G4cout << " test" << G4endl;
    remollEvent *evt = remollEvent::GetRemollEvent(); // mimic remollBeamTarget methods
	  G4cout << "Significant change detected: " << G4endl;
	  G4cout << deltaEnergy << " = fPartDeltaE[" << id << "]/" << __E_UNIT << G4endl;
	  G4cout << deltaAngle << " = fPartDeltaTh[" << id << "]/" << __ANG_UNIT << G4endl;
	  G4cout << fTrack->GetPosition().x() << " = fPartLastPos.x()[" << id << "]/" << __L_UNIT << G4endl;
	  G4cout << fTrack->GetPosition().y() << " = fPartLastPos.y()[" << id << "]/" << __L_UNIT << G4endl;
	  G4cout << fTrack->GetPosition().z() << " = fPartLastPos.z()[" << id << "]/" << __L_UNIT << G4endl;
    G4cout << " 2nd test" << G4endl;


// Update the hash tables to reflect changes:

// Change to unordered_map
// If the id isn't aready mapped then insert, else just modify interator pointer

// Declare an iterator to the unordered_map (fPartDeltaEMap)
std::unordered_map<G4int, G4double>::iterator it;
// Find if an element with key "id" exists or not.
// find() returns an iterator
it = evt->fPartDeltaEMap.find(id);
G4cout << " 3rd test" << G4endl;

// Check if the iterator points to the end of the map (meaning that it didn't find the key)
// CONSIDER some mechanism to prune out tracks that die
if ( it == evt->fPartDeltaEMap.end() ){ // Then add this track's info into the map
G4cout << "New track added, ID = " << id << G4endl;
evt->fPartDeltaEMap.insert( { id, deltaEnergy } );
	    evt->fPartDeltaThMap.insert( { id, deltaAngle } );
	    evt->fPartLastPosMap.insert( { id, fTrack->GetPosition() } );
      G4cout << " Append MAP test" << G4endl;
    }
    else { // just update the old info
      evt->fPartDeltaEMap[id] = deltaEnergy;
      evt->fPartDeltaThMap[id] = deltaAngle;
      evt->fPartLastPosMap[id] = fTrack->GetPosition();
      G4cout << " EDIT MAP test" << G4endl;
    }
	  //evt->UpdateLastParticle( fTrack->GetPosition(), deltaEnergy, deltaAngle );
    G4cout << " 4th test" << G4endl;
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
