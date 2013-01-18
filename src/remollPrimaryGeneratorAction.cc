#include "remollPrimaryGeneratorAction.hh"

#include "G4Event.hh"
#include "G4ParticleGun.hh"
#include "G4ParticleTable.hh"
#include "G4ParticleDefinition.hh"
#include "remollIO.hh"
#include "remollVEventGen.hh"
#include "remollEvent.hh"
#include "remolltypes.hh"
#include "globals.hh"

remollPrimaryGeneratorAction::remollPrimaryGeneratorAction() {
  G4int n_particle = 1;
  fParticleGun = new G4ParticleGun(n_particle);


  fDefaultEvent = new remollEvent();
  fDefaultEvent->ProduceNewParticle( 
	  G4ThreeVector(0.*cm,0.*cm,-100.*cm),
	  G4ThreeVector(0.0,0.0,11.0*GeV),
	  "e-" );

  // Default generator data
  fParticleGun->SetParticleDefinition(fDefaultEvent->fPartType[0]);
  fParticleGun->SetParticleMomentumDirection(fDefaultEvent->fPartMom[0].unit());
  fParticleGun->SetParticleMomentum( fDefaultEvent->fPartMom[0].mag()  );
  fParticleGun->SetParticlePosition( fDefaultEvent->fPartPos[0] );

  fEventGen = NULL;
}

remollPrimaryGeneratorAction::~remollPrimaryGeneratorAction() {
  delete fParticleGun;
  delete fDefaultEvent;
}

void remollPrimaryGeneratorAction::GeneratePrimaries(G4Event* anEvent) {

  /*  Generate event, set IO data */

  remollEvent *thisev;
  if( fEventGen ){  // Specified our own generator
      thisev = fEventGen->GenerateEvent();
      for( unsigned int pidx = 0; pidx < thisev->fPartType.size(); pidx++ ){
	  fParticleGun->SetParticleDefinition(thisev->fPartType[pidx]);
	  fParticleGun->SetParticleMomentumDirection(thisev->fPartMom[pidx].unit());
	  fParticleGun->SetParticleMomentum( thisev->fPartMom[pidx].mag()  );
	  fParticleGun->SetParticlePosition( thisev->fPartPos[pidx] );
      }

      fIO->SetEventData(thisev);
  } else { // Use default, static single generator
      // Update this just in case things changed
      // from the command user interface
      fDefaultEvent->Reset();
      fDefaultEvent->ProduceNewParticle( 
	      fParticleGun->GetParticlePosition(),
	      fParticleGun->GetParticleMomentumDirection()*
	      fParticleGun->GetParticleMomentum(),
	      fParticleGun->GetParticleDefinition()->GetParticleName() );
      fIO->SetEventData(fDefaultEvent);
  }

  fParticleGun->GeneratePrimaryVertex(anEvent);
}

G4ParticleGun* remollPrimaryGeneratorAction::GetParticleGun() {
  return fParticleGun;
} 

