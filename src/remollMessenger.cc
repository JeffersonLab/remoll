#include "remollMessenger.hh"

#include "Randomize.hh"

#include "G4UIcmdWithAnInteger.hh"
#include "G4UIcmdWithAString.hh"
#include "G4UIcmdWithADoubleAndUnit.hh"
#include "G4UIcmdWithABool.hh"

#include "remollGlobalField.hh"
#include "remollOpticalPhysics.hh"
#include "remollDetectorConstruction.hh"
#include "remollIO.hh"
#include "remollEventAction.hh"
#include "remollVEventGen.hh"
#include "remollPrimaryGeneratorAction.hh"
#include "remollBeamTarget.hh"
#include "remollRun.hh"
#include "remollRunData.hh"
#include "remollGenPion.hh"
#include "remollGenFlat.hh"
#include "remollGenLUND.hh"

#include "G4UImanager.hh"
#include "G4VModularPhysicsList.hh"

#include "G4GDMLParser.hh"
#include "G4VPhysicalVolume.hh"

#include <iostream>

// Singleton
remollMessenger* remollMessenger::gInstance = 0;
remollMessenger* remollMessenger::GetInstance() {
  if (gInstance == 0) {
    gInstance = new remollMessenger();
  }
  return gInstance;
}

remollMessenger::remollMessenger()
{
  G4cout << "Creating remollMessenger object " << this << G4endl;

    /*  Initialize all the things it talks to to NULL */
    fPhysicsList  = NULL;

    seedCmd = new G4UIcmdWithAnInteger("/remoll/seed",this);
    seedCmd->SetGuidance("Set random engine seed");
    seedCmd->SetParameterName("seed", false);

    opticalCmd = new G4UIcmdWithABool("/remoll/optical",this);
    opticalCmd->SetGuidance("Enable optical physics");
    opticalCmd->SetParameterName("optical", false);
    opticalCmd->AvailableForStates(G4State_Idle);
}

remollMessenger::~remollMessenger() { }

void remollMessenger::SetNewValue(G4UIcommand* cmd, G4String newValue)
{
    if( cmd == seedCmd ){
	G4long seed = seedCmd->GetNewIntValue(newValue);
	G4Random::setTheSeed(seed);
	remollRun::UpdateSeed();
    }

    if( cmd == opticalCmd ){
	G4bool optical = opticalCmd->GetNewBoolValue(newValue);
	if( optical ){
	    fPhysicsList->RegisterPhysics( new remollOpticalPhysics() );
	} else {
	    fPhysicsList->RemovePhysics("Optical");
	}
    }
}
