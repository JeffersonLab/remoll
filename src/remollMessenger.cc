#include "remollMessenger.hh"

#include "Randomize.hh"

#include "G4UIcmdWithAnInteger.hh"

#include "remollRun.hh"
#include "remollRunData.hh"

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
  seedCmd = new G4UIcmdWithAnInteger("/remoll/seed",this);
  seedCmd->SetGuidance("Set random engine seed");
  seedCmd->SetParameterName("seed", false);
}

remollMessenger::~remollMessenger() { }

void remollMessenger::SetNewValue(G4UIcommand* cmd, G4String newValue)
{
    if( cmd == seedCmd ){
	G4int seed = seedCmd->GetNewIntValue(newValue);
	G4Random::setTheSeed(seed);
	remollRun::GetRunData()->SetSeed(seed);
    }
}
