#include "remollRun.hh"

#include "Randomize.hh"
#include "G4Event.hh"
#include "G4GenericMessenger.hh"

#include "remollRunData.hh"

remollRunData* remollRun::fRunData = 0;

remollRunData* remollRun::GetRunData()
{
  if (!fRunData) {
    fRunData = new remollRunData();
    fRunData->Init();
  }
  return fRunData;
}

void remollRun::UpdateSeed(G4long seed)
{
  GetRunData()->SetSeed(seed);
  G4Random::setTheSeed(seed);
  G4cout << "Random seed set to " << seed << G4endl;
}

remollRun::remollRun()
: G4Run()
{
  // Create messenger to set the seed with single long int
  fMessenger = new G4GenericMessenger(this,"/remoll/","Remoll properties");
  fMessenger->DeclareMethod(
      "seed",
      &remollRun::UpdateSeed,
      "Set random engine seed")
      .SetParameterName("seed", false)
      .SetStates(G4State_PreInit,G4State_Idle);
}

remollRun::~remollRun()
{
  delete fMessenger;
}
