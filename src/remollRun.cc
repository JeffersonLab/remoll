#include "remollRun.hh"

#include "Randomize.hh"
#include "G4Event.hh"
#include "G4HCofThisEvent.hh"

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

void remollRun::UpdateSeed()
{
  GetRunData()->SetSeed(G4Random::getTheSeed());
}

remollRun::remollRun() { }

remollRun::~remollRun() { }
