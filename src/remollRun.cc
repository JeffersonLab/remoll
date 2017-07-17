#include "remollRun.hh"

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

remollRun::remollRun() { }

remollRun::~remollRun() { }
