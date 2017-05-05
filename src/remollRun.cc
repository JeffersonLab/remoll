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

void remollRun::RecordEvent(const G4Event* aEvent)
{
  G4Run::RecordEvent(aEvent);
}

void remollRun::Merge(const G4Run* aRun) {
  G4Run::Merge(aRun);
}
