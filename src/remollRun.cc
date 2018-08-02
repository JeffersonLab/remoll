#include "remollRun.hh"

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
