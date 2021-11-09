#include "remollRun.hh"

#include "remollRunData.hh"

remollRunData* remollRun::fRunData = 0;

remollRunData* remollRun::GetRunData()
{
  if (fRunData == nullptr) {
    fRunData = new remollRunData();
    fRunData->Init();
  }
  return fRunData;
}
