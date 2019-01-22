#include "remollGenericDetectorSum.hh"

G4ThreadLocal G4Allocator<remollGenericDetectorSum>* remollGenericDetectorSumAllocator = 0;

remollGenericDetectorSum::remollGenericDetectorSum(int detid, int copyid)
: fDetID(detid),fCopyID(copyid),fEdep(0.0) { }

remollGenericDetectorSum::~remollGenericDetectorSum() { }

void remollGenericDetectorSum::AddEDep(int pid, G4ThreeVector pos, double edep)
{
  fEdep += edep;

  if (fSumByPID.count(pid) == 0)
    fSumByPID.at(pid) = {.x = 0, .y = 0, .z = 0, .edep = 0, .pid = pid};

  G4double oldedep = fSumByPID[pid].edep;
  fSumByPID[pid].edep += edep;
  G4double newedep = fSumByPID[pid].edep;

  fSumByPID[pid].x = (oldedep * fSumByPID[pid].x + edep * pos.x()) / newedep;
  fSumByPID[pid].y = (oldedep * fSumByPID[pid].y + edep * pos.y()) / newedep;
  fSumByPID[pid].z = (oldedep * fSumByPID[pid].z + edep * pos.z()) / newedep;
}

remollGenericDetectorSum::remollGenericDetectorSum(const remollGenericDetectorSum &right)
: G4VHit(right) {
  // copy constructor
  fDetID  = right.fDetID;
  fCopyID = right.fCopyID;
  fEdep   = right.fEdep;
  fSumByPID   = right.fSumByPID;
}

const remollGenericDetectorSum& remollGenericDetectorSum::operator=(const remollGenericDetectorSum &right){
  (*this) = right;
  return *this;
}

G4int remollGenericDetectorSum::operator==(const remollGenericDetectorSum &right) const {
  return (this==&right) ? 1 : 0;
}
