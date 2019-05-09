#include "remollGenericDetectorSum.hh"

G4ThreadLocal G4Allocator<remollGenericDetectorSum>* remollGenericDetectorSumAllocator = 0;

remollGenericDetectorSum::remollGenericDetectorSum(int detid, int copyid)
: fDetID(detid),fCopyID(copyid),fEdep(0.0),fNhit(0) { }

remollGenericDetectorSum::~remollGenericDetectorSum() { }

void remollGenericDetectorSum::AddEDep(int pid, G4ThreeVector pos, double edep)
{
  fNhit += 1;
  fEdep += edep;

  if (fSumByPID.count(pid) == 0)
    fSumByPID[pid] = { 0 };

  G4double oldedep = fSumByPID[pid].edep;
  fSumByPID[pid].edep += edep;
  fSumByPID[pid].n++;
  G4double newedep = fSumByPID[pid].edep;

  if (newedep > 0.0) { // avoid division by zero for first hit with zero edep
    fSumByPID[pid].x = (oldedep * fSumByPID[pid].x + edep * pos.x()) / newedep;
    fSumByPID[pid].y = (oldedep * fSumByPID[pid].y + edep * pos.y()) / newedep;
    fSumByPID[pid].z = (oldedep * fSumByPID[pid].z + edep * pos.z()) / newedep;
  }
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
