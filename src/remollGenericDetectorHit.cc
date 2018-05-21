#include "remollGenericDetectorHit.hh"

G4ThreadLocal G4Allocator<remollGenericDetectorHit>* remollGenericDetectorHitAllocator = 0;

remollGenericDetectorHit::remollGenericDetectorHit(G4int det, G4int copy)
: fDetID(det),fCopyID(copy)
{
  f3X = G4ThreeVector(-1e9, -1e9, -1e9);
  f3Xl = G4ThreeVector(-1e9, -1e9, -1e9);
  f3P = G4ThreeVector(-1e9, -1e9, -1e9);
  f3S = G4ThreeVector(-1e9, -1e9, -1e9);
  f3V = G4ThreeVector(-1e9, -1e9, -1e9);

  fTime = 0.0;

  fP  = -1.0;
  fE  = -1.0;
  fM  = -1.0;

  fTrID  = -1;
  fPID   = (G4int) 1e9;
  fmTrID = -1;

  fGen   = 1;
}

remollGenericDetectorHit::~remollGenericDetectorHit() { }

remollGenericDetectorHit::remollGenericDetectorHit(const remollGenericDetectorHit &right)
: G4VHit(right) {
  // copy constructor
  fDetID  = right.fDetID;
  fCopyID = right.fCopyID;
  f3X     = right.f3X;
  f3Xl    = right.f3Xl;
  f3P     = right.f3P;
  f3S     = right.f3S;
  f3V     = right.f3V;

  fTime   = right.fTime;

  fP      = right.fP;
  fE      = right.fE;
  fM      = right.fM;

  fTrID   = right.fTrID;
  fPID    = right.fPID;
  fmTrID  = right.fmTrID;
  fGen    = right.fGen;
}

const remollGenericDetectorHit& remollGenericDetectorHit::operator=(const remollGenericDetectorHit &right) {
  (*this) = right;
  return *this;
}

G4int remollGenericDetectorHit::operator==(const remollGenericDetectorHit &right) const {
  return (this==&right) ? 1 : 0;
}
