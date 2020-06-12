#include "remollGenericDetectorHit.hh"

#include "remollSystemOfUnits.hh"

G4ThreadLocal G4Allocator<remollGenericDetectorHit>* remollGenericDetectorHitAllocator = 0;

remollGenericDetectorHit::remollGenericDetectorHit(G4int det, G4int copy)
: fDetID(det),fCopyID(copy)
{
  f3X  = G4ThreeVector(-1e9, -1e9, -1e9);
  f3Xl = G4ThreeVector(-1e9, -1e9, -1e9);
  f3P  = G4ThreeVector(-1e9, -1e9, -1e9);
  f3Pl = G4ThreeVector(-1e9, -1e9, -1e9);
  f3S  = G4ThreeVector(-1e9, -1e9, -1e9);
  f3V  = G4ThreeVector(-1e9, -1e9, -1e9);

  fTime = 0.0;

  fP  = -1.0;
  fE  = -1.0;
  fM  = -1.0;
  fK  = -1.0;

  f3XRec = G4ThreeVector(-1e9, -1e9, -1e9);
  f3dPRec = G4ThreeVector(-1e9, -1e9, -1e9);
  fThRec = -1.0;

  f3dP = G4ThreeVector(-1e9, -1e9, -1e9); // direction

  fTrID  = -1;
  fPID   = (G4int) 1e9;
  fmTrID = -1;

  fGen   = 1;

  fEdep  = 0.0;
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
  f3Pl    = right.f3Pl;
  f3S     = right.f3S;
  f3V     = right.f3V;

  fTime   = right.fTime;

  fP      = right.fP;
  fE      = right.fE;
  fM      = right.fM;
  fK      = right.fK;

  f3XRec  = right.f3XRec;
  f3dPRec = right.f3dPRec;
  fThRec  = right.fThRec;

  f3dP     = right.f3dP; //direction

  fTrID   = right.fTrID;
  fPID    = right.fPID;
  fmTrID  = right.fmTrID;
  fGen    = right.fGen;

  fEdep   = right.fEdep;
}

const remollGenericDetectorHit& remollGenericDetectorHit::operator=(const remollGenericDetectorHit &right) {
  (*this) = right;
  return *this;
}

G4int remollGenericDetectorHit::operator==(const remollGenericDetectorHit &right) const {
  return (this==&right) ? 1 : 0;
}

void remollGenericDetectorHit::Print(){

  G4cout << "  det[" << fDetID << "] : Hit[" << fCopyID 
	 << "] : Trid " << fTrID 
         << " --- global (x,y,z) [mm] " 
         << f3X.x()/mm << ", " 
         << f3X.y()/mm << ", " 
         << f3X.z()/mm << G4endl;
}
