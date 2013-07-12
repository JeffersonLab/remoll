#include "remollGenericDetectorHit.hh"

G4Allocator<remollGenericDetectorHit> remollGenericDetectorHitAllocator;

remollGenericDetectorHit::remollGenericDetectorHit(G4int det, G4int copy){
    fDetID  = det;
    fCopyID = copy;

    f3X = G4ThreeVector(-1e9, -1e9, -1e9);
    f3P = G4ThreeVector(-1e9, -1e9, -1e9);
    f3V = G4ThreeVector(-1e9, -1e9, -1e9);

    f3XRec = G4ThreeVector(-1e9, -1e9, -1e9);
    f3dPRec = G4ThreeVector(-1e9, -1e9, -1e9);
    fThRec = -1.0;

    f3dP = G4ThreeVector(-1e9, -1e9, -1e9); // direction

    fP  = -1.0;
    fE  = -1.0;
    fM  = -1.0;

    fTrID  = -1;
    fPID   = (G4int) 1e9;
    fmTrID = -1;

    fGen   = 1;
}

remollGenericDetectorHit::~remollGenericDetectorHit(){
}

remollGenericDetectorHit::remollGenericDetectorHit(const remollGenericDetectorHit &right) : G4VHit(){
    // copy constructor

    fDetID  = right.fDetID;
    fCopyID = right.fCopyID;
    f3X     = right.f3X;
    f3P     = right.f3P;
    f3V     = right.f3V;

    f3XRec  = right.f3XRec;
    f3dPRec = right.f3dPRec;
    fThRec  = right.fThRec;

    f3dP     = right.f3dP; //direction

    fP      = right.fP;
    fE      = right.fE;
    fM      = right.fM;

    fTrID   = right.fTrID;
    fPID    = right.fPID;
    fmTrID  = right.fmTrID;
    fGen    = right.fGen;
}

const remollGenericDetectorHit& remollGenericDetectorHit::operator =(const remollGenericDetectorHit &right){
    (*this) = right;
    return *this;
}

G4int remollGenericDetectorHit::operator==(const remollGenericDetectorHit &right ) const {
    return (this==&right) ? 1 : 0;
}


void remollGenericDetectorHit::Print(){

  G4cout << "  det[" << fDetID << "] : Hit[" << fCopyID 
         << "] --- global (x,y,z) [mm] " 
         << f3X.x()/mm << ", " 
         << f3X.y()/mm << ", " 
         << f3X.z()/mm << G4endl;
}
