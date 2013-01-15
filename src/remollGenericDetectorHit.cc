#include "remollGenericDetectorHit.hh"

G4Allocator<remollGenericDetectorHit> remollGenericDetectorHitAllocator;

remollGenericDetectorHit::remollGenericDetectorHit(){
}

remollGenericDetectorHit::~remollGenericDetectorHit(){
}

remollGenericDetectorHit::remollGenericDetectorHit(const remollGenericDetectorHit &right) : G4VHit(){
    // copy constructor
}

const remollGenericDetectorHit& remollGenericDetectorHit::operator =(const remollGenericDetectorHit &right){
    (*this) = right;
    return *this;
}

G4int remollGenericDetectorHit::operator==(const remollGenericDetectorHit &right ) const {
    return (this==&right) ? 1 : 0;
}
