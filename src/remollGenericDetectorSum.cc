#include "remollGenericDetectorSum.hh"

G4Allocator<remollGenericDetectorSum> remollGenericDetectorSumAllocator;

remollGenericDetectorSum::remollGenericDetectorSum(){
}

remollGenericDetectorSum::~remollGenericDetectorSum(){
}

remollGenericDetectorSum::remollGenericDetectorSum(const remollGenericDetectorSum &right) : G4VHit(){
    // copy constructor
}

const remollGenericDetectorSum& remollGenericDetectorSum::operator =(const remollGenericDetectorSum &right){
    (*this) = right;
    return *this;
}

G4int remollGenericDetectorSum::operator==(const remollGenericDetectorSum &right ) const {
    return (this==&right) ? 1 : 0;
}
