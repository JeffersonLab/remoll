#ifndef __REMOLLGENERICDETECTORSUM_HH
#define __REMOLLGENERICDETECTORSUM_HH

#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4Allocator.hh"

class remollGenericDetectorSum : public G4VHit {
    public:
	remollGenericDetectorSum();
	~remollGenericDetectorSum();

	remollGenericDetectorSum(const remollGenericDetectorSum &right);
	const remollGenericDetectorSum& operator=(const remollGenericDetectorSum &right);
	G4int operator==(const remollGenericDetectorSum &right) const;

	inline void *operator new(size_t);
	inline void operator delete(void *aHit);
	void *operator new(size_t,void*p){return p;}

    private:
	/* FIXME data */
};


typedef G4THitsCollection<remollGenericDetectorSum> remollGenericDetectorSumCollection;

extern G4Allocator<remollGenericDetectorSum> remollGenericDetectorSumAllocator;

inline void* remollGenericDetectorSum::operator new(size_t){
    void *aHit;
    aHit = (void *) remollGenericDetectorSumAllocator.MallocSingle();
    return aHit;
}

inline void remollGenericDetectorSum::operator delete(void *aHit){
    remollGenericDetectorSumAllocator.FreeSingle( (remollGenericDetectorSum*) aHit);
}

#endif//__REMOLLGENERICDETECTORSUM_HH
