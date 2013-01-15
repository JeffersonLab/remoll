#ifndef __REMOLLGENERICDETECTORHIT_HH
#define __REMOLLGENERICDETECTORHIT_HH

#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4Allocator.hh"

class remollGenericDetectorHit : public G4VHit {
    public:
	remollGenericDetectorHit();
	~remollGenericDetectorHit();

	remollGenericDetectorHit(const remollGenericDetectorHit &right);
	const remollGenericDetectorHit& operator=(const remollGenericDetectorHit &right);
	G4int operator==(const remollGenericDetectorHit &right) const;

	inline void *operator new(size_t);
	inline void operator delete(void *aHit);
	void *operator new(size_t,void*p){return p;}

    private:
	/* FIXME data */
};


typedef G4THitsCollection<remollGenericDetectorHit> remollGenericDetectorHitsCollection;

extern G4Allocator<remollGenericDetectorHit> remollGenericDetectorHitAllocator;

inline void* remollGenericDetectorHit::operator new(size_t){
    void *aHit;
    aHit = (void *) remollGenericDetectorHitAllocator.MallocSingle();
    return aHit;
}

inline void remollGenericDetectorHit::operator delete(void *aHit){
    remollGenericDetectorHitAllocator.FreeSingle( (remollGenericDetectorHit*) aHit);
}

#endif//__REMOLLGENERICDETECTORHIT_HH
