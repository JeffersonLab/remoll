#ifndef __REMOLLGENERICDETECTORHIT_HH
#define __REMOLLGENERICDETECTORHIT_HH

#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4Allocator.hh"
#include "G4ThreeVector.hh"

#include "remolltypes.hh"

class remollGenericDetectorHit : public G4VHit {
    public:
	remollGenericDetectorHit(G4int, G4int);
	~remollGenericDetectorHit();

	remollGenericDetectorHit(const remollGenericDetectorHit &right);
	const remollGenericDetectorHit& operator=(const remollGenericDetectorHit &right);
	G4int operator==(const remollGenericDetectorHit &right) const;

	inline void *operator new(size_t);
	inline void operator delete(void *aHit);
	void *operator new(size_t,void*p){return p;}

    private:

    public:
	G4int fDetID;
	G4int fCopyID;

	// Position and momentum in lab coordinates
	G4ThreeVector f3X;
	G4ThreeVector f3P;
	// Total momentum, energy, mass
	G4double fP, fE, fM;
	// Origin
	G4ThreeVector f3V;
	// Geant4 track ID, particle type, and mother ID
	G4int    fTrID, fPID, fmTrID;
	// Process generator type
	G4int    fGen;

    public:
      const remollGenericDetectorHit_t GetIO() const {
        remollGenericDetectorHit_t hit;
        hit.det  = fDetID;
        hit.id   = fCopyID;
        hit.trid = fTrID;
        hit.mtrid= fmTrID;
        hit.pid  = fPID;
        hit.gen  = fGen;
        hit.x  = f3X.x();
        hit.y  = f3X.y();
        hit.z  = f3X.z();
        hit.r  = sqrt(f3X.x()*f3X.x()+f3X.y()*f3X.y());
        hit.ph = f3X.phi();
        hit.px  = f3P.x();
        hit.py  = f3P.y();
        hit.pz  = f3P.z();
        hit.vx  = f3V.x();
        hit.vy  = f3V.y();
        hit.vz  = f3V.z();
        hit.p  = fP;
        hit.e  = fE;
        hit.m  = fM;
        return hit;
      };
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
