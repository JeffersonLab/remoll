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
	virtual ~remollGenericDetectorHit();

	remollGenericDetectorHit(const remollGenericDetectorHit &right);
	const remollGenericDetectorHit& operator=(const remollGenericDetectorHit &right);
	G4int operator==(const remollGenericDetectorHit &right) const;

	inline void *operator new(size_t);
	inline void operator delete(void *aHit);

        void Print();

    private:

    public:
	G4int fDetID;
	G4int fCopyID;

	// Position and momentum in lab coordinates
	G4ThreeVector f3X;
	G4ThreeVector f3P;
	G4ThreeVector f3S;

	// Position and momentum in local volume coordinate system
	G4ThreeVector f3Pl; 
	G4ThreeVector f3Xl;

        // Global time
        G4double fTime;
        // direction
        G4ThreeVector f3dP;

        // reconstructed pos, Th for GEM
	G4ThreeVector f3XRec;
	G4ThreeVector f3dPRec;
        G4double fThRec;

	// Total momentum, energy, mass, kinetic energy
	G4double fP, fE, fM, fK;
	// Origin
	G4ThreeVector f3V;
	// Geant4 track ID, particle type, and mother ID
	G4int    fTrID, fPID, fmTrID;
	// Process generator type
	G4int    fGen;
        // Energy deposited
        G4double fEdep;

    public:
      const remollGenericDetectorHit_t GetGenericDetectorHitIO() const {
        remollGenericDetectorHit_t hit;
        hit.det  = fDetID;
        hit.id   = fCopyID;
        hit.trid = fTrID;
        hit.mtrid= fmTrID;
        hit.pid  = fPID;
        hit.gen  = fGen;
        hit.t  = fTime;
        hit.x  = f3X.x();
        hit.y  = f3X.y();
        hit.z  = f3X.z();
        hit.xl = f3Xl.x();
        hit.yl = f3Xl.y();
        hit.zl = f3Xl.z();
        hit.r  = sqrt(f3X.x()*f3X.x()+f3X.y()*f3X.y());
        hit.ph = f3X.phi();
        hit.px  = f3P.x();
        hit.py  = f3P.y();
        hit.pz  = f3P.z();
        hit.pxl  = f3Pl.x();
        hit.pyl  = f3Pl.y();
        hit.pzl  = f3Pl.z();
        hit.sx  = f3S.x();
        hit.sy  = f3S.y();
        hit.sz  = f3S.z();
        hit.vx  = f3V.x();
        hit.vy  = f3V.y();
        hit.vz  = f3V.z();
        hit.p  = fP;
        hit.e  = fE;
        hit.m  = fM;
        hit.k  = fK;
        hit.edep = fEdep;
        return hit;
      };
};


typedef G4THitsCollection<remollGenericDetectorHit> remollGenericDetectorHitCollection;

extern G4ThreadLocal G4Allocator<remollGenericDetectorHit>* remollGenericDetectorHitAllocator;

inline void* remollGenericDetectorHit::operator new(size_t){
  if (!remollGenericDetectorHitAllocator)
    remollGenericDetectorHitAllocator = new G4Allocator<remollGenericDetectorHit>;
  return (void *) remollGenericDetectorHitAllocator->MallocSingle();
}

inline void remollGenericDetectorHit::operator delete(void *aHit){
  remollGenericDetectorHitAllocator->FreeSingle( (remollGenericDetectorHit*) aHit);
}

#endif //__REMOLLGENERICDETECTORHIT_HH
