#ifndef __REMOLLGENERICDETECTORSUM_HH
#define __REMOLLGENERICDETECTORSUM_HH

#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4Allocator.hh"
#include "G4ThreeVector.hh"

#include "remolltypes.hh"

#include <vector>

#include "remolltypes.hh"

class remollGenericDetectorSum : public G4VHit {
    public:
	remollGenericDetectorSum(G4int, G4int);
	virtual ~remollGenericDetectorSum();

	remollGenericDetectorSum(const remollGenericDetectorSum &right);
	const remollGenericDetectorSum& operator=(const remollGenericDetectorSum &right);
	G4int operator==(const remollGenericDetectorSum &right) const;

	inline void *operator new(size_t);
	inline void operator delete(void *aHit);

    public:
	G4int    fDetID;
	G4int    fCopyID;
	G4double fEdep;

	void AddEDep( int pid, G4ThreeVector x, double ene );

	double GetEdep( int pid );
	G4ThreeVector GetPos( int pid );

	std::vector<sumdata_t> fData;

    private:
	int parttypes[N_PART_DIVISIONS];

    public:
      const remollGenericDetectorSum_t GetGenericDetectorSumIO(int pid = 0) const {
        remollGenericDetectorSum_t sum;
        sum.pid = pid;
        sum.det = fDetID;
        sum.vid = fCopyID;
        sum.x = GetPos(pid).x();
        sum.y = GetPos(pid).y();
        sum.z = GetPos(pid).z();
        sum.edep = GetEdep(pid);
        return sum;
      }
};


typedef G4THitsCollection<remollGenericDetectorSum> remollGenericDetectorSumCollection;

extern G4ThreadLocal G4Allocator<remollGenericDetectorSum>* remollGenericDetectorSumAllocator;

inline void* remollGenericDetectorSum::operator new(size_t){
  if (!remollGenericDetectorSumAllocator)
    remollGenericDetectorSumAllocator = new G4Allocator<remollGenericDetectorSum>;
  return (void *) remollGenericDetectorSumAllocator->MallocSingle();
}

inline void remollGenericDetectorSum::operator delete(void *aHit){
  remollGenericDetectorSumAllocator->FreeSingle( (remollGenericDetectorSum*) aHit);
}

#endif//__REMOLLGENERICDETECTORSUM_HH
