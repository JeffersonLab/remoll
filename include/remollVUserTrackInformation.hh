#ifndef __REMOLLVUSERTRACKINFORMATION_HH
#define __REMOLLVUSERTRACKINFORMATION_HH

#include "G4Track.hh"
#include "G4Step.hh"
#include "G4VUserTrackInformation.hh"
#include "G4ThreeVector.hh"
#include "remolltypes.hh"
#include "globals.hh"
#include "G4Allocator.hh"
#include "G4ParticleDefinition.hh"

// Guide obtained from http://geant4.slac.stanford.edu/Tips/event/1.html

class remollVUserTrackInformation : public G4VUserTrackInformation 
{
  public:
    remollVUserTrackInformation();
    remollVUserTrackInformation(const G4Track* aTrack);
    remollVUserTrackInformation(const remollVUserTrackInformation* aTrackInfo);
    virtual ~remollVUserTrackInformation();

    inline void *operator new(size_t);
    inline void operator delete(void *aTrackInfo);
    inline int operator ==(const remollVUserTrackInformation& right) const 
      { return (this==&right);}

    void SetLastSigVert(G4double dE, G4double dEDep, G4double dTh, G4ThreeVector Pos);
    
    void Print() const;

  private:
    G4double LastSigVertdE;
    G4double LastSigVertdEDep;
    G4double LastSigVertdTh;
    G4ThreeVector LastSigVertPos;
  
  public:
    G4double GetLastSigVertdE();
    G4double GetLastSigVertdEDep();
    G4double GetLastSigVertdTh();
    G4ThreeVector GetLastSigVertPos();

};

extern G4Allocator<remollVUserTrackInformation> aTrackInformationAllocator;

inline void* remollVUserTrackInformation::operator new(size_t)
{ void* aTrackInfo;
  aTrackInfo = (void*)aTrackInformationAllocator.MallocSingle();
  return aTrackInfo;
}

inline void remollVUserTrackInformation::operator delete(void *aTrackInfo) 
{ aTrackInformationAllocator.FreeSingle((remollVUserTrackInformation*)aTrackInfo);}

#endif//__REMOLLVUSERTRACKINFORMATION
