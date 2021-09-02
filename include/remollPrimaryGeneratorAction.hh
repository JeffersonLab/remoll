#ifndef remollPrimaryGeneratorAction_h
#define remollPrimaryGeneratorAction_h 1

#include "G4VUserPrimaryGeneratorAction.hh"
#include "G4VPrimaryGenerator.hh"
#include "G4String.hh"

#include <map>

class G4GenericMessenger;
class G4ParticleGun;
class G4Event;
class remollBeamTarget;
class remollVEventGen;
class remollEvent;

class remollPrimaryGeneratorAction : public G4VUserPrimaryGeneratorAction
{
  public:
    remollPrimaryGeneratorAction();
    virtual ~remollPrimaryGeneratorAction();

  public:
    void GeneratePrimaries(G4Event* anEvent);

    const remollEvent* GetEvent() const { return fEvent; }

    void SetGenerator(G4String&);

  private:
    std::map<G4String,remollVEventGen*> fEvGenMap;
    remollVEventGen *fEventGen;
    G4String fEventGenName;

    std::map<G4String,G4VPrimaryGenerator*> fPriGenMap;
    G4VPrimaryGenerator *fPriGen;
    G4String fPriGenName;

    G4ParticleGun* fParticleGun;

    remollBeamTarget *fBeamTarg;


    remollEvent *fEvent;

    G4GenericMessenger* fMessenger;
  G4int fRateCopy;
    G4GenericMessenger* fEvGenMessenger;

    G4double fEffCrossSection;
};

#endif


