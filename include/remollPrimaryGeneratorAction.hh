
#ifndef remollPrimaryGeneratorAction_h
#define remollPrimaryGeneratorAction_h 1

#include "G4VUserPrimaryGeneratorAction.hh"
#include "G4String.hh"

class G4GenericMessenger;
class G4ParticleGun;
class G4Event;
class remollIO;
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
    void SetGenerator_Deprecated(G4String&);

  private:
    G4ParticleGun* fParticleGun;

    remollBeamTarget *fBeamTarg;

    remollVEventGen *fEventGen;

    remollEvent *fEvent;

    G4GenericMessenger* fMessenger;
    G4GenericMessenger* fEvGenMessenger;
};

#endif


