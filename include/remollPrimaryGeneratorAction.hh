
#ifndef remollPrimaryGeneratorAction_h
#define remollPrimaryGeneratorAction_h 1

#include "G4VUserPrimaryGeneratorAction.hh"
#include "G4String.hh"

class G4ParticleGun;
class G4Event;
class remollIO;
class remollVEventGen;
class remollEvent;

class remollPrimaryGeneratorAction : public G4VUserPrimaryGeneratorAction
{
  public:
    remollPrimaryGeneratorAction();
    ~remollPrimaryGeneratorAction();

  public:
    void GeneratePrimaries(G4Event* anEvent);
    G4ParticleGun* GetParticleGun();
    void SetIO( remollIO *io ){ fIO = io; }

    void SetGenerator( G4String );

    remollVEventGen *GetGenerator(){ return fEventGen; }

  private:
    G4ParticleGun* fParticleGun;

    remollVEventGen *fEventGen;
    remollEvent *fDefaultEvent;
    remollIO *fIO;
};

#endif


