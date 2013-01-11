
#ifndef remollPrimaryGeneratorAction_h
#define remollPrimaryGeneratorAction_h 1

#include "G4VUserPrimaryGeneratorAction.hh"

class G4ParticleGun;
class G4Event;
class remollIO;

class remollPrimaryGeneratorAction : public G4VUserPrimaryGeneratorAction
{
  public:
    remollPrimaryGeneratorAction();
    ~remollPrimaryGeneratorAction();

  public:
    void GeneratePrimaries(G4Event* anEvent);
    G4ParticleGun* GetParticleGun();
    void SetIO( remollIO *io ){ fIO = io; }

  private:
    G4ParticleGun* particleGun;

    remollIO *fIO;
};

#endif


