
#ifndef remollPrimaryGeneratorAction_h
#define remollPrimaryGeneratorAction_h 1

#include "G4VUserPrimaryGeneratorAction.hh"

class G4ParticleGun;
class G4Event;
class remollEventGen;
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

    remollEventGen *GetEvGen(){ return sbsgen; }

    void SetUseGeantino(bool b){ fUseGeantino = b; }

  private:
    G4ParticleGun* particleGun;
    remollEventGen* sbsgen;

    remollIO *fIO;

    bool fUseGeantino;
};

#endif


