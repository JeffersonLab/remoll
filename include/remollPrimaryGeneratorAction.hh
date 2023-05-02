#ifndef remollPrimaryGeneratorAction_h
#define remollPrimaryGeneratorAction_h 1

#include "remollBeamTarget.hh"

#include "G4VUserPrimaryGeneratorAction.hh"
#include "G4VPrimaryGenerator.hh"
#include "G4GenericMessenger.hh"
#include "G4String.hh"

#include <map>
#include <memory>

class G4ParticleGun;
class G4Event;
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
    std::map<G4String,std::shared_ptr<remollVEventGen>> fEvGenMap;
    std::shared_ptr<remollVEventGen> fEventGen;
    G4String fEventGenName;

    std::map<G4String,std::shared_ptr<G4VPrimaryGenerator>> fPriGenMap;
    std::shared_ptr<G4VPrimaryGenerator> fPriGen;
    G4String fPriGenName;

    G4ParticleGun* fParticleGun;

    remollBeamTarget fBeamTarg;


    remollEvent *fEvent;

    G4int fRateCopy;
    G4GenericMessenger fEvGenMessenger{this,"/remoll/evgen/","Remoll event generator properties"};


    G4double fEffCrossSection;
};

#endif


