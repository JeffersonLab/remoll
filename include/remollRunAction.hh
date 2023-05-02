#ifndef remollRunAction_h
#define remollRunAction_h 1

#include "globals.hh"
#include "G4GenericMessenger.hh"
#include "G4UserRunAction.hh"
#include "G4Timer.hh"

class G4Run;

class remollRunAction : public G4UserRunAction
{
  public:
    remollRunAction();
    virtual ~remollRunAction() = default;

  public:
    G4Run* GenerateRun();

    void BeginOfRunAction(const G4Run* aRun);
    void EndOfRunAction(const G4Run* aRun);

    void UpdateSeed(const G4long seed);

  private:
    G4GenericMessenger fMessenger{
        this,
        "/remoll/",
        "Remoll properties"};

    G4Timer fTimer;

  private:
    G4int fInterval{10};
  public:
    void SetUpdateInterval(G4int interval) { fInterval = interval; };
};

#endif

