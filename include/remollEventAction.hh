#ifndef remollEventAction_h
#define remollEventAction_h 1

#include "G4UserEventAction.hh"
#include "G4Timer.hh"

#include "globals.hh"

class G4Event;

class remollPrimaryGeneratorAction;
class remollIO;
class remollTrackReconstruct;

class remollEventAction : public G4UserEventAction
{
  public:
    remollEventAction();
    virtual ~remollEventAction();

  public:
    virtual void BeginOfEventAction(const G4Event*);
    virtual void EndOfEventAction(const G4Event*);

  private:
    // Pointer to primary generator action
    remollPrimaryGeneratorAction* fPrimaryGeneratorAction;
    remollIO *fIO;
    remollTrackReconstruct* rTrack;

    // Timer for benchmarking of simulation time per event
    G4Timer fTimer;

  public:
    // Setter for primary generator action
    void SetPrimaryGeneratorAction(remollPrimaryGeneratorAction* action) {
      fPrimaryGeneratorAction = action;
    }

  private:
    // Random seed at begin of event
    G4String fEventSeed;
};

#endif

