
#ifndef remollEventAction_h
#define remollEventAction_h 1

#include "G4UserEventAction.hh"
#include "G4Timer.hh"

#include "globals.hh"

class G4Event;

class remollEventAction : public G4UserEventAction
{
  public:
    remollEventAction();
    virtual ~remollEventAction();

  public:
    virtual void BeginOfEventAction(const G4Event*);
    virtual void EndOfEventAction(const G4Event*);

  private:

    // Timer for benchmarking of simulation time per event
    G4Timer fTimer;
    G4int fCounter;

};

#endif

    
