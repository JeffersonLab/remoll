
#ifndef remollEventAction_h
#define remollEventAction_h 1

#include "G4UserEventAction.hh"
#include "G4Timer.hh"

#include "globals.hh"

class G4Event;
class remollIO;

class remollEventAction : public G4UserEventAction
{
  public:
    remollEventAction();
    virtual ~remollEventAction();

  public:
    virtual void BeginOfEventAction(const G4Event*);
    virtual void EndOfEventAction(const G4Event*);

    void SetIO( remollIO *io ){ fIO = io; }

  private:
  //  G4int gemCollID, hcalCollID, bbcalCollID;

    double fGEMres;

    remollIO *fIO;

    // Timer for benchmarking of simulation time per event
    G4Timer fTimer;

  public:
};

#endif

    
