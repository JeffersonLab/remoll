
#ifndef remollRunAction_h
#define remollRunAction_h 1

#include "globals.hh"
#include "G4UserRunAction.hh"

class G4Timer;
class G4Run;
class remollIO;

class remollRunAction : public G4UserRunAction
{
  public:
    remollRunAction();
    ~remollRunAction();

  public:
    void BeginOfRunAction(const G4Run* aRun);
    void EndOfRunAction(const G4Run* aRun);

    void SetIO( remollIO *io ){ fIO = io; }

  private:
    G4Timer* timer;

    remollIO *fIO;
};

#endif

