
#ifndef __REMOLLSTEPPINGACTION_HH
#define __REMOLLSTEPPINGACTION_HH

#include "G4UserSteppingAction.hh"
#include "globals.hh"

class remollEvent;
class remollIO;

class remollSteppingAction : public G4UserSteppingAction
{
  public:
    remollSteppingAction();
    virtual ~remollSteppingAction(){};

    void SetIO( remollIO *io ){ fIO = io; }
    
    virtual void UserSteppingAction(const G4Step*);

    void SetEnableKryptonite(G4bool k){ fEnableKryptonite = k; }

  private:
    G4bool drawFlag;

    remollIO *fIO;
    
    G4bool fEnableKryptonite;

  public:
    inline void SetDrawFlag(G4bool val)
    { drawFlag = val; };
};

#endif//__REMOLLSTEPPINGACTION_HH
