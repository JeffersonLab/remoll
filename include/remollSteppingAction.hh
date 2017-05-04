
#ifndef __REMOLLSTEPPINGACTION_HH
#define __REMOLLSTEPPINGACTION_HH

#include "G4UserSteppingAction.hh"
#include "globals.hh"

#include <set>

class G4Material;

class remollSteppingAction : public G4UserSteppingAction
{
  public:
    remollSteppingAction();
    virtual ~remollSteppingAction(){};

    virtual void UserSteppingAction(const G4Step*);

    void SetEnableKryptonite(G4bool k){ fEnableKryptonite = k; }

  private:
    G4bool fDrawFlag;

    G4bool fEnableKryptonite;
    std::set<G4Material*> fKryptoniteMaterials;

  public:
    inline void SetDrawFlag(G4bool val) { fDrawFlag = val; };
};

#endif//__REMOLLSTEPPINGACTION_HH
