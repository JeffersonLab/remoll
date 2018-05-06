
#ifndef __REMOLLSTEPPINGACTION_HH
#define __REMOLLSTEPPINGACTION_HH

#include "G4UserSteppingAction.hh"
#include "globals.hh"

#include <set>

class G4Material;
class G4GenericMessenger;

class remollSteppingAction : public G4UserSteppingAction
{
  public:
    remollSteppingAction();
    virtual ~remollSteppingAction();

    virtual void UserSteppingAction(const G4Step*);

    void SetEnableKryptonite(G4bool k){ fEnableKryptonite = k; }

    void AddKryptoniteCandidate(const G4String name);
    void DelKryptoniteCandidate(const G4String name);
    void ListKryptoniteCandidates();

  private:
    G4bool fDrawFlag;

    G4bool fEnableKryptonite;
    std::set<G4String> fKryptoniteCandidates;
    std::set<G4Material*> fKryptoniteMaterials;
    void InitializeMaterials();

    G4GenericMessenger* fMessenger;

  public:
    inline void SetDrawFlag(G4bool val) { fDrawFlag = val; };
};

#endif//__REMOLLSTEPPINGACTION_HH
