
#ifndef __REMOLLSTEPPINGACTION_HH
#define __REMOLLSTEPPINGACTION_HH

#include "G4UserSteppingAction.hh"
#include "globals.hh"
#include <unordered_map>
#include "G4ThreeVector.hh"

class remollEvent;
//class remollIO;

class remollSteppingAction : public G4UserSteppingAction
{
    private:
//        static remollSteppingAction *gSingletonSteppingAction; // NEW potentially for having a static self reference, if needed
    public:
    remollSteppingAction();
    virtual ~remollSteppingAction(){};

//    void SetIO( remollIO *io ){ fIO = io; }
    
    virtual void UserSteppingAction(const G4Step*);

    void SetEnableKryptonite(G4bool k){ fEnableKryptonite = k; }

    // void UpdateLastSigVert( G4Track, G4ThreeVector, G4double, G4double ); // NEW - Store last significant 
    // vertex info - This really doesn't need to be its own method, since the public hash tables 
    // are already defined publicly in the class of interest.

  private:
    G4bool drawFlag;

//    remollIO *fIO;
    
    G4bool fEnableKryptonite;

  public:

    inline void SetDrawFlag(G4bool val)
    { drawFlag = val; };
};

#endif//__REMOLLSTEPPINGACTION_HH
