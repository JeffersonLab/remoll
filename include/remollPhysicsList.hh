#ifndef remollPhysicsList_h
#define remollPhysicsList_h 1

#include "G4VModularPhysicsList.hh"

class G4GenericMessenger;
class G4VPhysicsConstructor;

class remollPhysicsList: public G4VModularPhysicsList
{
  public:
    remollPhysicsList();
    virtual ~remollPhysicsList();

  public:
    // Set optical physics
    void SetOpticalPhysics(G4bool flag);
    // Enable optical physics
    void EnableOpticalPhysics();
    // Disable optical physics
    void DisableOpticalPhysics();

    // Handle reference physics lists in messenger
    void ListReferencePhysLists();
    void RegisterReferencePhysList(G4String name);

  private:
    G4int fVerboseLevel;

    G4VModularPhysicsList* fReferencePhysList;
    G4VPhysicsConstructor* fOpticalPhysics;

  protected:
    // Generic messenger as protected to be used in derived classes
    G4GenericMessenger* fPhysListMessenger;
    G4GenericMessenger* fOpticalMessenger;
    G4GenericMessenger* fBaseMessenger;
};

#endif
