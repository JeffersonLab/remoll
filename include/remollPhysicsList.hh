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
    // Set verbose level
    void SetVerboseLevel(G4int level);

    // Set optical physics
    void SetOpticalPhysics(G4bool flag);
    // Enable optical physics
    void EnableOpticalPhysics();
    // Disable optical physics
    void DisableOpticalPhysics();

    // Handle reference physics lists in messenger
    void ListReferencePhysLists();
    void RemoveReferencePhysList();
    void RegisterReferencePhysList(G4String name);

  private:
    G4String fReferencePhysListName;
    G4VModularPhysicsList* fReferencePhysList;
    G4VPhysicsConstructor* fOpticalPhysics;

    // Deleting an unused physics list also deletes particles, causing
    // all kinds of issues with new reference physics lists
    std::vector<G4VModularPhysicsList*> fReferencePhysicsListToDelete;

  protected:
    // Generic messenger as protected to be used in derived classes
    G4GenericMessenger* fPhysListMessenger;
    G4GenericMessenger* fOpticalMessenger;
    G4GenericMessenger* fBaseMessenger;
};

#endif
