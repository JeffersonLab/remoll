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

    // Set Parallel physics
    void SetParallelPhysics(G4bool flag);
    // Enable Parallel physics
    void EnableParallelPhysics();
    // Disable Parallel physics
    void DisableParallelPhysics();

    // Set optical physics
    void SetOpticalPhysics(G4bool flag);
    // Enable optical physics
    void EnableOpticalPhysics();
    // Disable optical physics
    void DisableOpticalPhysics();

    // Set step limiter physics
    void SetStepLimiterPhysics(G4bool flag);
    // Enable step limiter physics
    void EnableStepLimiterPhysics();
    // Disable step limiter physics
    void DisableStepLimiterPhysics();

    // Handle reference physics lists in messenger
    void ListReferencePhysLists();
    void RemoveReferencePhysList();
    void RegisterReferencePhysList(G4String name);

  private:
    G4String fReferencePhysListName;
    G4VModularPhysicsList* fReferencePhysList;
    G4VPhysicsConstructor* fParallelPhysics;
    G4VPhysicsConstructor* fOpticalPhysics;
    G4VPhysicsConstructor* fStepLimiterPhysics;

    // Deleting an unused physics list also deletes particles, causing
    // all kinds of issues with new reference physics lists
    std::vector<G4VModularPhysicsList*> fReferencePhysicsListToDelete;

  protected:
    // Generic messenger as protected to be used in derived classes
    G4GenericMessenger* fPhysListMessenger;
    G4GenericMessenger* fOpticalMessenger;
    G4GenericMessenger* fParallelMessenger;
    G4GenericMessenger* fStepLimiterMessenger;
    G4GenericMessenger* fBaseMessenger;
};

#endif
