#include "remollPhysicsList.hh"

#include "G4PhysListFactory.hh"
#include "G4OpticalPhysics.hh"
#include "G4GenericMessenger.hh"
#include "G4RunManager.hh"
#include "G4HadronicProcessStore.hh"

remollPhysicsList::remollPhysicsList()
: G4VModularPhysicsList(),
  fReferencePhysList(0),fOpticalPhysics(0),
  fPhysListMessenger(0),fBaseMessenger(0)
{
  SetVerboseLevel(0);

  // Get default reference physics list
  RegisterReferencePhysList("QGSP_BERT_HP");

  // TODO Backwards compatible, remove this on next major version change
  // Create base messenger
  fBaseMessenger = new G4GenericMessenger(this,
      "/remoll/",
      "Remoll properties");
  // Create base commands
  fBaseMessenger->DeclareMethod(
      "optical",
      &remollPhysicsList::SetOpticalPhysics,
      "Enable optical physics")
              .SetStates(G4State_PreInit)
              .SetDefaultValue("true");


  // Create physlist messenger
  fPhysListMessenger = new G4GenericMessenger(this,
      "/remoll/physlist/",
      "Remoll physics list properties");
  fOpticalMessenger = new G4GenericMessenger(this,
      "/remoll/physlist/optical/",
      "Remoll optical physics properties");

  // Create commands
  fPhysListMessenger->DeclareMethod(
      "verbose",
      &remollPhysicsList::SetVerboseLevel,
      "Set physics list verbose level")
              .SetStates(G4State_PreInit);
  fPhysListMessenger->DeclareMethod(
      "register",
      &remollPhysicsList::RegisterReferencePhysList,
      "Register reference physics list")
              .SetStates(G4State_PreInit);
  fPhysListMessenger->DeclareMethod(
      "list",
      &remollPhysicsList::ListReferencePhysLists,
      "List reference physics lists");

  fOpticalMessenger->DeclareMethod(
      "enable",
      &remollPhysicsList::EnableOpticalPhysics,
      "Enable optical physics")
              .SetStates(G4State_PreInit);
  fOpticalMessenger->DeclareMethod(
      "disable",
      &remollPhysicsList::DisableOpticalPhysics,
      "Disable optical physics")
              .SetStates(G4State_PreInit);
}

remollPhysicsList::~remollPhysicsList()
{
  if (fPhysListMessenger) delete fPhysListMessenger;
  if (fOpticalMessenger) delete fOpticalMessenger;
  if (fBaseMessenger) delete fBaseMessenger;
}

void remollPhysicsList::SetVerboseLevel(G4int level)
{
  // Let upstream handle this first
  G4VModularPhysicsList::SetVerboseLevel(level);

  // Set verbose level of HadronicProcessStore
  G4HadronicProcessStore::Instance()->SetVerbose(level);
}

void remollPhysicsList::SetOpticalPhysics(G4bool flag)
{
  if (flag) EnableOpticalPhysics();
  else     DisableOpticalPhysics();
}

void remollPhysicsList::EnableOpticalPhysics()
{
  if (fOpticalPhysics) delete fOpticalPhysics;
  fOpticalPhysics = new G4OpticalPhysics(GetVerboseLevel());

  // Print output
  if (GetVerboseLevel() > 0)
    G4cout << "Registering " << fOpticalPhysics->GetPhysicsName() << G4endl;

  // Replace existing physics
  ReplacePhysics(fOpticalPhysics);

  // Notify run manager
  G4RunManager::GetRunManager()->PhysicsHasBeenModified();
}

void remollPhysicsList::DisableOpticalPhysics()
{
  // Print output
  if (GetVerboseLevel() > 0)
    G4cout << "Removing " << fOpticalPhysics->GetPhysicsName() << G4endl;

  if (fOpticalPhysics)
    RemovePhysics(fOpticalPhysics);
  fOpticalPhysics = 0;

  //if (fOpticalPhysics) delete fOpticalPhysics;
  //fOpticalPhysics = 0;

  // Notify run manager
  G4RunManager::GetRunManager()->PhysicsHasBeenModified();
}

void remollPhysicsList::ListReferencePhysLists()
{
  G4PhysListFactory factory;
  std::vector<G4String> list;

  G4cout << "Available reference physics lists:" << G4endl;
  list = factory.AvailablePhysLists();
  for (std::vector<G4String>::const_iterator
      item  = list.begin();
      item != list.end();
      item++)
    G4cout << " " << *item << G4endl;
  G4cout << G4endl;

  G4cout << "Available physics list EM options:" << G4endl;
  list = factory.AvailablePhysListsEM();
  for (std::vector<G4String>::const_iterator
      item  = list.begin();
      item != list.end();
      item++)
    G4cout << " " << *item << G4endl;
  G4cout << G4endl;
}

void remollPhysicsList::RegisterReferencePhysList(G4String name)
{
  // This approach is based on examples/advanced/medical_linac.
  G4PhysListFactory factory;
  factory.SetVerbose(GetVerboseLevel());

  // Check whether this reference physics list exists
  if (! factory.IsReferencePhysList(name)) {
    G4cerr << "Physics list " << name
           << " is not a valid reference physics list" << G4endl;
    ListReferencePhysLists();
    return;
  }

  // Get reference physics list
  fReferencePhysList = factory.GetReferencePhysList(name);
  fReferencePhysList->SetVerboseLevel(GetVerboseLevel());

  // Register physics from this list
  G4int i = 0;
  G4VPhysicsConstructor* elem = 0;
  while ((elem = const_cast<G4VPhysicsConstructor*>(fReferencePhysList->GetPhysics(i++))) != 0) {
    // Change verbose level
    elem->SetVerboseLevel(GetVerboseLevel());

    // Print output
    if (GetVerboseLevel() > 0)
      G4cout << "Registering " << elem->GetPhysicsName() << G4endl;

    // Replace existing physics
    ReplacePhysics(elem);
  }

  // Blank space
  if (GetVerboseLevel() > 0)
    G4cout << G4endl;
}
