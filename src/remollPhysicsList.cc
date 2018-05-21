#include "remollPhysicsList.hh"

#include "G4PhysListFactory.hh"
#include "G4OpticalPhysics.hh"
#include "G4GenericMessenger.hh"
#include "G4RunManager.hh"
#include "G4HadronicProcessStore.hh"
#include "G4ParticleHPManager.hh"

remollPhysicsList::remollPhysicsList()
: G4VModularPhysicsList(),
  fReferencePhysList(0),fOpticalPhysics(0),
  fPhysListMessenger(0),fBaseMessenger(0)
{
  // Let users know to ignore the warning by Particle HP package
  G4cout << "remoll: Since the high precision neutron simulation in the default physics list" << G4endl;
  G4cout << "remoll: generates a lot of warnings that cannot be avoided, we are setting the " << G4endl;
  G4cout << "remoll: physics list verbose level to zero. Use /remoll/physlist/verbose to set" << G4endl;
  G4cout << "remoll: the verbose level to a non-zero value." << G4endl << G4endl;
  //
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
  // TODO deleting old reference physics lists still fails when the same one
  // was loaded again, e.g. when loading default physics list explicitly
  //for (size_t i = 0; i < fReferencePhysicsListToDelete.size(); i++) {
  //  delete fReferencePhysicsListToDelete.at(i);
  //}
  //fReferencePhysicsListToDelete.clear();

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
  G4ParticleHPManager::GetInstance()->SetVerboseLevel(level);
  G4cout << G4endl; // empty line after G4ParticleHPManager complaint
}

void remollPhysicsList::SetOpticalPhysics(G4bool flag)
{
  if (flag) EnableOpticalPhysics();
  else     DisableOpticalPhysics();
}

void remollPhysicsList::EnableOpticalPhysics()
{
  if (fOpticalPhysics) {
    G4cout << "Optical physics already active" << G4endl;
    return;
  }

  // Print output
  if (GetVerboseLevel() > 0)
    G4cout << "Registering optical physics" << G4endl;

  // Create optical physics
  fOpticalPhysics = new G4OpticalPhysics(GetVerboseLevel());

  // Register existing physics
  RegisterPhysics(fOpticalPhysics);
}

void remollPhysicsList::DisableOpticalPhysics()
{
  if (!fOpticalPhysics) {
    G4cout << "Optical physics not active" << G4endl;
    return;
  }

  // Print output
  if (GetVerboseLevel() > 0)
    G4cout << "Removing optical physics" << G4endl;

  // Remove optical physics
  RemovePhysics(fOpticalPhysics);

  // Delete optical physics
  delete fOpticalPhysics;
  fOpticalPhysics = 0;
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

void remollPhysicsList::RemoveReferencePhysList()
{
  // Print output
  if (GetVerboseLevel() > 0)
    G4cout << "Removing existing reference physics list" << G4endl;

  // Remove physics in previous reference list
  G4int i = 0;
  G4VPhysicsConstructor* elem = 0;
  while ((elem = const_cast<G4VPhysicsConstructor*>(fReferencePhysList->GetPhysics(i++))) != 0) {
    // Print output
    if (GetVerboseLevel() > 0)
      G4cout << "Removing " << elem->GetPhysicsName() << G4endl;

    // Remove physics
    RemovePhysics(elem);
  }

  // Delete reference physics list
  fReferencePhysicsListToDelete.push_back(fReferencePhysList);
  fReferencePhysList = 0;
}

void remollPhysicsList::RegisterReferencePhysList(G4String name)
{
  // Already loaded
  if (name == fReferencePhysListName) {
    G4cout << "Reference physics list " << name << " already loaded" << G4endl;
    return;
  }

  // Load the factory
  G4PhysListFactory factory;
  factory.SetVerbose(GetVerboseLevel());

  // Check whether this reference physics list exists
  if (! factory.IsReferencePhysList(name)) {
    G4cerr << "Physics list " << name
           << " is not a valid reference physics list" << G4endl;
    ListReferencePhysLists();
    return;
  }

  // Remove previous reference physics list
  if (fReferencePhysList) RemoveReferencePhysList();

  // Get reference physics list
  fReferencePhysList = factory.GetReferencePhysList(name);
  fReferencePhysList->SetVerboseLevel(GetVerboseLevel());
  fReferencePhysListName = name;

  // Register physics from this list
  G4int i = 0;
  G4VPhysicsConstructor* elem = 0;
  while ((elem = const_cast<G4VPhysicsConstructor*>(fReferencePhysList->GetPhysics(i++))) != 0) {
    // Change verbose level
    elem->SetVerboseLevel(GetVerboseLevel());

    // Print output
    if (GetVerboseLevel() > 0)
      G4cout << "Registering " << elem->GetPhysicsName() << G4endl;

    // Register existing physics
    RegisterPhysics(elem);
  }

  // Blank space
  if (GetVerboseLevel() > 0)
    G4cout << G4endl;
}
