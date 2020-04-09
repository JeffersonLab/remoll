#include "remollPhysicsList.hh"

#include "G4PhysListFactory.hh"
#include "G4ParallelWorldPhysics.hh"
#include "G4OpticalPhysics.hh"
#include "G4GenericMessenger.hh"
#include "G4RunManager.hh"
#include "G4NuclearLevelData.hh"
#include "G4HadronicProcessStore.hh"
#include "G4ParticleHPManager.hh"

#include "G4Version.hh"
#if G4VERSION_NUMBER < 1000
#include "G4StepLimiterBuilder.hh"
#else
#include "G4StepLimiterPhysics.hh"
#endif

remollPhysicsList::remollPhysicsList()
: G4VModularPhysicsList(),
  fReferencePhysList(0),
  fParallelPhysics(0),
  fOpticalPhysics(0),
  fStepLimiterPhysics(0),
  fPhysListMessenger(0),
  fOpticalMessenger(0),
  fParallelMessenger(0),
  fBaseMessenger(0)
{
  // Let users know to ignore the warning by Particle HP package
  G4cout << "remoll: Since the high precision neutron simulation in the some physics lists  " << G4endl;
  G4cout << "remoll: generates a lot of warnings that cannot be avoided, we are setting the " << G4endl;
  G4cout << "remoll: physics list verbose level to zero. Use /remoll/physlist/verbose to set" << G4endl;
  G4cout << "remoll: the verbose level to a non-zero value." << G4endl << G4endl;
  //
  SetVerboseLevel(0);

  // Set and print default reference physics list
  RegisterReferencePhysList("QGSP_BERT");
  G4cout << "remoll: loaded reference physics list " << fReferencePhysListName << G4endl;

  // Set and print default status of other physics
  EnableStepLimiterPhysics();
  EnableParallelPhysics();
  DisableOpticalPhysics();
  G4cout << "remoll: step limiter physics is " << (fStepLimiterPhysics? "enabled":"disabled") << G4endl;
  G4cout << "remoll: parallel physics is "     << (fParallelPhysics?    "enabled":"disabled") << G4endl;
  G4cout << "remoll: optical physics is "      << (fOpticalPhysics?     "enabled":"disabled") << G4endl;

  // Create physlist messenger
  fPhysListMessenger = new G4GenericMessenger(this,
      "/remoll/physlist/",
      "Remoll physics list properties");
  fParallelMessenger = new G4GenericMessenger(this,
      "/remoll/physlist/parallel/",
      "Remoll parallel physics properties");
  fOpticalMessenger = new G4GenericMessenger(this,
      "/remoll/physlist/optical/",
      "Remoll optical physics properties");
  fStepLimiterMessenger = new G4GenericMessenger(this,
      "/remoll/physlist/steplimiter/",
      "Remoll step limiter properties");

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

  fParallelMessenger->DeclareMethod(
      "enable",
      &remollPhysicsList::EnableParallelPhysics,
      "Enable parallel physics")
              .SetStates(G4State_PreInit);
  fParallelMessenger->DeclareMethod(
      "disable",
      &remollPhysicsList::DisableParallelPhysics,
      "Disable parallel physics")
              .SetStates(G4State_PreInit);

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

  fStepLimiterMessenger->DeclareMethod(
      "enable",
      &remollPhysicsList::EnableStepLimiterPhysics,
      "Enable step limiter");
  fStepLimiterMessenger->DeclareMethod(
      "disable",
      &remollPhysicsList::DisableStepLimiterPhysics,
      "Disable step limiter");
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
  if (fStepLimiterMessenger) delete fStepLimiterMessenger;
  if (fParallelMessenger) delete fParallelMessenger;
  if (fOpticalMessenger) delete fOpticalMessenger;
  if (fBaseMessenger) delete fBaseMessenger;
}

void remollPhysicsList::SetVerboseLevel(G4int level)
{
  // Let upstream handle this first
  G4VModularPhysicsList::SetVerboseLevel(level);

  // Set verbose level of precompound deexcitation
  #if G4VERSION_NUMBER >= 1060
  if (auto nuclearleveldata = G4NuclearLevelData::GetInstance())
    if (auto param = nuclearleveldata->GetParameters())
      param->SetVerbose(level);
  #endif

  // Set verbose level of HadronicProcessStore
  G4HadronicProcessStore::Instance()->SetVerbose(level);
  G4ParticleHPManager::GetInstance()->SetVerboseLevel(level);
  G4cout << G4endl; // empty line after G4ParticleHPManager complaint
}

void remollPhysicsList::SetParallelPhysics(G4bool flag)
{
  if (flag) EnableParallelPhysics();
  else     DisableParallelPhysics();
}

void remollPhysicsList::EnableParallelPhysics()
{
  if (fParallelPhysics) {
    G4cout << "Parallel physics already active" << G4endl;
    return;
  }

  // Print output
  if (GetVerboseLevel() > 0)
    G4cout << "Registering parallel physics" << G4endl;

  // Create Parallel physics
  fParallelPhysics = new G4ParallelWorldPhysics("parallel");
  // Note: name must correspond with name of remollParallelConstruction

  // Register existing physics
  RegisterPhysics(fParallelPhysics);
}

void remollPhysicsList::DisableParallelPhysics()
{
  if (!fParallelPhysics) {
    G4cout << "Parallel physics not active" << G4endl;
    return;
  }

  // Print output
  if (GetVerboseLevel() > 0)
    G4cout << "Removing parallel physics" << G4endl;

  // Remove Parallel physics
  RemovePhysics(fParallelPhysics);

  // Delete Parallel physics
  delete fParallelPhysics;
  fParallelPhysics = 0;
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

void remollPhysicsList::SetStepLimiterPhysics(G4bool flag)
{
  if (flag) EnableStepLimiterPhysics();
  else     DisableStepLimiterPhysics();
}

void remollPhysicsList::EnableStepLimiterPhysics()
{
  if (fStepLimiterPhysics) {
    G4cout << "Step limiter already active" << G4endl;
    return;
  }

  // Print output
  if (GetVerboseLevel() > 0)
    G4cout << "Registering step limiter physics" << G4endl;

  // Create step limiter physics
  #if G4VERSION_NUMBER < 1000
  fStepLimiterPhysics = new G4StepLimiterBuilder(GetVerboseLevel());
  #else
  fStepLimiterPhysics = new G4StepLimiterPhysics(GetVerboseLevel());
  #endif

  // Register existing physics
  RegisterPhysics(fStepLimiterPhysics);
}

void remollPhysicsList::DisableStepLimiterPhysics()
{
  if (!fStepLimiterPhysics) {
    G4cout << "Step limiter physics not active" << G4endl;
    return;
  }

  // Print output
  if (GetVerboseLevel() > 0)
    G4cout << "Removing step limiter physics" << G4endl;

  // Remove step limiter physics
  RemovePhysics(fStepLimiterPhysics);

  // Delete step limiter physics
  delete fStepLimiterPhysics;
  fStepLimiterPhysics = 0;
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
