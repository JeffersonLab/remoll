#include "remollSteppingAction.hh"

#include "G4Step.hh"
#include "G4Track.hh"
#include "G4Material.hh"
#include "G4MaterialTable.hh"
#include "G4GenericMessenger.hh"

remollSteppingAction::remollSteppingAction()
: fDrawFlag(false),fEnableKryptonite(true)
{
  AddKryptoniteCandidate("VacuumKryptonite");
  AddKryptoniteCandidate("Tungsten");
  AddKryptoniteCandidate("Copper");
  AddKryptoniteCandidate("Lead");

  // Create generic messenger
  fMessenger = new G4GenericMessenger(this,"/remoll/kryptonite/","Remoll kryptonite properties");
  fMessenger->DeclareProperty("set",fEnableKryptonite,"Treat materials as kryptonite");
  fMessenger->DeclareMethod("add",&remollSteppingAction::AddKryptoniteCandidate,"Add specified material to list of kryptonite candidates");
  fMessenger->DeclareMethod("del",&remollSteppingAction::DelKryptoniteCandidate,"Remove specified material from list of kryptonite candidates");
  fMessenger->DeclareMethod("list",&remollSteppingAction::ListKryptoniteCandidates,"List kryptonite candidate materials");
}

remollSteppingAction::~remollSteppingAction()
{
  delete fMessenger;
}

void remollSteppingAction::AddKryptoniteCandidate(const G4String name)
{
  G4cout << "Adding " << name << " to list of kryptonite candidates." << G4endl;
  fKryptoniteCandidates.insert(name);
  InitializeMaterials();
}

void remollSteppingAction::DelKryptoniteCandidate(const G4String name)
{
  G4cout << "Removing " << name << " from list of kryptonite candidates." << G4endl;
  fKryptoniteCandidates.erase(name);
  InitializeMaterials();
}

void remollSteppingAction::ListKryptoniteCandidates()
{
  G4cout << "List of kryptonite candidate materials:" << G4endl;
  for (std::set<G4String>::const_iterator
      it  = fKryptoniteCandidates.begin();
      it != fKryptoniteCandidates.end();
      it++)
    G4cout << *it << G4endl;
}

void remollSteppingAction::InitializeMaterials()
{
  // Find kryptonite materials in material tables
  G4MaterialTable* table = G4Material::GetMaterialTable();
  G4cout << "Regenerating table of kryptonite material candidate pointers..." << G4endl;
  fKryptoniteMaterials.clear();
  for (G4MaterialTable::const_iterator
      it  = table->begin();
      it != table->end(); it++) {
    if (fKryptoniteCandidates.find((*it)->GetName()) != fKryptoniteCandidates.end()) {
      fKryptoniteMaterials.insert(*it);
    }
  }
}

void remollSteppingAction::UserSteppingAction(const G4Step *aStep)
{
    G4Track* fTrack = aStep->GetTrack();
    G4Material* material = fTrack->GetMaterial();

    // Initialize during first stepping action
    static bool needs_initialization = true;
    if (needs_initialization) {
      InitializeMaterials();
      needs_initialization = false;
    }

    // Don't continue in these materials
    if (fEnableKryptonite
        && fKryptoniteMaterials.find(material) != fKryptoniteMaterials.end()) {

      fTrack->SetTrackStatus(fStopAndKill); // kill the current track
      // fTrack->SetTrackStatus(fKillTrackAndSecondaries); // kill the current track and also associated secondaries
    }
}
