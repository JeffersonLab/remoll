#include "remollSteppingAction.hh"

#include "G4Step.hh"
#include "G4Track.hh"
#include "G4Material.hh"
#include "G4MaterialTable.hh"
#include "G4GenericMessenger.hh"

remollSteppingAction::remollSteppingAction()
: fDrawFlag(false),fEnableKryptonite(true)
{
  // Create generic messenger
  fMessenger = new G4GenericMessenger(this,"/remoll/","Remoll properties");
  fMessenger->DeclareProperty("kryptonite",fEnableKryptonite,"Treat W, Pb, Cu as kryptonite");
}

remollSteppingAction::~remollSteppingAction()
{
  delete fMessenger;
}

void remollSteppingAction::InitializeMaterials()
{
  // List of kryptonite materials
  std::set<G4String> materials = {"Tungsten", "Pb", "Copper"};

  // Find kryptonite materials in material tables
  G4MaterialTable* table = G4Material::GetMaterialTable();
  G4cout << "Loading kryptonite materials table..." << G4endl;
  for (G4MaterialTable::const_iterator
      it  = table->begin();
      it != table->end(); it++) {
    if (materials.find((*it)->GetName()) != materials.end()) {
      fKryptoniteMaterials.insert(*it);
      G4cout << "Treating " << (*it)->GetName() << " as kryptonite." << G4endl;
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
