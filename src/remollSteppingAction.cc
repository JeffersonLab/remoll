#include "remollSteppingAction.hh"
//#include "remollSteppingActionMessenger.hh"

#include "G4VVisManager.hh"
#include "G4Polyline.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"
#include "G4SteppingManager.hh"
#include "G4GenericMessenger.hh"

remollSteppingAction::remollSteppingAction()
: fDrawFlag(false),fEnableKryptonite(true)
{
  // List of kryptonite materials
  std::set<G4String> materials = {"Tungsten", "Pb", "Copper"};

  // Find kryptonite materials in material tables
  G4MaterialTable* table = G4Material::GetMaterialTable();
  for (G4MaterialTable::const_iterator
      it  = table->begin();
      it != table->end(); it++) {
    if (materials.find((*it)->GetName()) != materials.end()) {
      fKryptoniteMaterials.insert(*it);
    }
  }

  // Create generic messenger
  fMessenger = new G4GenericMessenger(this,"/remoll/","Remoll properties");
  fMessenger->DeclareProperty("kryptonite",fEnableKryptonite,"Treat W, Pb, Cu as kryptonite");
}

remollSteppingAction::~remollSteppingAction()
{
  delete fMessenger;
}

void remollSteppingAction::UserSteppingAction(const G4Step *aStep)
{
    G4Track* fTrack = aStep->GetTrack();
    G4Material* material = fTrack->GetMaterial();

    // Don't continue in these materials
    if (fEnableKryptonite
        && fKryptoniteMaterials.find(material) != fKryptoniteMaterials.end()) {

      fTrack->SetTrackStatus(fStopAndKill); // kill the current track
      // fTrack->SetTrackStatus(fKillTrackAndSecondaries); // kill the current track and also associated secondaries
    }
}
