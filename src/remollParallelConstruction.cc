#include "remollParallelConstruction.hh"

#include <G4GDMLParser.hh>
#include <G4LogicalVolume.hh>
#include <G4PVPlacement.hh>

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
remollParallelConstruction
::remollParallelConstruction()
: G4VUserParallelWorld("The Upside Down"),
  fConstructed(false),fFileName("geometry/parallelWorld.gdml")
{;}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
remollParallelConstruction::~remollParallelConstruction()
{;}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
void remollParallelConstruction::Construct()
{
  if (fConstructed) return;
  fConstructed = true;

  // Read GDML parallel world
  G4GDMLParser* parser = new G4GDMLParser();
  parser->Clear();
  parser->Read(fFileName);

  // Get parallel world
  G4VPhysicalVolume* parallelWorld = parser->GetWorldVolume();
  G4LogicalVolume *parallelWorldLogical = parallelWorld->GetLogicalVolume();

  // Get ghost world
  G4VPhysicalVolume* ghostWorld = GetWorld();
  G4LogicalVolume* ghostWorldLogical = ghostWorld->GetLogicalVolume();

  // Place parallel world into ghost world
  new G4PVPlacement(0,G4ThreeVector(0.,0.,0.),
                    parallelWorldLogical,"parallelWorld",
                    ghostWorldLogical,false,0);
}
