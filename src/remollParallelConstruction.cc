#include "remollParallelConstruction.hh"

#include <G4GDMLParser.hh>
#include <G4LogicalVolume.hh>
#include <G4PVPlacement.hh>
#include <G4SDManager.hh>
#include <G4GenericMessenger.hh>
#include <G4VisAttributes.hh>
#include <G4Colour.hh>

#include "remollGenericDetector.hh"

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
remollParallelConstruction::remollParallelConstruction(const G4String& gdmlfile)
: G4VUserParallelWorld("parallel_logic_PV"),
  fConstructed(false),
  fVerboseLevel(1),
  fParallelMessenger(0)
{
  // If gdmlfile is non-empty
  if (gdmlfile.length() > 0) fGDMLFile = gdmlfile;

  // Create GDML parser
  fGDMLParser = new G4GDMLParser();

  // Create parallel geometry messenger
  fParallelMessenger = new G4GenericMessenger(this,
      "/remoll/geometry/parallel/",
      "Remoll parallel geometry properties");
  fParallelMessenger->DeclareMethod(
      "setfile",
      &remollParallelConstruction::SetParallelGeomFile,
      "Set parallel geometry GDML file")
      .SetStates(G4State_PreInit);
  fParallelMessenger->DeclareProperty(
      "verbose",
      fVerboseLevel,
      "Set geometry verbose level")
          .SetStates(G4State_PreInit);
  fParallelMessenger->DeclareProperty(
      "validate",
      fGDMLValidate,
      "Set GMDL validate flag")
          .SetStates(G4State_PreInit)
          .SetDefaultValue("true");
  fParallelMessenger->DeclareProperty(
      "overlapcheck",
      fGDMLOverlapCheck,
      "Set GMDL overlap check flag")
          .SetStates(G4State_PreInit)
          .SetDefaultValue("true");
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
remollParallelConstruction::~remollParallelConstruction()
{
  delete fParallelMessenger;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
void remollParallelConstruction::Construct()
{
  if (fConstructed) return;
  fConstructed = true;

  // Read GDML parallel world
  fGDMLParser->Clear();
  fGDMLParser->Read(fGDMLFile);

  // Get parallel world
  G4VPhysicalVolume* parallelWorld = fGDMLParser->GetWorldVolume();
  G4LogicalVolume *parallelWorldLogical = parallelWorld->GetLogicalVolume();

  // Get ghost world
  G4VPhysicalVolume* ghostWorld = GetWorld();
  G4LogicalVolume* ghostWorldLogical = ghostWorld->GetLogicalVolume();

  // Place parallel world into ghost world
  new G4PVPlacement(0,G4ThreeVector(0.,0.,0.),
                    parallelWorldLogical,"parallel_logic_PV",
                    ghostWorldLogical,false,0);

  // Set the world volume invisible
  G4VisAttributes* motherVisAtt = new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  motherVisAtt->SetForceWireframe(true);
  parallelWorld->GetLogicalVolume()->SetVisAttributes(motherVisAtt);
  ghostWorld->GetLogicalVolume()->SetVisAttributes(motherVisAtt);

  // Parse visibility
  ParseAuxiliaryVisibilityInfo();
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
void remollParallelConstruction::ParseAuxiliaryVisibilityInfo()
{
  // Loop over volumes with auxiliary information
  const G4GDMLAuxMapType* auxmap = fGDMLParser->GetAuxMap();
  for(G4GDMLAuxMapType::const_iterator
      iter  = auxmap->begin();
      iter != auxmap->end(); iter++) {

    if (fVerboseLevel > 0)
      G4cout << "Volume " << ((*iter).first)->GetName()
             << " has the following list of auxiliary information: "<< G4endl;

    // Loop over auxiliary tags for this logical volume
    for (G4GDMLAuxListType::const_iterator
         vit  = (*iter).second.begin();
         vit != (*iter).second.end(); vit++) {

      if (fVerboseLevel > 0)
        G4cout << "--> Type: " << (*vit).type
               << " Value: "   << (*vit).value << std::endl;

      // Visibility = true|false|wireframe
      if ((*vit).type == "Visibility") {
        G4Colour colour(1.0,1.0,1.0);
        const G4VisAttributes* visAttribute_old = ((*iter).first)->GetVisAttributes();
        if (visAttribute_old)
          colour = visAttribute_old->GetColour();
        G4VisAttributes visAttribute_new(colour);
        if ((*vit).value == "true")
          visAttribute_new.SetVisibility(true);
        if ((*vit).value == "false")
          visAttribute_new.SetVisibility(false);
        if ((*vit).value == "wireframe")
          visAttribute_new.SetForceWireframe(false);

        ((*iter).first)->SetVisAttributes(visAttribute_new);
      }

      // Color = name
      if ((*vit).type == "Color") {
        G4Colour colour(1.0,1.0,1.0);
        if (G4Colour::GetColour((*vit).value, colour)) {

          if (fVerboseLevel > 0)
            G4cout << "Setting color to " << (*vit).value << "." << G4endl;

          G4VisAttributes visAttribute(colour);
          ((*iter).first)->SetVisAttributes(visAttribute);

        } else {

          if (fVerboseLevel > 0)
            G4cout << "Colour " << (*vit).value << " is not known." << G4endl;

        }
      }

      // Alpha = float between 0 and 1
      if ((*vit).type == "Alpha") {
        G4Colour colour(1.0,1.0,1.0);
        const G4VisAttributes* visAttribute_old = ((*iter).first)->GetVisAttributes();

        if (visAttribute_old)
          colour = visAttribute_old->GetColour();

        G4Colour colour_new(
            colour.GetRed(),
            colour.GetGreen(),
            colour.GetBlue(),
            std::atof((*vit).value.c_str()));
        G4VisAttributes visAttribute_new(colour_new);
        ((*iter).first)->SetVisAttributes(visAttribute_new);
      }
    }
  }
  if (fVerboseLevel > 0)
      G4cout << G4endl << G4endl;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
void remollParallelConstruction::ConstructSD()
{
  //==========================
  // Sensitive detectors
  //==========================
  G4SDManager* SDman = G4SDManager::GetSDMpointer();

  if (fVerboseLevel > 0)
      G4cout << "Beginning sensitive detector assignment" << G4endl;

  const G4GDMLAuxMapType* auxmap = fGDMLParser->GetAuxMap();
  for (G4GDMLAuxMapType::const_iterator iter  = auxmap->begin(); iter != auxmap->end(); iter++) {
      G4LogicalVolume* myvol = (*iter).first;
      if (fVerboseLevel > 0)
          G4cout << "Volume " << myvol->GetName() << G4endl;

      for (G4GDMLAuxListType::const_iterator
          vit  = (*iter).second.begin();
          vit != (*iter).second.end(); vit++) {

          if ((*vit).type == "SensDet") {

              // Also allow specification of det number ///////////////////
              G4String det_type = "";
              int det_no = -1;
              for (G4GDMLAuxListType::const_iterator
                  nit  = (*iter).second.begin();
                  nit != (*iter).second.end(); nit++) {

                  if ((*nit).type == "DetNo") {
                      det_no = atoi((*nit).value.data());
                  }

                  if ((*nit).type == "DetType") {
                      det_type = (*nit).value.data();
                  }
              }
              if (det_no <= 0) {
                  G4cerr << __FILE__ << " line " << __LINE__ << ": "
                         << "Warning: detector number not set for volume " << myvol->GetName() << G4endl;
                  G4cerr << "Skipping sensitive detector assignment." << G4endl;
                  continue;
              }
              /////////////////////////////////////////////////////////////

              char det_no_str[10];
              snprintf(det_no_str, 10, "%d", det_no);
              G4String detectorname = "remoll/det_" + G4String(det_no_str);

              G4VSensitiveDetector* thisdet = SDman->FindSensitiveDetector(detectorname,(fVerboseLevel > 0));

              if( thisdet == 0 ) {
                  if (fVerboseLevel > 0)
                      G4cout << "  Creating sensitive detector "
                          << "for volume " << myvol->GetName()
                          <<  G4endl << G4endl;

                  remollGenericDetector* det = new remollGenericDetector(detectorname, det_no);
                  if (det_type.size() > 0) det->SetDetectorType(det_type);

                  SDman->AddNewDetector(det);

                  thisdet = det;
              }

              myvol->SetSensitiveDetector(thisdet);
          }
      }
  }

  if (fVerboseLevel > 0)
    G4cout << "Completed sensitive detector assignment" << G4endl;
}

