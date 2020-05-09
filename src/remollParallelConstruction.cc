#include "remollParallelConstruction.hh"

#include <sys/param.h>

#include <G4GDMLParser.hh>
#include <G4LogicalVolume.hh>
#include <G4PVPlacement.hh>
#include <G4SDManager.hh>
#include <G4GenericMessenger.hh>
#include <G4VisAttributes.hh>
#include <G4Colour.hh>

#include "remollGenericDetector.hh"
#include "remollIO.hh"

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
remollParallelConstruction::remollParallelConstruction(const G4String& name, const G4String& gdmlfile)
: G4VUserParallelWorld(name),
  fGDMLPath("geometry"),fGDMLFile(""),
  fGDMLParser(0),
  fGDMLValidate(false),
  fGDMLOverlapCheck(false),
  fVerboseLevel(0),
  fParallelMessenger(0),
  fWorldVolume(0),
  fWorldName(name)
{
  // If gdmlfile is non-empty
  if (gdmlfile.length() > 0) fGDMLFile = gdmlfile;

  // Create GDML parser
  fGDMLParser = new G4GDMLParser();

  // Create parallel geometry messenger
  fParallelMessenger = new G4GenericMessenger(this,
      "/remoll/parallel/",
      "Remoll parallel geometry properties");
  fParallelMessenger->DeclareMethod(
      "setfile",
      &remollParallelConstruction::SetGDMLFile,
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
  delete fGDMLParser;
  delete fParallelMessenger;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
void remollParallelConstruction::PrintGDMLWarning() const
{
    G4cout << G4endl;
    G4cout << "remoll: Note: GDML file validation can cause many warnings." << G4endl;
    G4cout << "remoll: Some can be safely ignore. Here are some guidelines:" << G4endl;
    G4cout << "remoll: - 'ID attribute is referenced but was never declared'" << G4endl;
    G4cout << "remoll:   If the attribute starts with G4_ it is likely defined" << G4endl;
    G4cout << "remoll:   in the NIST materials database and declared later." << G4endl;
    G4cout << "remoll: - 'attribute phi is not declared for element direction'" << G4endl;
    G4cout << "remoll:   Replication along the phi direction is not supported" << G4endl;
    G4cout << "remoll:   by the GDML standard, but it is by geant4." << G4endl;
    G4cout << "remoll: - 'no declaration found for element property'" << G4endl;
    G4cout << "remoll:   Setting optical properties is not supported" << G4endl;
    G4cout << "remoll:   by the GDML standard, but it is by geant4 (e.g. G01)." << G4endl;
    G4cout << G4endl;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
void remollParallelConstruction::Construct()
{
  // Only parse GDML if file is defined
  if (fGDMLFile.size() == 0) {
    G4cout << "Warning: no parallel world loaded." << G4endl;
    return;
  }

  // Parse GDML file
  fWorldVolume = ParseGDMLFile();

  // Parse auxiliary info
  PrintAuxiliaryInfo();
  ParseAuxiliaryVisibilityInfo();
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
void remollParallelConstruction::ConstructSD()
{
  // Only parse GDML if file is defined
  if (fGDMLFile.size() == 0) {
    G4cout << "Warning: no parallel world loaded." << G4endl;
    return;
  }

  // Parse auxiliary info
  ParseAuxiliarySensDetInfo();
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
G4VPhysicalVolume* remollParallelConstruction::ParseGDMLFile()
{
  // Clear parser
  //fGDMLParser->Clear(); // FIXME doesn't clear auxmap, instead just recreate
  if (fGDMLParser) delete fGDMLParser;
  fGDMLParser = new G4GDMLParser();

  // Print GDML warning
  PrintGDMLWarning();

  // Print parsing options
  G4cout << "Reading " << fGDMLFile << G4endl;
  G4cout << "- schema validation " << (fGDMLValidate? "on": "off") << G4endl;
  G4cout << "- overlap check " << (fGDMLOverlapCheck? "on": "off") << G4endl;

  // Change directory
  char cwd[MAXPATHLEN];
  if (!getcwd(cwd,MAXPATHLEN)) {
    G4cerr << __FILE__ << " line " << __LINE__ << ": ERROR no current working directory" << G4endl;
    exit(-1);
  }
  if (chdir(fGDMLPath)) {
    G4cerr << __FILE__ << " line " << __LINE__ << ": ERROR cannot change directory" << G4endl;
    exit(-1);
  }

  // Parse GDML file
  fGDMLParser->SetOverlapCheck(fGDMLOverlapCheck);
  // hide output if not validating or checking ovelaps
  if (! fGDMLOverlapCheck && ! fGDMLValidate)
    G4cout.setstate(std::ios_base::failbit);
  fGDMLParser->Read(fGDMLFile,fGDMLValidate);
  G4cout.clear();
  G4VPhysicalVolume* parallelvolume = fGDMLParser->GetWorldVolume();
  G4LogicalVolume* parallellogical = parallelvolume->GetLogicalVolume();

  // Add GDML files to IO
  remollIO* io = remollIO::GetInstance();
  io->GrabGDMLFiles(fGDMLFile);

  // Change directory back
  if (chdir(cwd)) {
    G4cerr << __FILE__ << " line " << __LINE__ << ": ERROR cannot change directory" << G4endl;
    exit(-1);
  }

  // Get world volume in which we must place parallel world
  G4VPhysicalVolume* worldvolume = GetWorld();
  G4LogicalVolume* worldlogical = worldvolume->GetLogicalVolume();

  // Place parallel world into world volume
  new G4PVPlacement(0,G4ThreeVector(0.,0.,0.),
                    parallellogical,"parallel_logic_PV",
                    worldlogical,false,0);

  return worldvolume;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
void remollParallelConstruction::PrintAuxiliaryInfo() const
{
  const G4GDMLAuxMapType* auxmap = fGDMLParser->GetAuxMap();
  G4cout << "Found " << auxmap->size()
         << " volume(s) with auxiliary information."
         << G4endl << G4endl;
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


  // Set the world volume invisible
  G4VisAttributes* motherVisAtt = new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  motherVisAtt->SetForceWireframe(true);
  fWorldVolume->GetLogicalVolume()->SetVisAttributes(motherVisAtt);

  // Set all immediate daughters of the world volume to wireframe
  G4VisAttributes* daughterVisAtt = new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  daughterVisAtt->SetForceWireframe(true);
  auto n = fWorldVolume->GetLogicalVolume()->GetNoDaughters();
  for (decltype(n) i = 0; i < n; i++) {
    fWorldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->SetVisAttributes(daughterVisAtt);
  }
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
void remollParallelConstruction::ParseAuxiliarySensDetInfo()
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

              char det_no_str[12];
              snprintf(det_no_str, 12, "%d", det_no);
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

