#include "remollDetectorConstruction.hh"

#include "remollGenericDetector.hh"
#include "remollBeamTarget.hh"
#include "remollGlobalField.hh"
#include "remollIO.hh"

#include "G4GenericMessenger.hh"
#include "G4FieldManager.hh"
#include "G4TransportationManager.hh"

#include "G4LogicalVolume.hh"
#include "globals.hh"

#include "G4RunManager.hh"

#include "G4SDManager.hh"
#include "G4VSensitiveDetector.hh"

#include "G4UnitsTable.hh"
#include "G4NistManager.hh"

// GDML export
#include "G4GDMLParser.hh"

// visual
#include "G4VisAttributes.hh"
#include "G4Colour.hh"

#define __DET_STRLEN 200
#define __MAX_DETS 10000

#include "G4Threading.hh"
#include "G4AutoLock.hh"
namespace { G4Mutex remollDetectorConstructionMutex = G4MUTEX_INITIALIZER; }

G4ThreadLocal remollGlobalField* remollDetectorConstruction::fGlobalField = 0;

remollDetectorConstruction::remollDetectorConstruction(const G4String& gdmlfile)
: fGDMLFile("geometry/mollerMother.gdml"),fGDMLParser(0),
  fGDMLValidate(false),fGDMLOverlapCheck(true),
  fMessenger(0),fGeometryMessenger(0),
  fVerboseLevel(0),
  fWorldVolume(0)
{
  // If gdmlfile is non-empty
  if (gdmlfile.length() > 0) fGDMLFile = gdmlfile;

  // Create generic messenger
  fMessenger = new G4GenericMessenger(this,"/remoll/","Remoll properties");
  fMessenger->DeclareProperty(
      "setgeofile",
      fGDMLFile,
      "Set geometry GDML files")
      .SetStates(G4State_PreInit);
  fMessenger->DeclareMethod(
      "printgeometry",
      &remollDetectorConstruction::PrintGeometry,
      "Print the geometry tree")
      .SetStates(G4State_Idle)
      .SetDefaultValue("false");
  fMessenger->DeclareMethod(
      "printelements",
      &remollDetectorConstruction::PrintElements,
      "Print the elements")
      .SetStates(G4State_Idle);
  fMessenger->DeclareMethod(
      "printmaterials",
      &remollDetectorConstruction::PrintMaterials,
      "Print the materials")
      .SetStates(G4State_Idle);

  // Create geometry messenger
  fGeometryMessenger = new G4GenericMessenger(this,
      "/remoll/geometry/",
      "Remoll geometry properties");
  fGeometryMessenger->DeclareProperty(
      "setfile",
      fGDMLFile,
      "Set geometry GDML file")
      .SetStates(G4State_PreInit);
  fGeometryMessenger->DeclareProperty(
      "verbose",
      fVerboseLevel,
      "Set geometry verbose level")
          .SetStates(G4State_PreInit);
  fGeometryMessenger->DeclareProperty(
      "validate",
      fGDMLValidate,
      "Set GMDL validate flag")
          .SetStates(G4State_PreInit)
          .SetDefaultValue("true");
  fGeometryMessenger->DeclareProperty(
      "overlapcheck",
      fGDMLOverlapCheck,
      "Set GMDL overlap check flag")
          .SetStates(G4State_PreInit)
          .SetDefaultValue("true");
  fGeometryMessenger->DeclareMethod(
      "load",
      &remollDetectorConstruction::ReloadGeometry,
      "Reload the geometry")
      .SetStates(G4State_PreInit,G4State_Idle);
  fGeometryMessenger->DeclareMethod(
      "printelements",
      &remollDetectorConstruction::PrintElements,
      "Print the elements")
      .SetStates(G4State_Idle);
  fGeometryMessenger->DeclareMethod(
      "printmaterials",
      &remollDetectorConstruction::PrintMaterials,
      "Print the materials")
      .SetStates(G4State_Idle);
  fGeometryMessenger->DeclareMethod(
      "printgeometry",
      &remollDetectorConstruction::PrintGeometry,
      "Print the geometry tree")
      .SetStates(G4State_Idle)
      .SetDefaultValue("false");
}

remollDetectorConstruction::~remollDetectorConstruction()
{
    delete fGDMLParser;
    delete fMessenger;
    delete fGeometryMessenger;
}

void remollDetectorConstruction::PrintGDMLWarning() const
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

G4VPhysicalVolume* remollDetectorConstruction::ParseGDMLFile()
{
    // Clear parser
    //fGDMLParser->Clear(); // FIXME doesn't clear auxmap
    if (fGDMLParser) delete fGDMLParser;
    fGDMLParser = new G4GDMLParser();

    // Print GDML warning
    PrintGDMLWarning();

    // Parse GDML file
    G4cout << "Reading " << fGDMLFile << G4endl;
    G4cout << "- schema validation " << (fGDMLValidate? "on": "off") << G4endl;
    G4cout << "- overlap check " << (fGDMLOverlapCheck? "on": "off") << G4endl;
    fGDMLParser->SetOverlapCheck(fGDMLOverlapCheck);
    fGDMLParser->Read(fGDMLFile,fGDMLValidate);

    // Add GDML files to IO
    remollIO* io = remollIO::GetInstance();
    io->GrabGDMLFiles(fGDMLFile);

    // Return world volume
    return fGDMLParser->GetWorldVolume();
}

void remollDetectorConstruction::PrintAuxiliaryInfo() const
{
  const G4GDMLAuxMapType* auxmap = fGDMLParser->GetAuxMap();
  G4cout << "Found " << auxmap->size()
         << " volume(s) with auxiliary information."
         << G4endl << G4endl;
}

void remollDetectorConstruction::ParseAuxiliaryTargetInfo()
{
    //====================================================
    // Associate target volumes with beam/target class
    //====================================================

    // FIXME
    // This function is somewhat inefficient since it loops over the full
    // map of auxiliary tags in a nested fashion. If someone can figure out
    // how to improve this, you are welcome to :-)

    // Loop over volumes with auxiliary information
    const G4GDMLAuxMapType* auxmap = fGDMLParser->GetAuxMap();
    for(G4GDMLAuxMapType::const_iterator
        iter  = auxmap->begin();
        iter != auxmap->end(); iter++) {

      // Loop over auxiliary tags for this logical volume
      G4LogicalVolume* logical_volume = (*iter).first;
      for (G4GDMLAuxListType::const_iterator
          vit  = (*iter).second.begin();
          vit != (*iter).second.end(); vit++) {

        // Treat auxiliary type "TargetSystem" only
        if ((*vit).type != "TargetSystem") continue;

        // Found target mother logical volume
        G4LogicalVolume* mother_logical_volume = logical_volume;
        G4cout << "Found target mother logical volume "
               << mother_logical_volume->GetName() << "." << G4endl;

        // Now find target mother physical volume
        G4VPhysicalVolume* mother_physical_volume = 0;
        std::vector<G4VPhysicalVolume*> list =
            GetPhysicalVolumes(fWorldVolume,mother_logical_volume);
        if (list.size() == 1) {
          mother_physical_volume = list[0];

          // Mutex lock before writing static structures in remollBeamTarget
          G4AutoLock lock(&remollDetectorConstructionMutex);
          remollBeamTarget::ResetTargetVolumes();
          remollBeamTarget::SetMotherVolume(mother_physical_volume);

          G4cout << "Found target mother physical volume "
                 << mother_physical_volume->GetName() << "." << G4endl;
        } else {
          G4cout << "Target mother logical volume does not occur "
                 << "*exactly once* as a physical volume." << G4endl;
          exit(-1);
        }

        // Loop over target mother logical volume daughters
        for (int i = 0; i < mother_logical_volume->GetNoDaughters(); i++) {

          // Get daughter physical and logical volumes
          G4VPhysicalVolume* target_physical_volume = mother_logical_volume->GetDaughter(i);
          G4LogicalVolume* target_logical_volume = target_physical_volume->GetLogicalVolume();

          // Target volume must contain "Target" auxiliary tag as well
          //
          // TODO Seems like this shouldn't require an iteration over a map,
          // of all things, but I coulnd't get auxmap[target_logical_volume]
          // to work due to (unhelpful) compiler errors, probably related to
          // the use of the typedef instead of actual map. Something like a
          // for (G4GDMLAuxListType::const_iterator vit2 =
          //   auxmap[target_logical_volume].begin(); etc
          for(G4GDMLAuxMapType::const_iterator
              iter2  = auxmap->begin();
              iter2 != auxmap->end(); iter2++) {

            // Only the target logical volume is of interest
            if ((*iter2).first != target_logical_volume) continue;

            for (G4GDMLAuxListType::const_iterator
                 vit2  = (*iter2).second.begin();
                 vit2 != (*iter2).second.end(); vit2++) {

              // If the logical volume is tagged as "TargetSamplingVolume"
              if ((*vit2).type != "TargetSamplingVolume") continue;

              // Add target volume
              G4cout << "Adding target sampling volume "
                     << target_logical_volume->GetName() << "." << G4endl;
              remollBeamTarget::AddTargetVolume(target_physical_volume);

            } // loop over auxiliary tags in volume to find "TargetSamplingVolume"

          } // loop over volumes with auxiliary tags to find "TargetSamplingVolume"

        } // loop over daughter volumes in target system

      } // loop over auxiliary tags in volume to find "TargetSystem"

    } // loop over volumes with auxiliary tags to find "TargetSystem"
}

void remollDetectorConstruction::ParseAuxiliaryVisibilityInfo()
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
  motherVisAtt->SetVisibility(false);
  fWorldVolume->GetLogicalVolume()->SetVisAttributes(motherVisAtt);

  // Set all immediate daughters of the world volume to wireframe
  G4VisAttributes* daughterVisAtt = new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  daughterVisAtt->SetForceWireframe(true);
  for (int i = 0; i < fWorldVolume->GetLogicalVolume()->GetNoDaughters(); i++) {
    fWorldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->SetVisAttributes(daughterVisAtt);
  }
}

void remollDetectorConstruction::ParseAuxiliarySensDetInfo()
{
  //==========================
  // Sensitive detectors
  //==========================
  G4SDManager* SDman = G4SDManager::GetSDMpointer();

  G4int k=0;

  if (fVerboseLevel > 0)
      G4cout << "Beginning sensitive detector assignment" << G4endl;

  G4bool useddetnums[__MAX_DETS];
  for (k = 0; k < __MAX_DETS; k++ ){useddetnums[k] = false;}
  k = 0;

  const G4GDMLAuxMapType* auxmap = fGDMLParser->GetAuxMap();
  for (G4GDMLAuxMapType::const_iterator iter  = auxmap->begin(); iter != auxmap->end(); iter++) {
      G4LogicalVolume* myvol = (*iter).first;
      if (fVerboseLevel > 0)
          G4cout << "Volume " << myvol->GetName() << G4endl;

      for (G4GDMLAuxListType::const_iterator
          vit  = (*iter).second.begin();
          vit != (*iter).second.end(); vit++) {

          if ((*vit).type == "SensDet") {
              G4String det_type = (*vit).value;

              // Also allow specification of det number ///////////////////
              int det_no = -1;
              for (G4GDMLAuxListType::const_iterator
                  nit  = (*iter).second.begin();
                  nit != (*iter).second.end(); nit++) {

                  if ((*nit).type == "DetNo") {
                      det_no= atoi((*nit).value.data());
                      if( det_no >= __MAX_DETS ){
                          G4cerr << __FILE__ << " line " << __LINE__ << ": ERROR detector number too high" << G4endl;
                          exit(1);
                      }
                      useddetnums[det_no] = true;
                  }
              }
              if( det_no <= 0 ){
                  k = 1;
                  while( useddetnums[k] == true && k < __MAX_DETS ){ k++; }
                  if( k >= __MAX_DETS ){
                      G4cerr << __FILE__ << " line " << __LINE__ << ": ERROR too many detectors" << G4endl;
                      exit(1);
                  }
                  det_no = k;
                  useddetnums[k] = true;
              }
              /////////////////////////////////////////////////////////////

              char detectorname[__DET_STRLEN];
              int retval = snprintf(detectorname, __DET_STRLEN, "remoll/det_%d", det_no);

              assert( 0 < retval && retval < __DET_STRLEN ); // Ensure we're writing reasonable strings

              G4VSensitiveDetector* thisdet = SDman->FindSensitiveDetector(detectorname,(fVerboseLevel > 0));

              if( thisdet == 0 ) {
                  thisdet = new remollGenericDetector(detectorname, det_no);
                  if (fVerboseLevel > 0)
                      G4cout << "  Creating sensitive detector " << det_type
                          << " for volume " << myvol->GetName()
                          <<  G4endl << G4endl;
                  SDman->AddNewDetector(thisdet);
              }

              myvol->SetSensitiveDetector(thisdet);
          }
      }
  }

  if (fVerboseLevel > 0)
    G4cout << "Completed sensitive detector assignment" << G4endl;

}

G4VPhysicalVolume* remollDetectorConstruction::Construct()
{
  // Parse GDML file
  fWorldVolume = ParseGDMLFile();

  // Parse auxiliary info
  PrintAuxiliaryInfo();
  ParseAuxiliaryTargetInfo();
  ParseAuxiliaryVisibilityInfo();

  // Set copy number of geometry tree
  UpdateCopyNo(fWorldVolume,1);

  return fWorldVolume;
}

void remollDetectorConstruction::LoadMagneticField()
{
  // Remove existing field and load new field
  if (fGlobalField) delete fGlobalField;
  fGlobalField = new remollGlobalField();
}

void remollDetectorConstruction::ConstructSDandField()
{
  // Parse auxiliary info
  ParseAuxiliarySensDetInfo();

  // Load magnetic field
  LoadMagneticField();
}

void remollDetectorConstruction::ReloadGeometry(const G4String gdmlfile)
{
  // Set new geometry
  SetDetectorGeomFile(gdmlfile);

  // Trigger Construct and ConstructSDandField
  G4RunManager::GetRunManager()->ReinitializeGeometry(true);
}

G4int remollDetectorConstruction::UpdateCopyNo(G4VPhysicalVolume* aVolume,G4int index)
{
  //if (aVolume->GetLogicalVolume()->GetNoDaughters()==0 ){
      aVolume->SetCopyNo(index);
      index++;
      //}else {
    for(int i=0;i<aVolume->GetLogicalVolume()->GetNoDaughters();i++){
      index = UpdateCopyNo(aVolume->GetLogicalVolume()->GetDaughter(i),index);
    }
    //}

  return index;
}

void remollDetectorConstruction::PrintElements() {
  G4cout << G4endl << "Element table: " << G4endl << G4endl;
  G4cout << *(G4Element::GetElementTable()) << G4endl;
}

void remollDetectorConstruction::PrintMaterials() {
  G4cout << G4endl << "Material table: " << G4endl << G4endl;
  G4cout << *(G4Material::GetMaterialTable()) << G4endl;
}

std::vector<G4VPhysicalVolume*> remollDetectorConstruction::GetPhysicalVolumes(
    G4VPhysicalVolume* physical_volume,
    const G4LogicalVolume* logical_volume)
{
  // Create list of results
  std::vector<G4VPhysicalVolume*> list;

  // Store as result if the logical volume name agrees
  if (physical_volume->GetLogicalVolume() == logical_volume) {
    list.push_back(physical_volume);
  }

  // Descend down the tree
  for (int i = 0; i < physical_volume->GetLogicalVolume()->GetNoDaughters(); i++)
  {
    // Get results for daughter volumes
    std::vector<G4VPhysicalVolume*> daughter_list =
        GetPhysicalVolumes(physical_volume->GetLogicalVolume()->GetDaughter(i),logical_volume);
    // Add to the list of results
    list.insert(list.end(),daughter_list.begin(),daughter_list.end());
  }

  return list;
}

void remollDetectorConstruction::PrintGeometryTree(
    G4VPhysicalVolume* aVolume,
    G4int depth,
    G4bool surfchk)
{
  // Null volume
  if (aVolume == 0) aVolume = fWorldVolume;

  // Print spaces
  for (int isp = 0; isp < depth; isp++) { G4cout << "  "; }
  // Print name
  G4cout << aVolume->GetName() << "[" << aVolume->GetCopyNo() << "] "
         << aVolume->GetLogicalVolume()->GetName() << " "
         << aVolume->GetLogicalVolume()->GetNoDaughters() << " "
         << aVolume->GetLogicalVolume()->GetMaterial()->GetName() << " "
	 << G4BestUnit(aVolume->GetLogicalVolume()->GetMass(true),"Mass");
  // Print sensitive detector
  if (aVolume->GetLogicalVolume()->GetSensitiveDetector())
  {
    G4cout << " " << aVolume->GetLogicalVolume()->GetSensitiveDetector()
                            ->GetFullPathName();
  }
  G4cout << G4endl;

  // Check overlapping volumes
  if (surfchk) aVolume->CheckOverlaps();

  // Descend down the tree
  for (int i = 0; i < aVolume->GetLogicalVolume()->GetNoDaughters(); i++) {
    PrintGeometryTree(aVolume->GetLogicalVolume()->GetDaughter(i),depth+1,surfchk);
  }
}
