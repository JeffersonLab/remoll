#include "remollDetectorConstruction.hh"

#include "remollGenericDetector.hh"
#include "remollBeamTarget.hh"
#include "remollGlobalField.hh"
#include "remollIO.hh"

#include "G4GenericMessenger.hh"
#include "G4GeometryManager.hh"
#include "G4GeometryTolerance.hh"
#include "G4FieldManager.hh"
#include "G4TransportationManager.hh"
#include "G4UIcmdWithADoubleAndUnit.hh"
#include "G4UserLimits.hh"

#include "G4PhysicalVolumeStore.hh"
#include "G4LogicalVolumeStore.hh"
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

#include <algorithm>
#include <sys/param.h>

#include "G4Threading.hh"
#include "G4AutoLock.hh"
namespace { G4Mutex remollDetectorConstructionMutex = G4MUTEX_INITIALIZER; }

G4ThreadLocal remollGlobalField* remollDetectorConstruction::fGlobalField = 0;

G4UserLimits* remollDetectorConstruction::fKryptoniteUserLimits = new G4UserLimits(0,0,0,DBL_MAX,DBL_MAX);

remollDetectorConstruction::remollDetectorConstruction(const G4String& name, const G4String& gdmlfile)
: fVerboseLevel(0),
  fGDMLParser(0),
  fGDMLValidate(false),
  fGDMLOverlapCheck(false),
  fGDMLPath("geometry"),
  fGDMLFile("mollerMother.gdml"),
  fMessenger(0),
  fGeometryMessenger(0),
  fUserLimitsMessenger(0),
  fKryptoniteMessenger(0),
  fKryptoniteEnable(true),
  fKryptoniteVerbose(0),
  fWorldVolume(0),
  fWorldName(name)
{
  // If gdmlfile is non-empty
  if (gdmlfile.length() > 0) SetGDMLFile(gdmlfile);

  // Create GDML parser
  fGDMLParser = new G4GDMLParser();

  // Create generic messenger
  fMessenger = new G4GenericMessenger(this,"/remoll/","Remoll properties");
  fMessenger->DeclareMethod(
      "setgeofile",
      &remollDetectorConstruction::SetGDMLFile,
      "Set geometry GDML file")
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
  fGeometryMessenger->DeclareMethod(
      "setfile",
      &remollDetectorConstruction::SetGDMLFile,
      "Set geometry GDML file")
      .SetStates(G4State_PreInit);
  fGeometryMessenger->DeclareProperty(
      "verbose",
      fVerboseLevel,
      "Set geometry verbose level")
          .SetStates(G4State_PreInit,G4State_Idle);
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
  fGeometryMessenger->DeclareMethod(
      "printoverlaps",
      &remollDetectorConstruction::PrintOverlaps,
      "Print the geometry overlap")
      .SetStates(G4State_Idle);
  fGeometryMessenger->DeclareMethod(
      "absolute_position",
      &remollDetectorConstruction::AbsolutePosition,
      "Set the position of volume in parent frame [mm]")
      .SetStates(G4State_PreInit,G4State_Idle);
  fGeometryMessenger->DeclareMethod(
      "relative_position",
      &remollDetectorConstruction::RelativePosition,
      "Position a volume relative to current position [mm]")
      .SetStates(G4State_PreInit,G4State_Idle);
  fGeometryMessenger->DeclareMethod(
      "absolute_rotation",
      &remollDetectorConstruction::AbsoluteRotation,
      "Set the rotation of volume in parent frame [deg]")
      .SetStates(G4State_PreInit,G4State_Idle);
  fGeometryMessenger->DeclareMethod(
      "relative_rotation",
      &remollDetectorConstruction::RelativeRotation,
      "Rotate a volume relative to current orientation [deg]")
      .SetStates(G4State_PreInit,G4State_Idle);

  // Create user limits messenger
  fUserLimitsMessenger = new G4GenericMessenger(this,
      "/remoll/geometry/userlimits/",
      "Remoll geometry properties");
  fUserLimitsMessenger->DeclareMethod(
      "usermaxallowedstep",
      &remollDetectorConstruction::SetUserMaxAllowedStep,
      "Set user limit MaxAllowedStep for logical volume")
      .SetStates(G4State_Idle);
  fUserLimitsMessenger->DeclareMethod(
      "usermaxtracklength",
      &remollDetectorConstruction::SetUserMaxTrackLength,
      "Set user limit MaxTrackLength for logical volume")
      .SetStates(G4State_Idle);
  fUserLimitsMessenger->DeclareMethod(
      "usermaxtime",
      &remollDetectorConstruction::SetUserMaxTime,
      "Set user limit MaxTime for logical volume")
      .SetStates(G4State_Idle);
  fUserLimitsMessenger->DeclareMethod(
      "userminekine",
      &remollDetectorConstruction::SetUserMinEkine,
      "Set user limit MinEkine for logical volume")
      .SetStates(G4State_Idle);
  fUserLimitsMessenger->DeclareMethod(
      "userminrange",
      &remollDetectorConstruction::SetUserMinRange,
      "Set user limit MinRange for logical volume")
      .SetStates(G4State_Idle);

  // Create kryptonite messenger
  fKryptoniteMessenger = new G4GenericMessenger(this,
      "/remoll/kryptonite/",
      "Remoll kryptonite properties");
  fKryptoniteMessenger->DeclareMethod(
      "verbose",
      &remollDetectorConstruction::SetKryptoniteVerbose,
      "Set verbose level");
  fKryptoniteMessenger->DeclareMethod(
      "enable",
      &remollDetectorConstruction::EnableKryptonite,
      "Treat materials as kryptonite");
  fKryptoniteMessenger->DeclareMethod(
      "disable",
      &remollDetectorConstruction::DisableKryptonite,
      "Treat materials as regular");
  fKryptoniteMessenger->DeclareMethod(
      "add",
      &remollDetectorConstruction::AddKryptoniteCandidate,
      "Add specified material to list of kryptonite candidates");
  fKryptoniteMessenger->DeclareMethod(
      "list",
      &remollDetectorConstruction::ListKryptoniteCandidates,
      "List kryptonite candidate materials");
  fKryptoniteMessenger->DeclareMethod(
      "volume",
      &remollDetectorConstruction::EnableKryptoniteVolume,
      "Treat volume as kryptonite");
}

void remollDetectorConstruction::EnableKryptonite()
{
  if (fKryptoniteVerbose > 0)
    G4cout << "Enabling kryptonite." << G4endl;

  fKryptoniteEnable = true;

  SetKryptoniteUserLimits(fWorldVolume);
}

void remollDetectorConstruction::DisableKryptonite()
{
  if (fKryptoniteVerbose > 0)
    G4cout << "Disabling kryptonite." << G4endl;

  fKryptoniteEnable = false;

  SetKryptoniteUserLimits(fWorldVolume);
}

void remollDetectorConstruction::EnableKryptoniteVolume(G4String name)
{
  if (fKryptoniteVerbose > 0)
    G4cout << "Enabling kryptonite on volume" << name << "." << G4endl;

  // Find volume
  G4LogicalVolume* logical_volume = G4LogicalVolumeStore::GetInstance()->GetVolume(name);
  if (! logical_volume) {
    G4cerr << __FILE__ << " line " << __LINE__ << ": Warning volume " << name << " unknown" << G4endl;
    return;
  }

  fKryptoniteEnable = true;

  logical_volume->SetUserLimits(fKryptoniteUserLimits);
}

void remollDetectorConstruction::AddKryptoniteCandidate(G4String name)
{
  if (fKryptoniteVerbose > 0)
    G4cout << "Adding " << name << " to list of kryptonite candidates." << G4endl;

  fKryptoniteCandidates.insert(name);
  InitKryptoniteMaterials();

  SetKryptoniteUserLimits(fWorldVolume);
}

void remollDetectorConstruction::ListKryptoniteCandidates()
{
  G4cout << "List of kryptonite candidate materials:" << G4endl;
  for (std::set<G4String>::const_iterator
      it  = fKryptoniteCandidates.begin();
      it != fKryptoniteCandidates.end();
      it++)
    G4cout << *it << G4endl;
}

void remollDetectorConstruction::InitKryptoniteMaterials()
{
  if (fKryptoniteVerbose > 0)
    G4cout << "Regenerating table of kryptonite material candidate pointers..." << G4endl;

  // Find kryptonite materials in material tables
  G4MaterialTable* table = G4Material::GetMaterialTable();
  fKryptoniteMaterials.clear();
  for (G4MaterialTable::const_iterator
      it  = table->begin();
      it != table->end(); it++) {
    if (fKryptoniteCandidates.find((*it)->GetName()) != fKryptoniteCandidates.end()) {
      fKryptoniteMaterials.insert(*it);
    }
  }
}

void remollDetectorConstruction::SetKryptoniteUserLimits(G4VPhysicalVolume* volume)
{
  // If null volume, pick entire world
  if (volume == 0) volume = fWorldVolume;
  // If still null, give up
  if (volume == 0) return;

  // Get logical volume
  G4LogicalVolume* logical_volume = volume->GetLogicalVolume();
  G4Material* material = logical_volume->GetMaterial();

  // Set user limits for all materials in kryptonite materials list
  if (fKryptoniteMaterials.count(material) > 0) {
    if (fKryptoniteVerbose > 0)
      G4cout << "Setting kryptonite for " << logical_volume->GetName() << " to " <<
        (fKryptoniteEnable?"on":"off") << G4endl;

    if (fKryptoniteEnable)
      logical_volume->SetUserLimits(fKryptoniteUserLimits);
    else
      logical_volume->SetUserLimits(0);
  }

  // Descend down the tree
  auto n = logical_volume->GetNoDaughters();
  for (decltype(n) i = 0; i < n; i++) {
    G4VPhysicalVolume* daughter = logical_volume->GetDaughter(i);
    SetKryptoniteUserLimits(daughter);
  }
}


// Set of functions that passes function name as string for further processing
void remollDetectorConstruction::SetUserMaxAllowedStep(G4String name, G4String value_units)
{
  SetUserLimits(__FUNCTION__,name,value_units);
}
void remollDetectorConstruction::SetUserMaxTrackLength(G4String name, G4String value_units)
{
  SetUserLimits(__FUNCTION__,name,value_units);
}
void remollDetectorConstruction::SetUserMaxTime(G4String name, G4String value_units)
{
  SetUserLimits(__FUNCTION__,name,value_units);
}
void remollDetectorConstruction::SetUserMinEkine(G4String name, G4String value_units)
{
  SetUserLimits(__FUNCTION__,name,value_units);
}
void remollDetectorConstruction::SetUserMinRange(G4String name, G4String value_units)
{
  SetUserLimits(__FUNCTION__,name,value_units);
}

remollDetectorConstruction::~remollDetectorConstruction()
{
    delete fGDMLParser;
    delete fMessenger;
    delete fGeometryMessenger;
    delete fKryptoniteMessenger;
    delete fUserLimitsMessenger;
}

void remollDetectorConstruction::AbsolutePosition(G4String name, G4ThreeVector position)
{
  // Units
  position *= CLHEP::mm;

  // Find volume
  G4VPhysicalVolume* physical_volume = G4PhysicalVolumeStore::GetInstance()->GetVolume(name);
  if (! physical_volume) {
    G4cerr << __FILE__ << " line " << __LINE__ << ": Warning volume " << name << " unknown" << G4endl;
    return;
  }

  // Print verbose
  if (fVerboseLevel > 0)
    G4cout << "Setting position in mother volume "
           << "from " << physical_volume->GetTranslation() << " "
           << "to " << position << " for " << name << G4endl;

  // Set position for volume
  physical_volume->SetTranslation(position);

  // Reoptimize geometry
  G4RunManager* run_manager = G4RunManager::GetRunManager();
  run_manager->GeometryHasBeenModified();
}

void remollDetectorConstruction::RelativePosition(G4String name, G4ThreeVector position)
{
  // Units
  position *= CLHEP::mm;

  // Find volume
  G4VPhysicalVolume* physical_volume = G4PhysicalVolumeStore::GetInstance()->GetVolume(name);
  if (! physical_volume) {
    G4cerr << __FILE__ << " line " << __LINE__ << ": Warning volume " << name << " unknown" << G4endl;
    return;
  }

  // Print verbose
  if (fVerboseLevel > 0)
    G4cout << "Changing position in mother volume "
           << "from " << physical_volume->GetTranslation() << " "
           << "by " << position << " for " << name << G4endl;

  // Set position for volume
  physical_volume->SetTranslation(physical_volume->GetTranslation() + position);

  // Reoptimize geometry
  G4RunManager* run_manager = G4RunManager::GetRunManager();
  run_manager->GeometryHasBeenModified();
}

void remollDetectorConstruction::AbsoluteRotation(G4String name, G4ThreeVector rotation_xyz)
{
  // Units
  rotation_xyz *= CLHEP::deg;

  // Find volume
  G4VPhysicalVolume* physical_volume = G4PhysicalVolumeStore::GetInstance()->GetVolume(name);
  if (! physical_volume) {
    G4cerr << __FILE__ << " line " << __LINE__ << ": Warning volume " << name << " unknown" << G4endl;
    return;
  }

  // Construct rotation matrix
  G4RotationMatrix* rotation = new G4RotationMatrix();
  rotation->rotateX(rotation_xyz.x());
  rotation->rotateY(rotation_xyz.y());
  rotation->rotateZ(rotation_xyz.z());

  // Get previous rotation matrix
  G4RotationMatrix* old_rotation = physical_volume->GetRotation();
  if (old_rotation == 0) old_rotation = new G4RotationMatrix();

  // Print verbose
  if (fVerboseLevel > 0)
    G4cout << "Setting rotation in mother volume "
           << "from " << *old_rotation << " "
           << "to " << *rotation << " for " << name << G4endl;

  // Set position for volume
  physical_volume->SetRotation(rotation);

  // Delete old rotation matrix
  delete old_rotation;

  // Reoptimize geometry
  G4RunManager* run_manager = G4RunManager::GetRunManager();
  run_manager->GeometryHasBeenModified();
}

void remollDetectorConstruction::RelativeRotation(G4String name, G4ThreeVector rotation_xyz)
{
  // Units
  rotation_xyz *= CLHEP::deg;

  // Find volume
  G4VPhysicalVolume* physical_volume = G4PhysicalVolumeStore::GetInstance()->GetVolume(name);
  if (! physical_volume) {
    G4cerr << __FILE__ << " line " << __LINE__ << ": Warning volume " << name << " unknown" << G4endl;
    return;
  }

  // Get previous rotation matrix
  G4RotationMatrix* old_rotation = physical_volume->GetRotation();
  if (old_rotation == 0) old_rotation = new G4RotationMatrix();

  // Apply relative rotation
  G4RotationMatrix* rotation = new G4RotationMatrix(*old_rotation);
  rotation->rotateX(rotation_xyz.x());
  rotation->rotateY(rotation_xyz.y());
  rotation->rotateZ(rotation_xyz.z());

  // Print verbose
  if (fVerboseLevel > 0)
    G4cout << "Setting rotation in mother volume "
           << "from " << *old_rotation << " "
           << "to " << *rotation << " for " << name << G4endl;

  // Set position for volume
  physical_volume->SetRotation(rotation);

  // Delete old rotation matrix
  delete old_rotation;

  // Reoptimize geometry
  G4RunManager* run_manager = G4RunManager::GetRunManager();
  run_manager->GeometryHasBeenModified();
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
    fGDMLParser->Clear();

    // Print GDML warning
    PrintGDMLWarning();

    // Print parsing options
    G4cout << "Reading " << fGDMLFile << G4endl;
    G4cout << "- schema validation " << (fGDMLValidate? "on": "off") << G4endl;
    G4cout << "- overlap check " << (fGDMLOverlapCheck? "on": "off") << G4endl;

    // Get remollIO instance before chdir since remollIO creates root file
    remollIO* io = remollIO::GetInstance();

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
    G4VPhysicalVolume* worldvolume = fGDMLParser->GetWorldVolume();

    // Print tolerances
    if (fVerboseLevel > 0) {
      G4cout << "Computed surface tolerance = "
             << G4GeometryTolerance::GetInstance()->GetSurfaceTolerance()/mm
             << " mm" << G4endl;
      G4cout << "Computed angular tolerance = "
             << G4GeometryTolerance::GetInstance()->GetAngularTolerance()/rad
             << " rad" << G4endl;
      G4cout << "Computed radial tolerance = "
             << G4GeometryTolerance::GetInstance()->GetRadialTolerance()/mm
             << " mm" << G4endl;
    }

    // Print overlaps
    if (fGDMLOverlapCheck)
      PrintGeometryTree(worldvolume,0,true,false);

    // Add GDML files to IO
    io->GrabGDMLFiles(fGDMLFile);

    // Change directory back
    if (chdir(cwd)) {
      G4cerr << __FILE__ << " line " << __LINE__ << ": ERROR cannot change directory" << G4endl;
      exit(-1);
    }

    // Return world volume
    return worldvolume;
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
        if (fVerboseLevel > 0)
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

          if (fVerboseLevel > 0)
            G4cout << "Found target mother physical volume "
                   << mother_physical_volume->GetName() << "." << G4endl;
        } else {
          G4cout << "Target mother logical volume does not occur "
                 << "*exactly once* as a physical volume." << G4endl;
          exit(-1);
        }

        // Loop over target mother logical volume daughters
        auto n = mother_logical_volume->GetNoDaughters();
        for (decltype(n) i = 0; i < n; i++) {

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

void remollDetectorConstruction::ParseAuxiliaryUserLimits()
{
  const G4GDMLAuxMapType* auxmap = fGDMLParser->GetAuxMap();
  for(G4GDMLAuxMapType::const_iterator
      iter  = auxmap->begin();
      iter != auxmap->end(); iter++) {

    if (fVerboseLevel > 0)
      G4cout << "Volume " << ((*iter).first)->GetName()
             << " has the following list of auxiliary information: "<< G4endl;

    // Loop over auxiliary tags for this logical volume
    G4LogicalVolume* logical_volume = (*iter).first;
    for (G4GDMLAuxListType::const_iterator
        vit  = (*iter).second.begin();
        vit != (*iter).second.end(); vit++) {

      if (fVerboseLevel > 0)
        G4cout << "--> Type: " << (*vit).type
	       << " Value: "   << (*vit).value << std::endl;

      // Skip if not starting with "User"
      if (! (*vit).type.contains("User")) continue;

      // Set user limits
      SetUserLimits(logical_volume, (*vit).type, (*vit).value);
    }
  }

  if (fVerboseLevel > 0)
      G4cout << G4endl << G4endl;
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


  // Set the world volume to wireframe
  G4VisAttributes* motherVisAtt = new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  motherVisAtt->SetForceWireframe(true);
  fWorldVolume->GetLogicalVolume()->SetVisAttributes(motherVisAtt);
}

void remollDetectorConstruction::ParseAuxiliarySensDetInfo()
{
  if (fVerboseLevel > 0)
      G4cout << "Beginning sensitive detector assignment" << G4endl;

  // Duplication map
  std::map<int, G4LogicalVolume*> detnomap;

  // Loop over all volumes with auxiliary tags
  const G4GDMLAuxMapType* auxmap = fGDMLParser->GetAuxMap();
  for (G4GDMLAuxMapType::const_iterator iter  = auxmap->begin(); iter != auxmap->end(); iter++) {

      G4LogicalVolume* myvol = (*iter).first;
      G4GDMLAuxListType list = (*iter).second;

      if (fVerboseLevel > 0)
        G4cout << "Volume " << myvol->GetName() << G4endl;

      remollGenericDetector* remollsd = 0;

      // Find first aux list entry with type SensDet
      auto it_sensdet = NextAuxWithType(list.begin(), list.end(), "SensDet");
      if (it_sensdet != list.end()) {

        // Find first aux list entry with type DetNo
        auto it_detno = NextAuxWithType(list.begin(), list.end(), "DetNo");
        if (it_detno != list.end()) {

          int det_no = atoi(it_detno->value.data());
          bool enabled = (det_no > 0)? false : true;
          det_no = std::abs(det_no);

          // Construct detector name
          std::stringstream det_name_ss;
          det_name_ss << "remoll/det_" << det_no;
          std::string det_name = det_name_ss.str();

          // Check for duplication
          if (detnomap.count(det_no) != 0) {
            G4cerr << "remoll: DetNo " << det_no << " for " << myvol->GetName() << G4endl;
            G4cerr << "remoll: already used by " << detnomap[det_no]->GetName() << G4endl;
          }

          // Try to find sensitive detector
          G4SDManager* SDman = G4SDManager::GetSDMpointer();
          G4VSensitiveDetector* sd = SDman->FindSensitiveDetector(det_name, (fVerboseLevel > 0));
          // and cast into remoll sensitive detector
          remollsd = dynamic_cast<remollGenericDetector*>(sd);

          // No such detector yet
          if (remollsd == 0) {

            if (fVerboseLevel > 0)
              G4cout << "  Creating sensitive detector "
                     << "for volume " << myvol->GetName()
                     <<  G4endl;

            remollsd = new remollGenericDetector(det_name, det_no);
            remollsd->SetEnabled(enabled);

            // Register detector with SD manager
            SDman->AddNewDetector(remollsd);
            detnomap[det_no] = myvol;

            // Register detector with remollIO
            remollIO* io = remollIO::GetInstance();
            io->RegisterDetector(myvol->GetName(), it_sensdet->value, det_no);

          }

          // Register detector with this volume
          myvol->SetSensitiveDetector(remollsd);

        } // end of if aux tag with type DetNo

      } // end of if aux tag with type SensDet


      // Find aux list entries with type DetType
      for (auto it_dettype  = NextAuxWithType(list.begin(), list.end(), "DetType");
                it_dettype != list.end();
                it_dettype  = NextAuxWithType(++it_dettype, list.end(), "DetType")) {

        // Set detector type
        if (remollsd) remollsd->SetDetectorType(it_dettype->value);

        // Print detector type
        if (fVerboseLevel > 0)
          if (remollsd) remollsd->PrintDetectorType();

      }

  } // end of loop over volumes

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
  ParseAuxiliaryUserLimits();
  ParseAuxiliaryVisibilityInfo();

  // Set copy number of geometry tree
  UpdateCopyNo(fWorldVolume,1);

  // Set kryptonite user limits
  InitKryptoniteMaterials();
  SetKryptoniteUserLimits(fWorldVolume);

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


void remollDetectorConstruction::SetUserLimits(
    const G4String& set_type,
    const G4String& name,
    const G4String& value_units) const
{
  // Find volume
  G4LogicalVolume* logical_volume = G4LogicalVolumeStore::GetInstance()->GetVolume(name);
  if (! logical_volume) {
    G4cerr << __FILE__ << " line " << __LINE__ << ": Warning volume " << name << " unknown" << G4endl;
    return;
  }

  // Remove starting "Set" used by commands
  G4String type = set_type;
  if (type.find("Set") == 0) type.erase(0,3);

  if (fVerboseLevel > 0)
    G4cout << "Setting user limit " << type << " for " << name << G4endl;

  // Set user limits
  SetUserLimits(logical_volume, type, value_units);
}

void remollDetectorConstruction::SetUserLimits(
    G4LogicalVolume* logical_volume,
    const G4String& type,
    const G4String& value_units) const
{
  // Get user limits
  G4UserLimits* userlimits = logical_volume->GetUserLimits();
  if (! userlimits) {
    userlimits = new G4UserLimits();
    logical_volume->SetUserLimits(userlimits);
  }

  // Set user limits
  SetUserLimits(userlimits, type, value_units);
}

void remollDetectorConstruction::SetUserLimits(
    G4UserLimits* userlimits,
    const G4String& type,
    const G4String& value_units) const
{
  if (fVerboseLevel > 0)
    G4cout << "Setting user limit " << type << " to " << value_units << G4endl;

  // Resolve units in value_units
  G4String value_space_units = value_units;
  std::replace(value_space_units.begin(), value_space_units.end(), '*', ' ');
  G4double value = G4UIcmdWithADoubleAndUnit::GetNewDoubleValue(value_space_units);

  // Compare with allowed types while ignoring case
  if      (type.compareTo("usermaxallowedstep", G4String::ignoreCase) == 0)
    userlimits->SetMaxAllowedStep(value);
  else if (type.compareTo("usermaxtracklength", G4String::ignoreCase) == 0)
    userlimits->SetUserMaxTrackLength(value);
  else if (type.compareTo("usermaxtime", G4String::ignoreCase) == 0)
    userlimits->SetUserMaxTime(value);
  else if (type.compareTo("userminekine", G4String::ignoreCase) == 0)
    userlimits->SetUserMinEkine(value);
  else if (type.compareTo("userminrange", G4String::ignoreCase) == 0)
    userlimits->SetUserMinRange(value);
  else
    G4cerr << __FILE__ << " line " << __LINE__ << ": Warning user type " << type << " unknown" << G4endl;
}

void remollDetectorConstruction::ReloadGeometry(const G4String gdmlfile)
{
  // Set new geometry
  SetGDMLFile(gdmlfile);

  // Trigger Construct and ConstructSDandField
  G4RunManager::GetRunManager()->ReinitializeGeometry(true);
}

G4int remollDetectorConstruction::UpdateCopyNo(G4VPhysicalVolume* aVolume,G4int index)
{
  //if (aVolume->GetLogicalVolume()->GetNoDaughters()==0 ){
      aVolume->SetCopyNo(index);
      G4Material* material;
      G4VisAttributes* kryptoVisAtt= new G4VisAttributes(G4Colour(0.7,0.0,0.0));
      //set user limits for Kryptonite materials. When tracks are killed inside Kryptonite materials, energy will be properly deposited
      material = aVolume->GetLogicalVolume()->GetMaterial();
      if (material->GetName() == "Kryptonite") {
	aVolume->GetLogicalVolume()->SetUserLimits( new G4UserLimits(0.0, 0.0, 0.0, DBL_MAX, DBL_MAX) );
	aVolume->GetLogicalVolume()->SetVisAttributes(kryptoVisAtt);
      }
      index++;
      //}else {
    auto n = aVolume->GetLogicalVolume()->GetNoDaughters();
    for(decltype(n) i=0;i<n;i++){
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
  auto n = physical_volume->GetLogicalVolume()->GetNoDaughters();
  for (decltype(n) i = 0; i < n; i++)
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
    G4bool surfchk,
    G4bool print)
{
  // Null volume
  if (aVolume == 0) aVolume = fWorldVolume;

  // Print spaces
  if (print) {
    for (int isp = 0; isp < depth; isp++) { G4cout << "  "; }
  }
  // Print name
  if (print) {
    G4cout << aVolume->GetName() << "[" << aVolume->GetCopyNo() << "] "
           << aVolume->GetLogicalVolume()->GetName() << " "
           << aVolume->GetLogicalVolume()->GetNoDaughters() << " "
           << aVolume->GetLogicalVolume()->GetMaterial()->GetName() << " "
           << G4BestUnit(aVolume->GetLogicalVolume()->GetMass(true),"Mass");
  }
  // Print sensitive detector
  G4VSensitiveDetector* sd = aVolume->GetLogicalVolume()->GetSensitiveDetector();
  if (print && sd)
  {
    remollGenericDetector* remollsd = dynamic_cast<remollGenericDetector*>(sd);
    G4cout << " [" << remollsd->GetDetNo() << "]";
  }
  if (print) {
    G4cout << G4endl;
  }

  // Check overlapping volumes (tolerance of 1 mm)
  if (surfchk) aVolume->CheckOverlaps(1000,1.0*mm,false);

  // Descend down the tree
  auto n = aVolume->GetLogicalVolume()->GetNoDaughters();
  for (decltype(n) i = 0; i < n; i++) {
    PrintGeometryTree(aVolume->GetLogicalVolume()->GetDaughter(i),depth+1,surfchk,print);
  }
}
