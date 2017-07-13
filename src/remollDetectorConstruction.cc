

#include "remollDetectorConstruction.hh"
#include "remollGenericDetector.hh"
#include "remollBeamTarget.hh"
#include "remollGlobalField.hh"
#include "remollRun.hh"
#include "remollRunData.hh"
#include "remollIO.hh"

#include "TGeoManager.h"

#include "G4FieldManager.hh"
#include "G4TransportationManager.hh"
#include "G4RunManager.hh"

#include "G4Material.hh"
#include "G4Element.hh"
#include "G4NistManager.hh"

#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "G4UserLimits.hh"
#include "globals.hh"

#include "G4SDManager.hh"
#include "G4VSensitiveDetector.hh"

#include "G4UImanager.hh"
#include "G4UIcommand.hh"

#include "G4ios.hh"

#include "G4UnitsTable.hh"

// GDML export
#include "G4GDMLParser.hh"

//visual
#include "G4VisAttributes.hh"
#include "G4Colour.hh"

#define __DET_STRLEN 200
#define __MAX_DETS 5000

remollDetectorConstruction::remollDetectorConstruction() {
    // Default geometry file
    fDetFileName = "geometry_sculpt/mollerMother.gdml";

    CreateGlobalMagneticField();

    fIO = NULL;
    fGDMLParser = NULL;
    fWorldVolume = NULL;
}

remollDetectorConstruction::~remollDetectorConstruction() {
}

G4VPhysicalVolume* remollDetectorConstruction::Construct() {
    G4VPhysicalVolume *worldVolume;

    fIO->GrabGDMLFiles(fDetFileName);

    if( fGDMLParser ){
	delete fGDMLParser;
    }
    fGDMLParser = new G4GDMLParser();
    fGDMLParser->Clear();
    fGDMLParser->SetOverlapCheck(false);

    fprintf(stdout, "Reading %s\n", fDetFileName.data());


    fGDMLParser->Read(fDetFileName);

    worldVolume = fGDMLParser->GetWorldVolume();
    

    //==========================
    // List auxiliary info
    //==========================

    const G4GDMLAuxMapType* auxmap = fGDMLParser->GetAuxMap();
    G4cout << "Found " << auxmap->size()
               << " volume(s) with auxiliary information."
               << G4endl << G4endl;

    //====================================================
    // Associate target volumes with beam/target class
    //====================================================

    // Get beam/target singleton
    remollBeamTarget *beamtarg = remollBeamTarget::GetBeamTarget();
    // Reset target volume assignments
    beamtarg->Reset();

    // Loop over volumes with auxiliary information
    for(G4GDMLAuxMapType::const_iterator
        iter  = auxmap->begin();
        iter != auxmap->end(); iter++) {

      // Loop over auxiliary tags for this logical volume
      G4LogicalVolume* logical_volume = (*iter).first;
      for (G4GDMLAuxListType::const_iterator
          vit  = (*iter).second.begin();
          vit != (*iter).second.end(); vit++) {

        // Treat auxiliary type "TargetSystem"
        if ((*vit).type == "TargetSystem") {
          // Found target mother logical volume
          G4LogicalVolume* mother_logical_volume = logical_volume;
          G4cout << "Found target mother logical volume "
              << mother_logical_volume->GetName() << "." << G4endl;

          // Now find target mother physical volume
          G4VPhysicalVolume* mother_physical_volume = 0;
          std::vector<G4VPhysicalVolume*> list =
              GetPhysicalVolumes(worldVolume,mother_logical_volume);
          if (list.size() == 1) {
            mother_physical_volume = list[0];
            beamtarg->SetMotherVolume(mother_physical_volume);
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
              if ((*iter2).first == target_logical_volume) {
                for (G4GDMLAuxListType::const_iterator
                    vit2  = (*iter2).second.begin();
                    vit2 != (*iter2).second.end(); vit2++) {

                  // If the logical volume is tagged as "TargetSamplingVolume"
                  if ((*vit2).type == "TargetSamplingVolume") {

                    // Add target volume
                    G4cout << "Adding target sampling volume "
                        << target_logical_volume->GetName() << "." << G4endl;
                    beamtarg->AddVolume(target_physical_volume);
                  }
                }
              }
            }
          }
        }
      }
    }


  G4cout << "Found " << auxmap->size()
         << " volume(s) with auxiliary information."
	 << G4endl << G4endl;
  for(G4GDMLAuxMapType::const_iterator
	  iter  = auxmap->begin();
	  iter != auxmap->end(); iter++) {
      G4cout << "Volume " << ((*iter).first)->GetName()
	     << " has the following list of auxiliary information: "<< G4endl;
      for (G4GDMLAuxListType::const_iterator
	      vit  = (*iter).second.begin();
	      vit != (*iter).second.end(); vit++) {
	G4cout << "--> Type: " << (*vit).type
	       << " Value: "   << (*vit).value << std::endl;

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

        if ((*vit).type == "Color") {
          G4Colour colour(1.0,1.0,1.0);
          if (G4Colour::GetColour((*vit).value, colour)) {
            G4cout << "Setting color to " << (*vit).value << "." << G4endl;
            G4VisAttributes visAttribute(colour);
            ((*iter).first)->SetVisAttributes(visAttribute);
          } else {
            G4cout << "Colour " << (*vit).value << " is not known." << G4endl;
          }
        }

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
  G4cout << G4endl<< G4endl;


  //==========================
  // Sensitive detectors
  //==========================
  G4SDManager* SDman = G4SDManager::GetSDMpointer();
  char detectorname[__DET_STRLEN];
  int retval;

  G4VSensitiveDetector* thisdet;

  G4int k=0;

  G4GDMLAuxMapType::const_iterator iter;
  G4GDMLAuxListType::const_iterator vit, nit;

  G4cout << "Beginning sensitive detector assignment" << G4endl;

  G4bool useddetnums[__MAX_DETS];
  for( k = 0; k < __MAX_DETS; k++ ){useddetnums[k] = false;}
  k = 0;

  for( iter  = auxmap->begin(); iter != auxmap->end(); iter++) {
      G4LogicalVolume* myvol = (*iter).first;
      G4cout << "Volume " << myvol->GetName() << G4endl;

      for( vit  = (*iter).second.begin(); vit != (*iter).second.end(); vit++) {
	  if ((*vit).type == "SensDet") {
	      G4String det_name = (*vit).value;

	      // Also allow specification of det number ///////////////////
	      int det_no = -1;
	      for( nit  = (*iter).second.begin(); nit != (*iter).second.end(); nit++) {
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

	      // Also allow specification of det type ///////////////////
              G4String det_type = -1;
              for( nit  = (*iter).second.begin(); nit != (*iter).second.end(); nit++) {
                  if ((*nit).type == "DetType") {
                      det_type = (*nit).value;
                  }
              }
	      /////////////////////////////////////////////////////////////

	      retval = snprintf(detectorname, __DET_STRLEN,"remoll/det_%d", det_no);

	      assert( 0 < retval && retval < __DET_STRLEN ); // Ensure we're writing reasonable strings

	      thisdet = SDman->FindSensitiveDetector(detectorname);

	      if( thisdet == 0 ) {
	          remollGenericDetector* newdet
	              = new remollGenericDetector(detectorname, det_no);
		  newdet->SetDetectorType(det_type);
                  thisdet = newdet;
		  G4cout << "  Creating sensitive detector " << det_name
		      << " for volume " << myvol->GetName()
		      <<  G4endl << G4endl;
		  SDman->AddNewDetector(newdet);
	      }

	      myvol->SetSensitiveDetector(thisdet);
	  }
      }
  }

  G4cout << "Completed sensitive detector assignment" << G4endl;


  //==========================
  // Visualization attributes
  //==========================

  G4VisAttributes* motherVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  motherVisAtt->SetVisibility(false);
  worldVolume->GetLogicalVolume()->SetVisAttributes(motherVisAtt);

  G4VisAttributes* daughterVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  daughterVisAtt->SetForceWireframe (true);
  for(int i=0;i<worldVolume->GetLogicalVolume()->GetNoDaughters();i++){
      worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->SetVisAttributes(daughterVisAtt);
  }

  //==========================
  // Output geometry tree
  //==========================

  G4cout << G4endl << "Element table: " << G4endl << G4endl;
  G4cout << *(G4Element::GetElementTable()) << G4endl;

  G4cout << G4endl << "Material table: " << G4endl << G4endl;
  G4cout << *(G4Material::GetMaterialTable()) << G4endl;

  UpdateCopyNo(worldVolume,1); 
    
  fWorldVolume = worldVolume;

  //commented out the below dump geometry routine to save terminal output length 
  //G4cout << G4endl << "Geometry tree: " << G4endl << G4endl;
  //DumpGeometricalTree(worldVolume);
  
  G4cout << G4endl << "###### Leaving remollDetectorConstruction::Read() " << G4endl << G4endl;

  return fWorldVolume;
}

G4int remollDetectorConstruction::UpdateCopyNo(G4VPhysicalVolume* aVolume,G4int index){  

  //if (aVolume->GetLogicalVolume()->GetNoDaughters()==0 ){
      aVolume->SetCopyNo(index);
      index++;
      //}else {
    for(int i=0;i<aVolume->GetLogicalVolume()->GetNoDaughters();i++){
      index = UpdateCopyNo(aVolume->GetLogicalVolume()->GetDaughter(i),index);
    }
    //}

  return index;
};

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

void remollDetectorConstruction::DumpGeometricalTree(
    G4VPhysicalVolume* aVolume,
    G4int depth,
    G4bool surfchk)
{
  // Null volume
  if (aVolume == 0) aVolume = fWorldVolume;

  // Print spaces
  for(int isp=0;isp<depth;isp++)
  { G4cout << "  "; }
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
  for(int i=0;i<aVolume->GetLogicalVolume()->GetNoDaughters();i++)
  { DumpGeometricalTree(aVolume->GetLogicalVolume()->GetDaughter(i),depth+1,surfchk); }
}

void remollDetectorConstruction::CreateGlobalMagneticField() {
    fGlobalField = new remollGlobalField();

    fGlobalFieldManager = G4TransportationManager::GetTransportationManager()->GetFieldManager();
    fGlobalFieldManager->SetDetectorField(fGlobalField);
    fGlobalFieldManager->CreateChordFinder(fGlobalField);

    return;
} 

void remollDetectorConstruction::SetDetectorGeomFile(const G4String &str){
    fDetFileName = str;
}
