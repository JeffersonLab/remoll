

#include "remollDetectorConstruction.hh"
#include "remollGenericDetector.hh"
#include "remollGlobalField.hh"

#include "G4FieldManager.hh"
#include "G4TransportationManager.hh"

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

// GDML export
#include "G4GDMLParser.hh"

//visual
#include "G4VisAttributes.hh"
#include "G4Colour.hh"

#define __DET_STRLEN 200

remollDetectorConstruction::remollDetectorConstruction() {
}

remollDetectorConstruction::~remollDetectorConstruction() {
}

G4VPhysicalVolume* remollDetectorConstruction::Construct() {

    fGDMLParser.SetOverlapCheck(false);
    fGDMLParser.Read(fDetFileName);

    worldVolume = fGDMLParser.GetWorldVolume();

  //==========================
  // List auxiliary info
  //==========================

  const G4GDMLAuxMapType* auxmap = fGDMLParser.GetAuxMap();

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

  for( iter  = auxmap->begin(); iter != auxmap->end(); iter++) {
      G4LogicalVolume* myvol = (*iter).first;
      G4cout << "Volume " << myvol->GetName();

      for( vit  = (*iter).second.begin(); vit != (*iter).second.end(); vit++) {
	  if ((*vit).type == "SensDet") {
	      G4String det_type = (*vit).value;

	      // Also allow specification of det number ///////////////////
	      int det_no = -1;
	      for( nit  = (*iter).second.begin(); nit != (*iter).second.end(); vit++) {
		  if ((*nit).type == "DetNo") {
		      det_no= atoi((*nit).value.data());
		  }
	      }
	      if( det_no <= 0 ){
		  det_no = k+1;
		  k++;
	      }
	      /////////////////////////////////////////////////////////////

	      retval = snprintf(detectorname, __DET_STRLEN,"/det_%d", det_no);
	      assert( 0 < retval && retval < __DET_STRLEN ); // Ensure we're writing reasonable strings

	      thisdet = SDman->FindSensitiveDetector(detectorname);

	      if( thisdet == 0 ) {
		  thisdet = new remollGenericDetector(detectorname);
		  G4cout << "  Creating sensitive detector " << det_type
		      << " for volume " << myvol->GetName()
		      <<  G4endl << G4endl;
		  SDman->AddNewDetector(thisdet);
	      }

	      myvol->SetSensitiveDetector(thisdet);
	  }
      }
  }

  CreateGlobalMagneticField();

  //==========================
  // Visualization attributes
  //==========================

  G4VisAttributes* motherVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  worldVolume->GetLogicalVolume()->SetVisAttributes(motherVisAtt);

  //==========================
  // Output geometry tree
  //==========================

  G4cout << G4endl << "Element table: " << G4endl << G4endl;
  G4cout << *(G4Element::GetElementTable()) << G4endl;

  G4cout << G4endl << "Material table: " << G4endl << G4endl;
  G4cout << *(G4Material::GetMaterialTable()) << G4endl;

  G4cout << G4endl << "Geometry tree: " << G4endl << G4endl;


  G4cout << G4endl << "###### Leaving remollDetectorConstruction::Read() " << G4endl << G4endl;

  return worldVolume;
}


void remollDetectorConstruction::CreateGlobalMagneticField() {
    fGlobalField = new remollGlobalField();

    fGlobalFieldManager = G4TransportationManager::GetTransportationManager()->GetFieldManager();
    fGlobalFieldManager->SetDetectorField(fGlobalField);
    fGlobalFieldManager->CreateChordFinder(fGlobalField);

    return;
} 

