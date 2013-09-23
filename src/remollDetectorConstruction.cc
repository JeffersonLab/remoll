

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

// GDML export
#include "G4GDMLParser.hh"

//visual
#include "G4VisAttributes.hh"
#include "G4Colour.hh"

#define __DET_STRLEN 200
#define __MAX_DETS 500

remollDetectorConstruction::remollDetectorConstruction() {
    // Default geometry file
    fDetFileName = "geometry/mollerMother.gdml";


    CreateGlobalMagneticField();
    fIO = NULL;
    fGDMLParser = NULL;
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
    
  //====================================================
  // Associate target volumes with beam/target class
  // This has to match what is declared in the GDML volumes
  // We absolutely need some connection between the geometry
  // structure and having access to the physical volumes.  
  // This could be made more general with a full treesearch
  //====================================================

    remollBeamTarget *beamtarg = remollBeamTarget::GetBeamTarget();
    beamtarg->Reset();
    G4LogicalVolume *thislog = worldVolume->GetLogicalVolume();
    G4int vidx = 0;

    G4String targetmothername = "logicTarget";
    while( vidx < thislog->GetNoDaughters() ){
	if( thislog->GetDaughter(vidx)->GetName() == targetmothername.append("_PV")) break;
	vidx++; 
    }
    if( vidx == thislog->GetNoDaughters() ){
	G4cerr << "WARNING " << __PRETTY_FUNCTION__ << " line " << __LINE__ <<
	    ":  target definition structure in GDML not valid" << G4endl;
    } else {
	beamtarg->SetMotherVolume(thislog->GetDaughter(vidx));

	thislog = thislog->GetDaughter(vidx)->GetLogicalVolume();


	////////////////////////////////////////////////////////////////////////////////
	// List relevant target volumes here terminated by "" //////////////////////////
	// FIXME:  This could probably be done better with auxiliary information
	//         though that only gives us *logical* volumes and we need the physical
	//         volumes for placement information
	//
	//         *ORDERING IS IMPORTANT - MUST GO UPSTREAM TO DOWNSTREAM*
	//         FIXME:  can sort that on our own
	G4String targvolnames[] = {
	  //	    "h2Targ", ""
	    "C12Targ", ""
	};
	////////////////////////////////////////////////////////////////////////////////

	int nidx = 0;
	while( targvolnames[nidx] != "" ){
	    vidx = 0;
	    while( vidx < thislog->GetNoDaughters() ){
		if( thislog->GetDaughter(vidx)->GetName() == targvolnames[nidx].append("_PV")) break;
		vidx++; 
	    }
	    if( vidx == thislog->GetNoDaughters() ){
		G4cerr << "Error " << __PRETTY_FUNCTION__ << " line " << __LINE__ <<
		    ":  target definition structure in GDML not valid.  Could not find volume " << targvolnames[nidx] << G4endl;
		exit(1);
	    }

	    beamtarg->AddVolume(thislog->GetDaughter(vidx));
	    nidx++;
	}
    }

  //==========================
  // List auxiliary info
  //==========================

  const G4GDMLAuxMapType* auxmap = fGDMLParser->GetAuxMap();

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
	      G4String det_type = (*vit).value;

	      // Also allow specification of det number ///////////////////
	      int det_no = -1;
	      for( nit  = (*iter).second.begin(); nit != (*iter).second.end(); nit++) {
		  if ((*nit).type == "DetNo") {
		      det_no= atoi((*nit).value.data());
		      useddetnums[det_no] = true;
		  }
	      }
	      if( det_no <= 0 ){
		  k = 1;
		  while( useddetnums[k] == true && k < __MAX_DETS ){ k++; }
		  if( k == __MAX_DETS ){
		      G4cerr << __FILE__ << " line " << __LINE__ << ": ERROR too many detectors" << G4endl;
		      exit(1);
		  }
		  det_no = k;
		  useddetnums[k] = true;
	      }
	      /////////////////////////////////////////////////////////////

	      retval = snprintf(detectorname, __DET_STRLEN,"remoll/det_%d", det_no);

	      assert( 0 < retval && retval < __DET_STRLEN ); // Ensure we're writing reasonable strings

	      thisdet = SDman->FindSensitiveDetector(detectorname);

	      if( thisdet == 0 ) {
		  thisdet = new remollGenericDetector(detectorname, det_no);
		  G4cout << "  Creating sensitive detector " << det_type
		      << " for volume " << myvol->GetName()
		      <<  G4endl << G4endl;
		  SDman->AddNewDetector(thisdet);
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

void remollDetectorConstruction::SetDetectorGeomFile(const G4String &str){
    fDetFileName = str;
}
