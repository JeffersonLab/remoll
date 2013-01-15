

#include "remollDetectorConstruction.hh"
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
#define __DET_MAXDET 100

remollDetectorConstruction::remollDetectorConstruction() {
}

remollDetectorConstruction::~remollDetectorConstruction() {
}



G4VPhysicalVolume*  remollDetectorConstruction::Construct()
{

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
      iter != auxmap->end(); iter++) 
  {
    G4cout << "Volume " << ((*iter).first)->GetName()
           << " has the following list of auxiliary information: "<< G4endl;
    for (G4GDMLAuxListType::const_iterator
         vit  = (*iter).second.begin();
         vit != (*iter).second.end(); vit++)
    {
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

  G4VSensitiveDetector* collimatordetector[__DET_MAXDET];

  G4int k=0;
  for(G4GDMLAuxMapType::const_iterator
      iter  = auxmap->begin();
      iter != auxmap->end(); iter++)
  {
    G4LogicalVolume* myvol = (*iter).first;
    G4cout << "Volume " << myvol->GetName();
    
    for (G4GDMLAuxListType::const_iterator
	   vit  = (*iter).second.begin();
           vit != (*iter).second.end(); vit++)
    {
      if ((*vit).type == "SensDet")
      {
        G4String det_type = (*vit).value;
	//G4cout << " is a " << det_type <<  G4endl << G4endl;

	retval = snprintf(detectorname, __DET_STRLEN,"/det_%04d",k+1);
	assert( 0 < retval && retval < __DET_STRLEN ); // Ensure we're writing reasonable strings
	//collimatordetector[k] = new MollerDetectorSD(detectorname);

        if (collimatordetector[k] != 0)
        {

          G4cout << "  Creating sensitive detector " << det_type
                 << " for volume " << myvol->GetName()
                 <<  G4endl << G4endl;

          SDman->AddNewDetector(collimatordetector[k]);
          myvol->SetSensitiveDetector(collimatordetector[k]);
        }
        else
        {
          G4cout << det_type << " sensitive detector type not found" << G4endl;
	}
      }
    }
    k++;
  }

  //==========================
  // Visualization attributes
  //==========================

  G4VisAttributes* motherVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  G4VisAttributes* daughterVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  G4VisAttributes* targetVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,0.0));
  G4VisAttributes* planeDetVisAtt= new G4VisAttributes(G4Colour(0.3,0.8,0.5));

  motherVisAtt->SetVisibility(false);

  targetVisAtt->SetVisibility(true);

  daughterVisAtt->SetVisibility(true);
  daughterVisAtt->SetForceWireframe (true);

  planeDetVisAtt->SetVisibility(true);
  planeDetVisAtt->SetForceWireframe (true);

  G4VisAttributes* WVisAtt= new G4VisAttributes(G4Colour(0.7,0.7,0.7));
  G4VisAttributes* PbVisAtt= new G4VisAttributes(G4Colour(0.6,0.7,0.8));
  G4VisAttributes* CuVisAtt= new G4VisAttributes(G4Colour(1.0,0.5,0.1));
  G4VisAttributes* SiVisAtt= new G4VisAttributes(G4Colour(0.4,0.4,0.0));

  WVisAtt->SetVisibility(true);
  PbVisAtt->SetVisibility(true);
  CuVisAtt->SetVisibility(true);
  SiVisAtt->SetVisibility(true);

  worldVolume->GetLogicalVolume()->SetVisAttributes(motherVisAtt);


  //==========================
  // Output geometry tree
  //==========================

  G4cout << G4endl << "Element table: " << G4endl << G4endl;
  G4cout << *(G4Element::GetElementTable()) << G4endl;

  G4cout << G4endl << "Material table: " << G4endl << G4endl;
  G4cout << *(G4Material::GetMaterialTable()) << G4endl;

  G4cout << G4endl << "Geometry tree: " << G4endl << G4endl;
  //DumpGeometricalTree(worldVolume);


  G4cout << G4endl << "###### Leaving remollDetectorConstruction::Read() " << G4endl << G4endl;

  return worldVolume;
}


void remollDetectorConstruction::CreateGlobalMagneticField()   
{

  //--------- Magnetic Field -------------------------------
  
  //============================================
  //  Define the global magnet field Manager
  //============================================
  fGlobalField = new remollGlobalField();

  // Get transportation, field, and propagator  managers
  fGlobalFieldManager = G4TransportationManager::GetTransportationManager()->GetFieldManager();
//  G4TransportationManager::GetTransportationManager()->GetPropagatorInField()->SetLargestAcceptableStep(25*cm);
  fGlobalFieldManager->SetDetectorField(fGlobalField);
  fGlobalFieldManager->CreateChordFinder(fGlobalField);

  return;
} 

