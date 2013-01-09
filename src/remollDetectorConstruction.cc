

#include "remollDetectorConstruction.hh"

#include "G4FieldManager.hh"
#include "G4TransportationManager.hh"

#include "G4Material.hh"
#include "G4Element.hh"
#include "G4NistManager.hh"

#include "G4VSolid.hh"
#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4Cons.hh"
#include "G4Trap.hh"
#include "G4Trd.hh"

#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "G4UserLimits.hh"
#include "globals.hh"

#include "G4SDManager.hh"
#include "G4VSensitiveDetector.hh"

#include "G4UniformMagField.hh" 
#include "G4EqMagElectricField.hh"
#include "G4MagIntegratorStepper.hh"
#include "G4Mag_UsualEqRhs.hh"
#include "G4ChordFinder.hh"
#include "G4PropagatorInField.hh"

#include "G4UImanager.hh"
#include "G4UIcommand.hh"

#include "G4ios.hh"

// GDML export
#include "G4GDMLParser.hh"

//visual
#include "G4VisAttributes.hh"
#include "G4Colour.hh"



remollDetectorConstruction::remollDetectorConstruction() {
    DefineMaterials()
}

remollDetectorConstruction::~remollDetectorConstruction() {
}



G4VPhysicalVolume*  remollDetectorConstruction::Construct()
{

    fGDMLParser.SetOverlapCheck(false);
    fGDMLParser.Read(detfileName);

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
  char detectorname[200];

  G4VSensitiveDetector* collimatordetector[100];

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

	snprintf(detectorname,200,"/detector%i",k+1);
	collimatordetector[k] = new MollerDetectorSD(detectorname);

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


  /*  Ugh
  for(int i=0;i<worldVolume->GetLogicalVolume()->GetNoDaughters();i++)
  {

    worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->SetVisAttributes(daughterVisAtt); 

    for (int j=0;j<worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->GetNoDaughters();j++) {
      worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->GetDaughter(j)->GetLogicalVolume()->SetVisAttributes(daughterVisAtt);

    if (worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->GetDaughter(j)->GetLogicalVolume()->GetMaterial()->GetName().compare("VacuumDet")==0)
           worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->GetDaughter(j)->GetLogicalVolume()->SetVisAttributes(planeDetVisAtt); 

    if (worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->GetDaughter(j)->GetLogicalVolume()->GetMaterial()->GetName().compare("Tungsten")==0)
           worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->GetDaughter(j)->GetLogicalVolume()->SetVisAttributes(WVisAtt); 

    if (worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->GetDaughter(j)->GetLogicalVolume()->GetMaterial()->GetName().compare("Lead")==0)
           worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->GetDaughter(j)->GetLogicalVolume()->SetVisAttributes(PbVisAtt); 

    if (worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->GetDaughter(j)->GetLogicalVolume()->GetMaterial()->GetName().compare("Copper")==0)
           worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->GetDaughter(j)->GetLogicalVolume()->SetVisAttributes(CuVisAtt); 

    if (worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->GetDaughter(j)->GetLogicalVolume()->GetMaterial()->GetName().compare("SILICON")==0)
           worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->GetDaughter(j)->GetLogicalVolume()->SetVisAttributes(SiVisAtt); 

    if (worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->GetDaughter(j)->GetLogicalVolume()->GetMaterial()->GetName().compare("LiquidHydrogen")==0)
           worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->GetDaughter(j)->GetLogicalVolume()->SetVisAttributes(targetVisAtt); 
    }

    if (worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->GetMaterial()->GetName().compare("VacuumDet")==0)
           worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->SetVisAttributes(planeDetVisAtt); 

    if (worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->GetMaterial()->GetName().compare("Tungsten")==0)
           worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->SetVisAttributes(WVisAtt); 

    if (worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->GetMaterial()->GetName().compare("Lead")==0)
           worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->SetVisAttributes(PbVisAtt); 

    if (worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->GetMaterial()->GetName().compare("Copper")==0)
           worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->SetVisAttributes(CuVisAtt); 

    if (worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->GetMaterial()->GetName().compare("SILICON")==0)
           worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->SetVisAttributes(SiVisAtt); 

    if (worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->GetMaterial()->GetName().compare("LiquidHydrogen")==0)
           worldVolume->GetLogicalVolume()->GetDaughter(i)->GetLogicalVolume()->SetVisAttributes(targetVisAtt); 

  }

  //target_logic->SetVisAttributes(targetVisAtt);

  */
  // TODO


  //==========================
  // Output geometry tree
  //==========================

  G4cout << G4endl << "Element table: " << G4endl << G4endl;
  G4cout << *(G4Element::GetElementTable()) << G4endl;

  G4cout << G4endl << "Material table: " << G4endl << G4endl;
  G4cout << *(G4Material::GetMaterialTable()) << G4endl;

  G4cout << G4endl << "Geometry tree: " << G4endl << G4endl;
  DumpGeometricalTree(worldVolume);

  ReadGlobalMagneticField();

  } else {

    worldVolume = ConstructDetector();
  }

  G4cout << G4endl << "###### Leaving remollDetectorConstruction::Read() " << G4endl << G4endl;

  return worldVolume;
}

void remollDetectorConstruction::WriteGeometryFile(const G4String& filename)
{
  G4bool appendPointerAddress = true;
  // Note: only change to false if all names are unique
  fGDMLParser.Write(filename,mother_phys,appendPointerAddress);
}

void remollDetectorConstruction::DefineMaterials()
{

  //--------------------------------------------------------------//
  //-----------------------------MATERIALS------------------------//
  //--------------------------------------------------------------//


  G4double a;
  G4double z;
  G4double density;
  G4double temp = 20.27*kelvin;
  G4int natoms;
  G4int nComponents;
  G4String symbol;

  //-----VACUUM------//

  Vacuum = new G4Material("Vacuum", z=1., a=1.01*g/mole,
			  density= universe_mean_density, 
			  kStateGas, 2.73*kelvin, 3.e-18*pascal);
  
  //-----Carbon------//
  Carbon = new G4Material("Carbon", z= 6., a= 12.01*g/mole,
			  density= 2.265*g/cm3);

  //-----Aluminum------//
  Al = new G4Material("Aluminum", z= 13., a= 26.98*g/mole,
			    density= 2.7*g/cm3);
			    
  //-----Copper------//
  Copper = new G4Material("Copper", z= 11., a= 63.54*g/mole,
			  density= 8.96*g/cm3);

  //-----Lead------//
  Pb = new G4Material("Lead", z= 82., a= 207.19*g/mole, 
		      density= 11.35*g/cm3);
  
  //-----Tungsten----//
  Tungsten = new G4Material("Tungsten", z= 74., a= 183.85*g/mole,
			    density= 19.3*g/cm3);
  
  //-----Iron-----//
  Iron = new G4Material("Iron", z= 26., a= 55.85*g/mole,
			density= 7.87*g/cm3);
  
  //-----Liquid Hydrogen---//
  H  = new G4Element("Hydrogen", symbol="H", 
		     z= 1., a= 1.00794*g/mole);
  LH2 = new G4Material("Liquid Hydrogen", density= 0.0708*g/cm3, 
		        nComponents=1, kStateLiquid, temp);

  LH2->AddElement(H, natoms=1);
  
  
  // Print materials defined.
  G4cout << G4endl << "The materials defined are : " << G4endl << G4endl;
  G4cout << *(G4Material::GetMaterialTable()) << G4endl;

}

G4VPhysicalVolume* remollDetectorConstruction::ConstructDetector()
{

  G4SDManager* SDman = G4SDManager::GetSDMpointer();
  G4String SDname;

  G4VisAttributes* VacVisAtt= new G4VisAttributes(G4Colour(0.3,0.8,0.5));
  G4VisAttributes* WVisAtt= new G4VisAttributes(G4Colour(0.7,0.7,0.7));
  G4VisAttributes* PbVisAtt= new G4VisAttributes(G4Colour(0.6,0.7,0.8));
  G4VisAttributes* CuVisAtt= new G4VisAttributes(G4Colour(1.0,0.5,0.1));

  VacVisAtt->SetVisibility(true);
  VacVisAtt->SetForceWireframe (true);

  WVisAtt->SetVisibility(true);
  PbVisAtt->SetVisibility(true);
  CuVisAtt->SetVisibility(true);

  //--------------------------------------------------------------//
  //-----------------------------VOLUMES--------------------------//
  //--------------------------------------------------------------//

  G4double inner_rad = 0.0*cm;
  G4double start_angle = 0.0*deg;
  G4double end_angle = 360.0*deg;
  G4double half_side_mom = 200.0*cm;
  G4double half_len_mom = 4000.0*cm;

  G4double outer_rad_targ = 4.0*cm;
  G4double half_len_targ = fTargetLength/2;

  G4cout<< "target half-length = "<<half_len_targ/cm<<G4endl;

  G4UserLimits* userLimits = new G4UserLimits(1.0*cm);

  //------World-----//

  G4Box* mother_solid = new G4Box("boxMother", half_side_mom, half_side_mom,
				   half_len_mom);

  mother_logic = new G4LogicalVolume(mother_solid, Vacuum, "logicMother", 0);
  mother_logic->SetUserLimits(userLimits);

  mother_phys = new G4PVPlacement(0,G4ThreeVector(), 
				  mother_logic, "111", 
				  0, false, 0);


  //------Target-----//

  target_solid = new G4Tubs("tubeTarget", inner_rad, outer_rad_targ,
				   half_len_targ, start_angle, end_angle);

  target_logic = new G4LogicalVolume(target_solid, LH2, "logicTarget");
  //target_logic = new G4LogicalVolume(target_solid, Vacuum, "logicTarget"); //for optics plots

  target_phys = new G4PVPlacement(0,G4ThreeVector(0, 0, 0), 
				  target_logic, "targ_0", 
				  mother_logic , false, 0);
  //Visualization Attributes
  //
  G4VisAttributes* motherVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  motherVisAtt->SetVisibility(false);
  mother_logic->SetVisAttributes(motherVisAtt);

  G4VisAttributes* targetVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,0.0));
  targetVisAtt->SetVisibility(true);
  target_logic->SetVisAttributes(targetVisAtt);


  //------Phi defining collimators
  const G4int numsectors=7;
  //  G4double startingphi=-pi/2;
  G4double startingphi=pi/2;
  G4double startingz=5.8*m;
  G4double radius=0.6*m;
  G4double internalholeradius=0*m;
  G4double thickness = 9.9*cm;

  G4VSensitiveDetector* collimatordetector[7];

  G4double openingangle=pi/numsectors;
  G4Trd* trapeziod_solid = new G4Trd("trapeziod_solid", 
				     radius*tan(openingangle/2.),0.,thickness,thickness,(radius-internalholeradius)/2.);
  G4RotationMatrix rotmatrix[numsectors];
  char trapphysname[100], traplogname[100];
  char washphysname[100];
  char detectorname[200];

  for (G4int sector = 1; sector <= numsectors; sector++) {
    G4double phi = startingphi + 2.*(sector-1)*(pi/numsectors); 
    rotmatrix[sector-1].rotateX(pi/2.);
    rotmatrix[sector-1].rotateZ(-phi);
    G4double xpos = radius/2.*sin(phi);
    G4double ypos = radius/2.*cos(phi);

    snprintf(traplogname,100,"trapeziod_log_%i",sector);
    trapeziod_logic.push_back(new G4LogicalVolume(trapeziod_solid, Tungsten, traplogname));


    snprintf(trapphysname,100,"trap_phys_%i",sector+NUM_COLS);
    

    trapeziod_phys.push_back(
      new G4PVPlacement(G4Transform3D(rotmatrix[sector-1],
				     G4ThreeVector(xpos,ypos,startingz)),
		       //0,G4ThreeVector(xpos,ypos,startingz),
		       trapeziod_logic.back(), 
		       trapphysname, 
		       mother_logic, 
		       false, 
		       0,
		       false));
    snprintf(detectorname,200,"/detector%i",sector+NUM_COLS);
    collimatordetector[sector-1] = new MollerDetectorSD(SDname=detectorname);
    SDman->AddNewDetector(collimatordetector[sector-1]);
    trapeziod_logic.back()->SetSensitiveDetector(collimatordetector[sector-1]);
    trapeziod_logic.back()->SetVisAttributes(WVisAtt);

    
  }


  //-----3QC1B (center)-----// 
  //-----Washers----// 

  std::vector<G4VSensitiveDetector*> detector;

  // For E'=2.75 GeV, theta_lab=0.0167 rad which from z=0.75 m to z=5.9 m gives a radius of 11.1 cm.
  //For E'=8.25 GeV, theta_lab=0.00557 rad which from z=-0.75 m to z=5.9 m gives a radius of 5.15*tan(0.00557)= 2.86 cm
  
  //  G4double inrad[NUM_COLS], extrad[NUM_COLS], detzmin[NUM_COLS], detzmax[NUM_COLS], 
  std::vector<G4double> dethalfleng;
  std::vector<G4double> loc;
  dethalfleng.resize(NUM_COLS);
  loc.resize(NUM_COLS);
  //  G4VSolid* washer_solid[NUM_COLS];

  std::vector<G4VSolid*> washer_solid;
  washer_solid.resize(NUM_COLS);

  char washer_logicname[100];

  //G4String collgeofile=("$G4WORKDIR/geometry/"+collimfileName+".dat");
  G4String gPath = ".";
  if (getenv("MOLLERGEANTDIR"))
    gPath = getenv("MOLLERGEANTDIR");
  else
    G4cout << "You do not have MOLLERGEANTDIR defined in your environment!" << G4endl;
  G4String collgeofile=(gPath+"/geometry/"+collimfileName+".dat");
  ReadCollimatorGeometry(collgeofile);

  for (int i=0; i<NUM_COLS; i++) {
    loc[i] = (collUpstreamZ[i]+collDownstreamZ[i])/2;
    dethalfleng[i] = (collDownstreamZ[i]-collUpstreamZ[i])/2;

    washer_solid[i] = new G4Cons("solWasher"+
				 G4UIcommand::ConvertToString((i+1)), 
				 collInnerRadius[i], collOuterRadius[i],
				 collInnerRadius[i], collOuterRadius[i], dethalfleng[i],
				 start_angle, end_angle);
    
    snprintf(washer_logicname,100,"logicWasher_%i",i);
    snprintf(washphysname,100,"wash_phys_%i",i);    

    SDname = "/detector";
    SDname+=G4UIcommand::ConvertToString((i+1));

    detector.push_back (new MollerDetectorSD(SDname));

    SDman->AddNewDetector(detector[i]);

  
    if (collMaterial[i] == "Vacuum") {
      washer_logic.push_back(new G4LogicalVolume(washer_solid[i], Vacuum, washer_logicname, 0));
      washer_logic.back()->SetSensitiveDetector(detector[i]);
      washer_logic.back()->SetVisAttributes(VacVisAtt);
    } else if (collMaterial[i] == "Tungsten") {
      washer_logic.push_back(new G4LogicalVolume(washer_solid[i], Tungsten, washer_logicname, 0));
      washer_logic.back()->SetSensitiveDetector(detector[i]);
      washer_logic.back()->SetVisAttributes(WVisAtt);
     } else if (collMaterial[i] == "Lead") {
      washer_logic.push_back(new G4LogicalVolume(washer_solid[i], Pb, washer_logicname, 0));
      washer_logic.back()->SetSensitiveDetector(detector[i]);
      washer_logic.back()->SetVisAttributes(PbVisAtt);
    } else if (collMaterial[i] == "Copper") {
      washer_logic.push_back(new G4LogicalVolume(washer_solid[i], Copper, washer_logicname, 0));
      washer_logic.back()->SetSensitiveDetector(detector[i]);
      washer_logic.back()->SetVisAttributes(CuVisAtt);
     } else {
      G4cerr << G4endl<< " Check collimator "<< i <<" -  material not specified!" << G4endl;
      break;
    }
    
    
    washer_phys.push_back(new G4PVPlacement(0,G4ThreeVector(0, 0, loc[i]), 
				       washer_logic.back(), 
				       washphysname, 
				       mother_logic, false, 0));
    
  }
  

  ReadGlobalMagneticField();

  return mother_phys;

}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
void remollDetectorConstruction::ReadGlobalMagneticField()
{
  pGlobalMagnetField->ReadMagneticField();
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
void remollDetectorConstruction::CreateGlobalMagneticField()   
{

  //--------- Magnetic Field -------------------------------
  
  //============================================
  //  Define the global magnet field Manager
  //============================================
  pGlobalMagnetField = new MollerGlobalMagnetField();

  // Get transportation, field, and propagator  managers
  fGlobalFieldManager = G4TransportationManager::GetTransportationManager()->GetFieldManager();
//  G4TransportationManager::GetTransportationManager()->GetPropagatorInField()->SetLargestAcceptableStep(25*cm);
  fGlobalFieldManager->SetDetectorField(pGlobalMagnetField);
  fGlobalFieldManager->CreateChordFinder(pGlobalMagnetField);

  return;
  fGlobalEquation = new G4Mag_UsualEqRhs(pGlobalMagnetField);
  
  // taken from one of the Geant4 presentation:
  // - If the field is calculated from a field map, a lower order stepper
  //   is recommended: the less smooth the field is, the lower the order of the
  //   stepper that should be used. For very rough fields one should use 1st order
  //   stepper, for a somewhat smooth field one must choose between 1st and 2nd
  //   order stepper.
  
  //fGlobalStepper  = new G4ClassicalRK4(fGlobalEquation);  // classical 4th order stepper
 // fGlobalStepper  = new G4ExplicitEuler(fGlobalEquation); //           1st order stepper
  //fGlobalStepper  = new G4ImplicitEuler(fGlobalEquation); //           2nd order stepper
  fGlobalStepper  = new G4SimpleRunge(fGlobalEquation);   //           2nd order stepper


  // Provides a driver that talks to the Integrator Stepper, and insures that 
  //   the error is within acceptable bounds.
  G4MagInt_Driver* fGlobalIntgrDriver = new G4MagInt_Driver(1.0e-3*mm, 
							    fGlobalStepper,
							    fGlobalStepper->GetNumberOfVariables());
  
  fGlobalChordFinder = new G4ChordFinder(fGlobalIntgrDriver);
  
  
  
  //       G4bool fieldChangesEnergy = false;
  //       G4FieldManager* pFieldMgr = new G4FieldManager(myField,pChordFinder,FieldChangeEnergy);
  //       LocalLogicalVolume = new G4LogicalVolume(shape, material,"name",pFieldMgr,0,0);
  
  //   // minimal step of 1 mm is default
  //   fMinStep = 0.01*mm ;
  //
  //   fGlobalChordFinder = new G4ChordFinder (pGlobalMagnetField,
  //                                           fMinStep,
  //                                           fGlobalStepper);
  
  fGlobalFieldManager->SetChordFinder(fGlobalChordFinder);
} 
void remollDetectorConstruction::ReadCollimatorGeometry(const char* filename)
{
  
  G4cout << G4endl << "###### Calling remollDetectorConstruction::ReadCollimatorGeometry " << G4endl << G4endl;
  //G4cout << G4endl;
  //G4cout << "------------------------------------------------" << G4endl;
  //G4cout << " Getting collimator geometry from " << filename   << G4endl; 
  //G4cout << "------------------------------------------------" << G4endl;


  G4int   coll_index=0;
  G4double ri,ro,zu,zd;
  G4String collmat;

  // open the collimator geometry file
  std::ifstream inputfile;
  inputfile.open(filename); // Open the file for reading.
  G4cout << G4endl << filename << " opened for reading, ncolls =" <<NUM_COLS<< G4endl<< G4endl;

  for (G4int nlines=0;nlines<NUM_COLS;nlines++) {

    if (!inputfile.good()) break;

    inputfile >> ri  >> ro >> zu >> zd >> collmat; 

    //printf("coll ri ro zu zd: %2i  %f %f %f %f %s\n",coll_index, ri, ro, zu, zd, collmat.c_str());

    collInnerRadius.push_back (ri*cm);
    collOuterRadius.push_back (ro*cm);
    collUpstreamZ.push_back (zu*cm);
    collDownstreamZ.push_back (zd*cm);
    collMaterial.push_back (collmat);

    printf("coll ri ro zu zd: %2i  %7.2f %7.2f %7.2f %7.2f \t %s\n",coll_index, collInnerRadius[nlines]/cm, collOuterRadius[nlines]/cm, 
	   collUpstreamZ[nlines]/cm, collDownstreamZ[nlines]/cm, collMaterial[nlines].c_str());
    //printf("coll ri ro zu zd: %2i  %f %f %f %f %s\n",coll_index, collInnerRadius[nlines], collOuterRadius[nlines], 
    //	 collUpstreamZ[nlines], collDownstreamZ[nlines], collMaterial[nlines]->GetName());
    coll_index++;
  }

	
  // close file
  inputfile.close();
	
  if (coll_index!=NUM_COLS) {
    G4cerr <<"coll_index, "<< coll_index<<" not equal to NUM_COLS,"<<NUM_COLS<<G4endl;
  }
  else if ((int)collDownstreamZ.size()!=NUM_COLS) {
    G4cerr <<"collDownstreamZ.size(), "<< collDownstreamZ.size()<<" not equal to NUM_COLS, "<<NUM_COLS<<G4endl;
  }
  G4cout << "... done reading " << coll_index << " lines." << G4endl;
	
  G4cout << G4endl << "###### Leaving remollDetectorConstruction::ReadCollimatorGeometry " << G4endl << G4endl;
}
