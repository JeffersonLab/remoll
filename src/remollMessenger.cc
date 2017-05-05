#include "remollMessenger.hh"

#include "Randomize.hh"

#include "G4UIcmdWithAnInteger.hh"
#include "G4UIcmdWithAString.hh"
#include "G4UIcmdWithADoubleAndUnit.hh"
#include "G4UIcmdWithABool.hh"

#include "remollGlobalField.hh"
#include "remollOpticalPhysics.hh"
#include "remollDetectorConstruction.hh"
#include "remollIO.hh"
#include "remollEventAction.hh"
#include "remollVEventGen.hh"
#include "remollPrimaryGeneratorAction.hh"
#include "remollBeamTarget.hh"
#include "remollRun.hh"
#include "remollRunData.hh"
#include "remollGenPion.hh"
#include "remollGenFlat.hh"
#include "remollGenLUND.hh"

#include "G4UImanager.hh"
#include "G4VModularPhysicsList.hh"

#include "G4GDMLParser.hh"
#include "G4VPhysicalVolume.hh"

#include <iostream>

// Singleton
remollMessenger* remollMessenger::gInstance = 0;
remollMessenger* remollMessenger::GetInstance() {
  if (gInstance == 0) {
    gInstance = new remollMessenger();
  }
  return gInstance;
}

remollMessenger::remollMessenger()
{
  G4cout << "Creating remollMessenger object " << this << G4endl;

    /*  Initialize all the things it talks to to NULL */

    fdetcon       = NULL;
    fField        = NULL;
    fPhysicsList  = NULL;

    detfilesCmd = new G4UIcmdWithAString("/remoll/setgeofile",this);
    detfilesCmd->SetGuidance("Set geometry GDML files");
    detfilesCmd->SetParameterName("geofilename", false);
    detfilesCmd->AvailableForStates(G4State_PreInit); // Only have this in pre-init or GDML freaks out

    seedCmd = new G4UIcmdWithAnInteger("/remoll/seed",this);
    seedCmd->SetGuidance("Set random engine seed");
    seedCmd->SetParameterName("seed", false);

    opticalCmd = new G4UIcmdWithABool("/remoll/optical",this);
    opticalCmd->SetGuidance("Enable optical physics");
    opticalCmd->SetParameterName("optical", false);
    opticalCmd->AvailableForStates(G4State_Idle); // Only have this AFTER we've initalized geometry

    dumpGeometryCmd = new G4UIcmdWithABool("/remoll/dumpgeometry",this);
    dumpGeometryCmd->SetGuidance("Dump the geometry tree");
    dumpGeometryCmd->SetParameterName("overlap_check", true);
    dumpGeometryCmd->SetDefaultValue(false);
    dumpGeometryCmd->AvailableForStates(G4State_Idle); // Only have this AFTER we've initalized geometry

    newfieldCmd = new G4UIcmdWithAString("/remoll/addfield",this);
    newfieldCmd->SetGuidance("Add magnetic field");
    newfieldCmd->SetParameterName("filename", false);

    fieldScaleCmd = new G4UIcmdWithAString("/remoll/scalefield",this);
    fieldScaleCmd->SetGuidance("Scale magnetic field");
    fieldScaleCmd->SetParameterName("filename", false);

    fieldCurrCmd = new G4UIcmdWithAString("/remoll/magcurrent",this);
    fieldCurrCmd->SetGuidance("Scale magnetic field by current");
    fieldCurrCmd->SetParameterName("filename", false);

}

remollMessenger::~remollMessenger(){
}


void remollMessenger::SetNewValue(G4UIcommand* cmd, G4String newValue){
    if( cmd == detfilesCmd ){
	fdetcon->SetDetectorGeomFile( newValue );
    }

    if( cmd == seedCmd ){
	G4int seed = seedCmd->GetNewIntValue(newValue);
	G4Random::setTheSeed(seed);
	remollRun::GetRunData()->SetSeed(seed);
    }

    if( cmd == opticalCmd ){
	G4bool optical = opticalCmd->GetNewBoolValue(newValue);
	if( optical ){
	    fPhysicsList->RegisterPhysics( new remollOpticalPhysics() );
	} else {
	    fPhysicsList->RemovePhysics("Optical");
	}
    }

    if( cmd == dumpGeometryCmd ){
        G4bool overlap_check = dumpGeometryCmd->GetNewBoolValue(newValue);
        fdetcon->DumpGeometricalTree(0,0,overlap_check);
    }

    if( cmd == newfieldCmd ){
	fField->AddNewField( newValue );
    }

    if( cmd == fieldScaleCmd ){
	std::istringstream iss(newValue);

	G4String scalefile, scalestr;
	G4double scaleval;

	iss >> scalefile;
	iss >> scalestr;

	scaleval = atof(scalestr.data());
	fField->SetFieldScale( scalefile, scaleval );
    }

    if( cmd == fieldCurrCmd ){
	std::istringstream iss(newValue);

	G4String scalefile, scalestr, scaleunit;
	G4double scaleval;

	iss >> scalefile;
	iss >> scalestr;
	iss >> scaleunit;

	if( scaleunit != "A" ){
	    // FIXME: less snark and more functionality?
	    G4cerr << __FILE__ << " line " << __LINE__ <<  ":\n\tGraaaah - just put the current for " <<  scalefile <<  " in amps..." << G4endl;
	    exit(1);
	}

	scaleval = atof(scalestr.data());
	fField->SetMagnetCurrent( scalefile, scaleval );
    }
}
