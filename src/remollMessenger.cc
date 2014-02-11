#include "remollMessenger.hh"

#include "G4UIcmdWithAnInteger.hh"
#include "G4UIcmdWithAString.hh"
#include "G4UIcmdWithADoubleAndUnit.hh"
#include "G4UIcmdWithABool.hh"

#include "remollOpticalPhysics.hh"
#include "remollDetectorConstruction.hh"
#include "remollIO.hh"
#include "remollEventAction.hh"
#include "remollVEventGen.hh"
#include "remollPrimaryGeneratorAction.hh"
#include "remollBeamTarget.hh"
#include "remollRun.hh"
#include "remollRunData.hh"
#include "remollSteppingAction.hh"
#include "remollGenFlat.hh"

#include "G4UImanager.hh"
#include "G4RunManager.hh"

#include "G4GDMLParser.hh"
#include "G4VPhysicalVolume.hh"

#include <iostream>

remollMessenger::remollMessenger(){
    /*  Initialize all the things it talks to to NULL */

    fIO           = NULL;
    fdetcon       = NULL;
    fevact        = NULL;
    fprigen       = NULL;
    fField        = NULL;
    fBeamTarg     = NULL;
    fStepAct      = NULL;
    fPhysicsList  = NULL;

    // Grab singleton beam/target
    fBeamTarg = remollBeamTarget::GetBeamTarget();

    detfilesCmd = new G4UIcmdWithAString("/remoll/setgeofile",this);
    detfilesCmd->SetGuidance("Set geometry GDML files");
    detfilesCmd->SetParameterName("geofilename", false);
    detfilesCmd->AvailableForStates(G4State_PreInit); // Only have this in pre-init or GDML freaks out

    seedCmd = new G4UIcmdWithAnInteger("/remoll/seed",this);
    seedCmd->SetGuidance("Set random engine seed");
    seedCmd->SetParameterName("seed", false);

    kryptCmd = new G4UIcmdWithABool("/remoll/kryptonite",this);
    kryptCmd->SetGuidance("Treat W, Pb, Cu as kryptonite");
    kryptCmd->SetParameterName("krypt", false);

    opticalCmd = new G4UIcmdWithABool("/remoll/optical",this);
    opticalCmd->SetGuidance("Enable optical physics");
    opticalCmd->SetParameterName("optical", false);
    opticalCmd->AvailableForStates(G4State_Idle); // Only have this AFTER we've initalized geometry

    newfieldCmd = new G4UIcmdWithAString("/remoll/addfield",this);
    newfieldCmd->SetGuidance("Add magnetic field");
    newfieldCmd->SetParameterName("filename", false);

    fieldScaleCmd = new G4UIcmdWithAString("/remoll/scalefield",this);
    fieldScaleCmd->SetGuidance("Scale magnetic field");
    fieldScaleCmd->SetParameterName("filename", false);

    fieldCurrCmd = new G4UIcmdWithAString("/remoll/magcurrent",this);
    fieldCurrCmd->SetGuidance("Scale magnetic field by current");
    fieldCurrCmd->SetParameterName("filename", false);

    tgtLenCmd = new G4UIcmdWithADoubleAndUnit("/remoll/targlen",this);
    tgtLenCmd->SetGuidance("Target length");
    tgtLenCmd->SetParameterName("targlen", false);
    tgtLenCmd->AvailableForStates(G4State_Idle); // Only have this AFTER we've initalized geometry

    tgtPosCmd = new G4UIcmdWithADoubleAndUnit("/remoll/targpos",this);
    tgtPosCmd->SetGuidance("Target position");
    tgtPosCmd->SetParameterName("targpos", false);
    tgtPosCmd->AvailableForStates(G4State_Idle); // Only have this AFTER we've initalized geometry

    beamCurrCmd = new G4UIcmdWithADoubleAndUnit("/remoll/beamcurr",this);
    beamCurrCmd->SetGuidance("Beam current");
    beamCurrCmd->SetParameterName("beamcurr", false);

    beamECmd = new G4UIcmdWithADoubleAndUnit("/remoll/beamene",this);
    beamECmd->SetGuidance("Beam energy");
    beamECmd->SetParameterName("beamene", false);

    genSelectCmd = new G4UIcmdWithAString("/remoll/gen",this);
    genSelectCmd->SetGuidance("Select physics generator");
    genSelectCmd->SetParameterName("generator", false);

    fileCmd = new G4UIcmdWithAString("/remoll/filename",this);
    fileCmd->SetGuidance("Output filename");
    fileCmd->SetParameterName("filename", false);

    thminCmd = new G4UIcmdWithADoubleAndUnit("/remoll/thmin",this);
    thminCmd->SetGuidance("Minimum generation angle");
    thminCmd->SetParameterName("thmin", false);

    thmaxCmd = new G4UIcmdWithADoubleAndUnit("/remoll/thmax",this);
    thmaxCmd->SetGuidance("Minimum generation angle");
    thmaxCmd->SetParameterName("thmax", false);

    thCoMminCmd = new G4UIcmdWithADoubleAndUnit("/remoll/thcommin",this);
    thCoMminCmd->SetGuidance("Minimum CoM generation angle");
    thCoMminCmd->SetParameterName("thcommin", false);

    thCoMmaxCmd = new G4UIcmdWithADoubleAndUnit("/remoll/thcommax",this);
    thCoMmaxCmd->SetGuidance("Minimum CoM generation angle");
    thCoMmaxCmd->SetParameterName("thcommax", false);

    EminCmd = new G4UIcmdWithADoubleAndUnit("/remoll/emin",this);
    EminCmd->SetGuidance("Minimum generation energy");
    EminCmd->SetParameterName("emin", false);

    EmaxCmd = new G4UIcmdWithADoubleAndUnit("/remoll/emax",this);
    EmaxCmd->SetGuidance("Maximum generation energy");
    EmaxCmd->SetParameterName("emax", false);


    //////////////////////////////////////////////////
    // beam info

    rasXCmd = new G4UIcmdWithADoubleAndUnit("/remoll/rasx",this);
    rasXCmd->SetGuidance("Square raster width in x (horizontal)");
    rasXCmd->SetParameterName("rasx", false);

    rasYCmd = new G4UIcmdWithADoubleAndUnit("/remoll/rasy",this);
    rasYCmd->SetGuidance("Square raster width y (vertical)");
    rasYCmd->SetParameterName("rasy", false);

    beamX0Cmd = new G4UIcmdWithADoubleAndUnit("/remoll/beam_x0",this);
    beamX0Cmd->SetGuidance("beam initial position in x (horizontal)");
    beamX0Cmd->SetParameterName("beamX0", false);

    beamY0Cmd = new G4UIcmdWithADoubleAndUnit("/remoll/beam_y0",this);
    beamY0Cmd->SetGuidance("beam initial position in y (vertical)");
    beamY0Cmd->SetParameterName("beamY0", false);

    beamth0Cmd = new G4UIcmdWithADoubleAndUnit("/remoll/beam_th0",this);
    beamth0Cmd->SetGuidance("beam initial direction in x (horizontal)");
    beamth0Cmd->SetParameterName("beamth0", false);

    beamph0Cmd = new G4UIcmdWithADoubleAndUnit("/remoll/beam_ph0",this);
    beamph0Cmd->SetGuidance("beam initial direction in y (vertical)");
    beamph0Cmd->SetParameterName("beamph0", false);

    beamCorrThCmd = new G4UIcmdWithADoubleAndUnit("/remoll/beam_corrth",this);
    beamCorrThCmd->SetGuidance("beam correlated angle (horizontal)");
    beamCorrThCmd->SetParameterName("beam_corrth", false);

    beamCorrPhCmd = new G4UIcmdWithADoubleAndUnit("/remoll/beam_corrph",this);
    beamCorrPhCmd->SetGuidance("beam correlated angle (vertical)");
    beamCorrPhCmd->SetParameterName("beam_corrph", false);

    beamdthCmd = new G4UIcmdWithADoubleAndUnit("/remoll/beam_dth",this);
    beamdthCmd->SetGuidance("beam gaussian spread in direction x (horizontal)");
    beamdthCmd->SetParameterName("beamdth", false);

    beamdphCmd = new G4UIcmdWithADoubleAndUnit("/remoll/beam_dph",this);
    beamdphCmd->SetGuidance("beam gaussian spread in direction y (vertical)");
    beamdphCmd->SetParameterName("beamdph", false);



    /*
       fExpType = kNeutronExp;

       runCmd = new G4UIcmdWithAnInteger("/g4sbs/run",this);
       runCmd->SetGuidance("Run simulation with x events");
       runCmd->SetParameterName("nevt", false);

       gemconfigCmd = new G4UIcmdWithAnInteger("/g4sbs/gemconfig",this);
       gemconfigCmd->SetGuidance("Change between GEM configurations");
       gemconfigCmd->SetParameterName("gemconfig", false);


       sigfileCmd = new G4UIcmdWithAString("/g4sbs/sigmafile",this);
       sigfileCmd->SetGuidance("GEM Sigma filename");
       sigfileCmd->SetParameterName("sigmafile", false);

       tgtCmd = new G4UIcmdWithAString("/g4sbs/target",this);
       tgtCmd->SetGuidance("Target type from LH2, LD2, H2, 3He");
       tgtCmd->SetParameterName("targtype", false);

       kineCmd = new G4UIcmdWithAString("/g4sbs/kine",this);
       kineCmd->SetGuidance("Kinematic type");
       kineCmd->SetParameterName("kinetype", false);

       expCmd = new G4UIcmdWithAString("/g4sbs/exp",this);
       expCmd->SetGuidance("Experiment type");
       expCmd->SetParameterName("exptype", false);

       geantinoCmd = new G4UIcmdWithABool("/g4sbs/shootgeantino", this);
       geantinoCmd->SetGuidance("Shoot a geantino instead of e-");
       geantinoCmd->SetParameterName("shootgeantino", false);


       tgtDenCmd = new G4UIcmdWithADoubleAndUnit("/g4sbs/targden",this);
       tgtDenCmd->SetGuidance("Target density");
       tgtDenCmd->SetParameterName("targden", false);

       tgtPresCmd = new G4UIcmdWithADoubleAndUnit("/g4sbs/targpres",this);
       tgtPresCmd->SetGuidance("Gaseous Target pressure");
       tgtPresCmd->SetParameterName("targpres", false);

       beamcurCmd = new G4UIcmdWithADoubleAndUnit("/g4sbs/beamcur",this);
       beamcurCmd->SetGuidance("Beam current");
       beamcurCmd->SetParameterName("beamcur", false);

       runtimeCmd = new G4UIcmdWithADoubleAndUnit("/g4sbs/runtime",this);
       runtimeCmd->SetGuidance("Run time");
       runtimeCmd->SetParameterName("runtime", false);

       rasterxCmd = new G4UIcmdWithADoubleAndUnit("/g4sbs/rasterx",this);
       rasterxCmd->SetGuidance("Raster x size");
       rasterxCmd->SetParameterName("size", false);

       rasteryCmd = new G4UIcmdWithADoubleAndUnit("/g4sbs/rastery",this);
       rasteryCmd->SetGuidance("Raster y size");
       rasteryCmd->SetParameterName("size", false);

       beamECmd = new G4UIcmdWithADoubleAndUnit("/g4sbs/beamE",this);
       beamECmd->SetGuidance("Beam Energy");
       beamECmd->SetParameterName("energy", false);

       bbangCmd = new G4UIcmdWithADoubleAndUnit("/g4sbs/bbang",this);
       bbangCmd->SetGuidance("BigBite angle");
       bbangCmd->SetParameterName("angle", false);

       bbdistCmd = new G4UIcmdWithADoubleAndUnit("/g4sbs/bbdist",this);
       bbdistCmd->SetGuidance("BigBite distance");
       bbdistCmd->SetParameterName("dist", false);

       hcalangCmd = new G4UIcmdWithADoubleAndUnit("/g4sbs/hcalang",this);
       hcalangCmd->SetGuidance("HCAL angle");
    hcalangCmd->SetParameterName("angle", false);

    hcaldistCmd = new G4UIcmdWithADoubleAndUnit("/g4sbs/hcaldist",this);
    hcaldistCmd->SetGuidance("HCAL distance");
    hcaldistCmd->SetParameterName("dist", false);

    hmagdistCmd = new G4UIcmdWithADoubleAndUnit("/g4sbs/48D48dist",this);
    hmagdistCmd->SetGuidance("48D48 distance");
    hmagdistCmd->SetParameterName("dist", false);

    gemresCmd = new G4UIcmdWithADoubleAndUnit("/g4sbs/gemres",this);
    gemresCmd->SetGuidance("GEM resolution");
    gemresCmd->SetParameterName("dist", false);

    // Detector position commands

    cerDisCmd = new G4UIcmdWithADoubleAndUnit("/g4sbs/cerdist",this);
    cerDisCmd->SetGuidance("Cerenkov distance from front GEM");
    cerDisCmd->SetParameterName("dist", false);

    cerDepCmd = new G4UIcmdWithADoubleAndUnit("/g4sbs/cerdepth",this);
    cerDepCmd->SetGuidance("Cerenkov gas depth");
    cerDepCmd->SetParameterName("dist", false);

    gemSepCmd = new G4UIcmdWithADoubleAndUnit("/g4sbs/gemsep",this);
    gemSepCmd->SetGuidance("GEM separation from front to back set");
    gemSepCmd->SetParameterName("dist", false);

    bbCalDistCmd = new G4UIcmdWithADoubleAndUnit("/g4sbs/bbcaldist",this);
    bbCalDistCmd->SetGuidance("BigBite caloriter distance from front GEM");
    bbCalDistCmd->SetParameterName("dist", false);

    thminCmd = new G4UIcmdWithADoubleAndUnit("/g4sbs/thmin",this);
    thminCmd->SetGuidance("Minimum electron generation polar angle");
    thminCmd->SetParameterName("angle", false);

    thmaxCmd = new G4UIcmdWithADoubleAndUnit("/g4sbs/thmax",this);
    thmaxCmd->SetGuidance("Maximum electron generation polar angle");
    thmaxCmd->SetParameterName("angle", false);

    phminCmd = new G4UIcmdWithADoubleAndUnit("/g4sbs/phmin",this);
    phminCmd->SetGuidance("Minimum electron generation azimuthal angle");
    phminCmd->SetParameterName("angle", false);

    phmaxCmd = new G4UIcmdWithADoubleAndUnit("/g4sbs/phmax",this);
    phmaxCmd->SetGuidance("Maximum electron generation azimuthal angle");
    phmaxCmd->SetParameterName("angle", false);
    */

}

remollMessenger::~remollMessenger(){
}


void remollMessenger::SetNewValue(G4UIcommand* cmd, G4String newValue){
    if( cmd == detfilesCmd ){
	fdetcon->SetDetectorGeomFile( newValue );
    }

    if( cmd == seedCmd ){
	G4int seed = seedCmd->GetNewIntValue(newValue);
	CLHEP::HepRandom::setTheSeed(seed);
	remollRun::GetRun()->GetData()->SetSeed(seed);
    }

    if( cmd == kryptCmd ){
	G4bool krypt = kryptCmd->GetNewBoolValue(newValue);
	fStepAct->SetEnableKryptonite(krypt);
    }

    if( cmd == opticalCmd ){
	G4bool optical = opticalCmd->GetNewBoolValue(newValue);
	if( optical ){
	    fPhysicsList->RegisterPhysics( new remollOpticalPhysics() );
	} else {
	    fPhysicsList->RemovePhysics("Optical");
	}

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

    if( cmd == tgtLenCmd ){
	G4double len = tgtLenCmd->GetNewDoubleValue(newValue);
	fBeamTarg->SetTargetLen(len);
    }

    if( cmd == tgtPosCmd ){
	G4double pos = tgtPosCmd->GetNewDoubleValue(newValue);
	fBeamTarg->SetTargetPos(pos);
    }

    if( cmd == genSelectCmd ){
	fprigen->SetGenerator( newValue );
    }

    if( cmd == beamCurrCmd ){
	G4double cur = beamCurrCmd->GetNewDoubleValue(newValue);
	fBeamTarg->SetBeamCurrent(cur);
    }

    if( cmd == beamECmd ){
	G4double ene = beamECmd->GetNewDoubleValue(newValue);
	fBeamTarg->fBeamE = ene;
    }

    if( cmd == fileCmd ){
	fIO->SetFilename(newValue);
    }

    if( cmd == EminCmd ){
	G4double en = EminCmd->GetNewDoubleValue(newValue);
	remollVEventGen *agen = fprigen->GetGenerator();
	if( agen ){
	    agen->fE_min = en;
	}
    }

    if( cmd == EmaxCmd ){
	G4double en = EmaxCmd->GetNewDoubleValue(newValue);
	remollGenFlat *agen = dynamic_cast<remollGenFlat *>(fprigen->GetGenerator());
	if( agen ){
	    agen->fE_max = en;
	}
    }

    if( cmd == thminCmd ){
	G4double th = thminCmd->GetNewDoubleValue(newValue);
	remollVEventGen *agen = fprigen->GetGenerator();
	if( agen ){
	    agen->fTh_min = th;
	}
    }

    if( cmd == thmaxCmd ){
	G4double th = thminCmd->GetNewDoubleValue(newValue);
	remollVEventGen *agen = fprigen->GetGenerator();
	if( agen ){
	    agen->fTh_max = th;
	}
    }

    if( cmd == thCoMminCmd ){
	G4double th = thCoMminCmd->GetNewDoubleValue(newValue);
	remollVEventGen *agen = fprigen->GetGenerator();
	if( agen ){
	    agen->fThCoM_min = th;
	}
    }

    if( cmd == thCoMmaxCmd ){
	G4double th = thCoMminCmd->GetNewDoubleValue(newValue);
	remollVEventGen *agen = fprigen->GetGenerator();
	if( agen ){
	    agen->fThCoM_max = th;
	}
    }

    if( cmd == rasXCmd ){
	G4double x = rasXCmd->GetNewDoubleValue(newValue);
	fBeamTarg->fRasterX = x;
    }

    if( cmd == rasYCmd ){
	G4double y = rasYCmd->GetNewDoubleValue(newValue);
	fBeamTarg->fRasterY = y;
    }

    if( cmd == beamX0Cmd ){
	G4double x = beamX0Cmd->GetNewDoubleValue(newValue);
	fBeamTarg->fX0 = x;
    }

    if( cmd == beamY0Cmd ){
	G4double y = beamY0Cmd->GetNewDoubleValue(newValue);
	fBeamTarg->fY0 = y;
    }

    if( cmd == beamth0Cmd ){
	G4double x = beamth0Cmd->GetNewDoubleValue(newValue);
	fBeamTarg->fTh0 = x;
    }

    if( cmd == beamph0Cmd ){
	G4double y = beamph0Cmd->GetNewDoubleValue(newValue);
	fBeamTarg->fPh0 = y;
    }

    if( cmd == beamCorrThCmd ){
	G4double x = beamCorrThCmd->GetNewDoubleValue(newValue);
	fBeamTarg->fCorrTh = tan(x);
    }

    if( cmd == beamCorrPhCmd ){
	G4double y = beamCorrPhCmd->GetNewDoubleValue(newValue);
	fBeamTarg->fCorrPh = tan(y);
    }

    if( cmd == beamdthCmd ){
	G4double x = beamdthCmd->GetNewDoubleValue(newValue);
	fBeamTarg->fdTh = x;
    }

    if( cmd == beamdphCmd ){
	G4double y = beamdphCmd->GetNewDoubleValue(newValue);
	fBeamTarg->fdPh = y;
    }
}
