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

    seedCmd = new G4UIcmdWithAnInteger("/remoll/seed",this);
    seedCmd->SetGuidance("Set random engine seed");
    seedCmd->SetParameterName("seed", false);

    kryptCmd = new G4UIcmdWithABool("/remoll/kryptonite",this);
    kryptCmd->SetGuidance("Treat W, Pb, Cu as kryptonite");
    kryptCmd->SetParameterName("krypt", false);

    opticalCmd = new G4UIcmdWithABool("/remoll/optical",this);
    opticalCmd->SetGuidance("Enable optical physics");
    opticalCmd->SetParameterName("optical", false);

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

    tgtPosCmd = new G4UIcmdWithADoubleAndUnit("/remoll/targpos",this);
    tgtPosCmd->SetGuidance("Target length");
    tgtPosCmd->SetParameterName("targlen", false);

    beamCurrCmd = new G4UIcmdWithADoubleAndUnit("/remoll/beamcurr",this);
    beamCurrCmd->SetGuidance("Beam current");
    beamCurrCmd->SetParameterName("beamcurr", false);

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

    /*
       char cmdstr[255];
       if( cmd == runCmd ){
// Save geometry to GDML file
G4GDMLParser parser;
G4VPhysicalVolume* pWorld;

G4int nevt = runCmd->GetNewIntValue(newValue);
fevgen->SetNevents(nevt);

if( fExpType == kGEp ){
G4RunManager::GetRunManager()->DefineWorldVolume(pWorld = fdetcon->ConstructAllGEp());
} else {
G4RunManager::GetRunManager()->DefineWorldVolume(pWorld = fdetcon->ConstructAll());
}

// Clobber old gdml if it exists and write out the
// present geometry
unlink("g4sbs.gdml");
parser.Write("g4sbs.gdml", pWorld);

// Run the simulation
G4UImanager * UImanager = G4UImanager::GetUIpointer();
sprintf(cmdstr, "/run/beamOn %d", nevt);
UImanager->ApplyCommand(cmdstr);
}


if( cmd == sigfileCmd ){
fevact->LoadSigmas(newValue.data());
}

if( cmd == gemconfigCmd ){
int gemconfval = gemconfigCmd->GetNewIntValue(newValue);
fdetcon->SetGEMConfig(gemconfval);
}

if( cmd == kineCmd ){
bool validcmd = false;
if( newValue.compareTo("elastic") == 0 ){
fevgen->SetKine(kElastic);
validcmd = true;
}
if( newValue.compareTo("inelastic") == 0 ){
fevgen->SetKine(kInelastic);
validcmd = true;
}
if( newValue.compareTo("flat") == 0 ){
fevgen->SetKine(kFlat);
validcmd = true;
}
if( newValue.compareTo("dis") == 0 ){
fevgen->SetKine(kDIS);
validcmd = true;
}
if( !validcmd ){
fprintf(stderr, "%s: %s line %d - Error: kinematic type %s not valid\n", __PRETTY_FUNCTION__, __FILE__, __LINE__, newValue.data());
exit(1);
}
}

if( cmd == expCmd ){
bool validcmd = false;
if( newValue.compareTo("gep") == 0 ){
fExpType = kGEp;
validcmd = true;
}
if( newValue.compareTo("gmn") == 0 ){
fExpType = kNeutronExp;
validcmd = true;
}
if( newValue.compareTo("gen") == 0 ){
    fExpType = kNeutronExp;
    validcmd = true;
}
if( newValue.compareTo("a1n") == 0 ){
    fExpType = kNeutronExp;
    validcmd = true;
}
if( !validcmd ){
    fprintf(stderr, "%s: %s line %d - Error: kinematic type %s not valid\n", __PRETTY_FUNCTION__, __FILE__, __LINE__, newValue.data());
    exit(1);
}
}



if( cmd == tgtCmd ){
    bool validcmd = false;
    if( newValue.compareTo("LH2") == 0 ){
	fevgen->SetTarget(kLH2);
	fdetcon->SetTarget(kLH2);

	G4double den = (0.071*g/cm3)*Avogadro/(1.008*g/mole);
	fevgen->SetTargDen(den);
	fdetcon->SetTargDen(den);
	validcmd = true;
    }
    if( newValue.compareTo("H2") == 0 ){
	fevgen->SetTarget(kH2);
	fdetcon->SetTarget(kH2);

	G4double den = 10.0*atmosphere/(296.0*kelvin*k_Boltzmann);
	fevgen->SetTargDen(den);
	fdetcon->SetTargDen(den);
	validcmd = true;
    }
    if( newValue.compareTo("LD2") == 0 ){
	fevgen->SetTarget(kLD2);
	fdetcon->SetTarget(kLD2);

	G4double den = (162.4*kg/m3)*Avogadro/(2.014*g/mole);
	fevgen->SetTargDen(den);
	fdetcon->SetTargDen(den);
	validcmd = true;
    }
    if( newValue.compareTo("3He") == 0 ){
	fevgen->SetTarget(k3He);
	fdetcon->SetTarget(k3He);

	G4double den = 10.0*atmosphere/(296.0*kelvin*k_Boltzmann);
	fevgen->SetTargDen(den);
	fdetcon->SetTargDen(den);
	validcmd = true;

    }
    if( newValue.compareTo("Neutron") == 0 ){
	fevgen->SetTarget(kNeutTarg);
	fdetcon->SetTarget(kNeutTarg);

	G4double den = 10.0*atmosphere/(296.0*kelvin*k_Boltzmann);
	fevgen->SetTargDen(den);
	fdetcon->SetTargDen(den);
	validcmd = true;
    }

    if( !validcmd ){
	fprintf(stderr, "%s: %s line %d - Error: target type %s not valid\n", __PRETTY_FUNCTION__, __FILE__, __LINE__, newValue.data());
	exit(1);
    }

}

if( cmd == geantinoCmd ){
    G4bool b = geantinoCmd->GetNewBoolValue(newValue);
    fprigen->SetUseGeantino(b);
    fdetcon->GetBBField()->SetUseGeantino(b);
}


if( cmd == tgtDenCmd ){
    G4double den = tgtDenCmd->GetNewDoubleValue(newValue);
    fevgen->SetTargDen(den);
    fdetcon->SetTargDen(den);
}
if( cmd == tgtPresCmd ){
    G4double pre = tgtPresCmd->GetNewDoubleValue(newValue);
    G4double den = pre/(296.0*kelvin*k_Boltzmann);
    fevgen->SetTargDen(den);
    fdetcon->SetTargDen(den);
}

if( cmd == beamcurCmd ){
    G4double v = beamcurCmd->GetNewDoubleValue(newValue);
    printf("Setting beam current to %f uA\n", v/microampere);
    fevgen->SetBeamCur(v);
}
if( cmd == runtimeCmd ){
    G4double v = runtimeCmd->GetNewDoubleValue(newValue);
    fevgen->SetRunTime(v);
}

if( cmd == rasterxCmd ){
    G4double v = rasterxCmd->GetNewDoubleValue(newValue);
    fevgen->SetRasterX(v);
}

if( cmd == rasteryCmd ){
    G4double v = rasteryCmd->GetNewDoubleValue(newValue);
    fevgen->SetRasterY(v);
}

if( cmd == beamECmd ){
    G4double v = beamECmd->GetNewDoubleValue(newValue);
    fevgen->SetBeamE(v);
    fIO->SetBeamE(v);
}

if( cmd == bbangCmd ){
    G4double v = bbangCmd->GetNewDoubleValue(newValue);
    printf("Setting BB ang to %f deg\n", v/deg);
    fdetcon->SetBBAng(v);
    fIO->SetBigBiteTheta(v);
}

if( cmd == bbdistCmd ){
    G4double v = bbdistCmd->GetNewDoubleValue(newValue);
    fdetcon->SetBBDist(v);
    fIO->SetBigBiteDist(v);
}

if( cmd == hcalangCmd ){
    G4double v = hcalangCmd->GetNewDoubleValue(newValue);
    fdetcon->SetHCALAng(v);
    fIO->SetHcalTheta(v);
}

if( cmd == hcaldistCmd ){
    G4double v = hcaldistCmd->GetNewDoubleValue(newValue);
    fdetcon->SetHCALDist(v);
    fevgen->SetHCALDist(v);
    fIO->SetHcalDist(v);
}

if( cmd == hmagdistCmd ){
    G4double v = hmagdistCmd->GetNewDoubleValue(newValue);
    fdetcon->Set48D48Dist(v);
}

if( cmd == cerDepCmd ){
    G4double v = cerDepCmd->GetNewDoubleValue(newValue);
    fdetcon->SetCerDepth(v);
}

if( cmd == cerDisCmd ){
    G4double v = cerDisCmd->GetNewDoubleValue(newValue);
    fdetcon->SetCerDist(v);
}

if( cmd == gemSepCmd ){
    G4double v = gemSepCmd->GetNewDoubleValue(newValue);
    fdetcon->SetGEMSep(v);
}

if( cmd == bbCalDistCmd ){
    G4double v = bbCalDistCmd->GetNewDoubleValue(newValue);
    fdetcon->SetBBCalDist(v);
}

if( cmd == thminCmd ){
    G4double v = thminCmd->GetNewDoubleValue(newValue);
    fevgen->SetThMin(v);
}
if( cmd == thmaxCmd ){
    G4double v = thmaxCmd->GetNewDoubleValue(newValue);
    fevgen->SetThMax(v);
}
if( cmd == phminCmd ){
    G4double v = phminCmd->GetNewDoubleValue(newValue);
    fevgen->SetPhMin(v);
}
if( cmd == phmaxCmd ){
    G4double v = phmaxCmd->GetNewDoubleValue(newValue);
    fevgen->SetPhMax(v);
}

if( cmd == gemresCmd ){
    G4double v = gemresCmd->GetNewDoubleValue(newValue);
    fevact->SetGEMRes(v);
}

*/

}
