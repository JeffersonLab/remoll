#ifndef remollMessenger_HH
#define remollMessenger_HH

#include "globals.hh"
#include "remolltypes.hh"
#include "G4UImessenger.hh"
#include "G4UIcommand.hh"

/*!
 *   Global messenger class
 */

class remollIO;
class remollDetectorConstruction;
class remollEventAction;
class remollPrimaryGeneratorAction;
class remollGlobalField;
class remollBeamTarget;

class G4UIcmdWithAnInteger;
class G4UIcmdWithADoubleAndUnit;
class G4UIcmdWithAString;
class G4UIcmdWithABool;

class remollMessenger : public G4UImessenger {
    public:
       	remollMessenger();
       	~remollMessenger();

	void SetIO( remollIO *io ){ fIO = io; }
	void SetMagField( remollGlobalField *f ){ fField = f; }
	void SetPriGen( remollPrimaryGeneratorAction *pg ){ fprigen = pg; }
	void SetDetCon( remollDetectorConstruction *dc ){ fdetcon= dc; }
	void SetEvAct( remollEventAction *ev ){ fevact = ev; }

	void SetNewValue(G4UIcommand* cmd, G4String newValue);

    private:
	remollIO *fIO;
	remollDetectorConstruction *fdetcon;
	remollEventAction *fevact;
	remollPrimaryGeneratorAction *fprigen;
	remollGlobalField *fField;
	remollBeamTarget *fBeamTarg;

	G4UIcmdWithAString   *newfieldCmd;
	G4UIcmdWithAString   *fieldScaleCmd;
	G4UIcmdWithAString   *fieldCurrCmd;
	G4UIcmdWithAString   *genSelectCmd;

	G4UIcmdWithADoubleAndUnit *tgtLenCmd;
	G4UIcmdWithADoubleAndUnit *tgtPosCmd;

	G4UIcmdWithADoubleAndUnit *beamCurrCmd;

	G4UIcmdWithAString   *fileCmd;

	////////////////////////////////////////////////
	// To general event generators
	G4UIcmdWithADoubleAndUnit *thminCmd;
	G4UIcmdWithADoubleAndUnit *thmaxCmd;
	G4UIcmdWithADoubleAndUnit *thCoMminCmd;
	G4UIcmdWithADoubleAndUnit *thCoMmaxCmd;
	G4UIcmdWithADoubleAndUnit *EminCmd;

	/*
	G4UIcmdWithAnInteger *runCmd;
	G4UIcmdWithAString   *tgtCmd;


	G4UIcmdWithAString   *kineCmd;
	G4UIcmdWithAString   *expCmd;

	G4UIcmdWithABool *geantinoCmd;

	G4UIcmdWithAnInteger *gemconfigCmd;

	G4UIcmdWithADoubleAndUnit *tgtLenCmd;
	G4UIcmdWithADoubleAndUnit *tgtDenCmd;
	G4UIcmdWithADoubleAndUnit *tgtPresCmd;
	G4UIcmdWithADoubleAndUnit *beamcurCmd;
	G4UIcmdWithADoubleAndUnit *runtimeCmd;
	G4UIcmdWithADoubleAndUnit *rasterxCmd;
	G4UIcmdWithADoubleAndUnit *rasteryCmd;

	G4UIcmdWithADoubleAndUnit *beamECmd;

	G4UIcmdWithADoubleAndUnit *bbangCmd;
	G4UIcmdWithADoubleAndUnit *bbdistCmd;

	G4UIcmdWithADoubleAndUnit *hcaldistCmd;
	G4UIcmdWithADoubleAndUnit *hmagdistCmd;
	G4UIcmdWithADoubleAndUnit *hcalangCmd;

	G4UIcmdWithADoubleAndUnit *thminCmd;
	G4UIcmdWithADoubleAndUnit *thmaxCmd;
	G4UIcmdWithADoubleAndUnit *phminCmd;
	G4UIcmdWithADoubleAndUnit *phmaxCmd;

	G4UIcmdWithADoubleAndUnit *cerDepCmd;
	G4UIcmdWithADoubleAndUnit *cerDisCmd;
	G4UIcmdWithADoubleAndUnit *gemSepCmd;
	G4UIcmdWithADoubleAndUnit *bbCalDistCmd;

	G4UIcmdWithADoubleAndUnit *gemresCmd;
	*/

};

#endif//remollMessenger_HH























