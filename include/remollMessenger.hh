#ifndef remollMessenger_HH
#define remollMessenger_HH

#include "globals.hh"
#include "remolltypes.hh"
#include "G4UImessenger.hh"
#include "G4UIcommand.hh"
#include "G4VModularPhysicsList.hh"

/*!
 *   Global messenger class
 */

class remollIO;
class remollDetectorConstruction;
class remollEventAction;
class remollPrimaryGeneratorAction;
class remollGlobalField;
class remollBeamTarget;
class remollSteppingAction;

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
	void SetStepAct( remollSteppingAction *st ){ fStepAct = st; }
	void SetPhysList( G4VModularPhysicsList *l ){ fPhysicsList = l; }

	void SetNewValue(G4UIcommand* cmd, G4String newValue);

    private:
	remollIO *fIO;
	remollDetectorConstruction *fdetcon;
	remollEventAction *fevact;
	remollPrimaryGeneratorAction *fprigen;
	remollGlobalField *fField;
	remollBeamTarget *fBeamTarg;
	remollSteppingAction *fStepAct;
	G4VModularPhysicsList *fPhysicsList;

	G4UIcmdWithAnInteger *seedCmd;
	G4UIcmdWithABool     *kryptCmd;
	G4UIcmdWithABool     *opticalCmd;

	G4UIcmdWithAString   *detfilesCmd;

	G4UIcmdWithAString   *newfieldCmd;
	G4UIcmdWithAString   *fieldScaleCmd;
	G4UIcmdWithAString   *fieldCurrCmd;
	G4UIcmdWithAString   *genSelectCmd;

	G4UIcmdWithADoubleAndUnit *tgtLenCmd;
	G4UIcmdWithADoubleAndUnit *tgtPosCmd;

	G4UIcmdWithADoubleAndUnit *beamCurrCmd;
	G4UIcmdWithADoubleAndUnit *beamECmd;

	G4UIcmdWithADoubleAndUnit *rasXCmd;
	G4UIcmdWithADoubleAndUnit *rasYCmd;

	G4UIcmdWithADoubleAndUnit *beamX0Cmd;
	G4UIcmdWithADoubleAndUnit *beamY0Cmd;

	G4UIcmdWithADoubleAndUnit *beamth0Cmd;
	G4UIcmdWithADoubleAndUnit *beamph0Cmd;

	G4UIcmdWithADoubleAndUnit *beamCorrThCmd;
	G4UIcmdWithADoubleAndUnit *beamCorrPhCmd;

	G4UIcmdWithADoubleAndUnit *beamdthCmd;
	G4UIcmdWithADoubleAndUnit *beamdphCmd;

	G4UIcmdWithAString   *fileCmd;

	////////////////////////////////////////////////
	// To general event generators
	G4UIcmdWithADoubleAndUnit *thminCmd;
	G4UIcmdWithADoubleAndUnit *thmaxCmd;
	G4UIcmdWithADoubleAndUnit *thCoMminCmd;
	G4UIcmdWithADoubleAndUnit *thCoMmaxCmd;
	G4UIcmdWithADoubleAndUnit *EminCmd;
	G4UIcmdWithADoubleAndUnit *EmaxCmd;

};

#endif//remollMessenger_HH























