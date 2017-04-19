/*!
  remoll - 12 GeV Moller Simluation

  Seamus Riordan, et al.
  riordan@jlab.org

*/

#include "CLHEP/Random/Random.h"

#include "remollRunAction.hh"
#include "remollRun.hh"
#include "remollRunData.hh"
#include "remollPrimaryGeneratorAction.hh"
#include "remollEventAction.hh"
#include "remollSteppingAction.hh"
#include "remollDetectorConstruction.hh"
#include "remollParallelWorldConstruction.hh"

#include "remollIO.hh"
#include "remollMessenger.hh"

//  Standard physics list
#include "G4Version.hh"
#include "G4PhysListFactory.hh"
#include "G4OpticalPhysics.hh"
#if G4VERSION_NUMBER < 1000
#include "LHEP.hh"
#endif

#include "G4RunManager.hh"

#include "G4UnitsTable.hh"

#include "G4RunManagerKernel.hh"

//to make gui.mac work
#include <G4UImanager.hh>
#include <G4UIExecutive.hh>
#include <G4UIterminal.hh>

#ifdef G4UI_USE_QT
#include "G4UIQt.hh"
#endif

#ifdef G4UI_USE_XM
#include "G4UIXm.hh"
#endif

#ifdef G4UI_USE_TCSH
#include "G4UItcsh.hh"
#endif

#ifdef G4VIS_USE
#include "G4VisExecutive.hh"
#endif

#include <sys/types.h>
#include <sys/stat.h>

#ifdef __APPLE__
#include <unistd.h>
#endif

#include <time.h>

int main(int argc, char** argv){

    clock_t tStart=clock();
    // Initialize the CLHEP random engine used by
    // "shoot" type functions
    unsigned int seed = time(0) + (int) getpid();

    unsigned int devrandseed = 0;
    //  /dev/urandom doens't block
    FILE *fdrand = fopen("/dev/urandom", "r");
    if( fdrand ){
	fread(&devrandseed, sizeof(int), 1, fdrand);
	seed += devrandseed;
	fclose(fdrand);
    }

    CLHEP::HepRandom::createInstance();
    CLHEP::HepRandom::setTheSeed(seed);

    remollRun::GetRun()->GetData()->SetSeed(seed);

    remollIO *io = new remollIO();

    //-------------------------------
    // Initialization of Run manager
    //-------------------------------
    G4cout << "RunManager construction starting...." << G4endl;
    G4RunManager * runManager = new G4RunManager;

    // Detector geometry
    remollDetectorConstruction* detector = new remollDetectorConstruction();

    // Parallel world geometry
    G4String parallelWorldName = "ParallelWorld";
    remollParallelWorldConstruction* parallelWorld = 
      new remollParallelWorldConstruction(parallelWorldName);
    detector->RegisterParallelWorld(parallelWorld);
    runManager->SetUserInitialization(detector);
    detector->SetIO(io);


    // Messenger
    remollMessenger *rmmess = new remollMessenger();
    rmmess->SetIO(io);
    rmmess->SetDetCon(detector);
    rmmess->SetMagField(detector->GetGlobalField() );

    // Physics we want to use
    G4int verbose = 0;
    G4PhysListFactory factory;
    #if G4VERSION_NUMBER < 1000
    G4VModularPhysicsList* physlist = factory.GetReferencePhysList("LHEP");
    #else
    //G4VModularPhysicsList* physlist = factory.GetReferencePhysList("FTFP_BERT_LIV");
    G4VModularPhysicsList* physlist = factory.GetReferencePhysList("QGSP_BERT_HP");
    #endif
    physlist->RegisterPhysics(new G4OpticalPhysics());
    physlist->SetVerboseLevel(verbose);
    runManager->SetUserInitialization(physlist);

    //-------------------------------
    // UserAction classes
    //-------------------------------
    G4UserRunAction* run_action = new remollRunAction;
    ((remollRunAction *) run_action)->SetIO(io);
    runManager->SetUserAction(run_action);

    G4VUserPrimaryGeneratorAction* gen_action = new remollPrimaryGeneratorAction;
    ((remollPrimaryGeneratorAction *) gen_action)->SetIO(io);
    rmmess->SetPriGen((remollPrimaryGeneratorAction *)gen_action);
    runManager->SetUserAction(gen_action);

    G4UserEventAction* event_action = new remollEventAction;
    ((remollEventAction *) event_action)->SetIO(io);

    runManager->SetUserAction(event_action);
    G4UserSteppingAction* stepping_action = new remollSteppingAction;
    runManager->SetUserAction(stepping_action);
    rmmess->SetStepAct((remollSteppingAction *) stepping_action);

    // New units

    G4UIsession* session = 0;

    //----------------
    // Visualization:
    //----------------

    if (argc==1)   // Define UI session for interactive mode.
    {

	// G4UIterminal is a (dumb) terminal.

#if defined(G4UI_USE_XM)
	session = new G4UIXm(argc,argv);
#elif defined(G4UI_USE_WIN32)
	session = new G4UIWin32();
#elif defined(G4UI_USE_QT)
	session = new G4UIQt(argc,argv);
#elif defined(G4UI_USE_TCSH)
	session = new G4UIterminal(new G4UItcsh);
#else
	session = new G4UIterminal();
#endif

    }

    remollRunData *rundata = remollRun::GetRun()->GetData();

#ifdef G4VIS_USE
    // Visualization, if you choose to have it!
    //
    // Simple graded message scheme - give first letter or a digit:
    //  0) quiet,         // Nothing is printed.
    //  1) startup,       // Startup and endup messages are printed...
    //  2) errors,        // ...and errors...
    //  3) warnings,      // ...and warnings...
    //  4) confirmations, // ...and confirming messages...
    //  5) parameters,    // ...and parameters of scenes and views...
    //  6) all            // ...and everything available.

    //this is the initializing the run manager?? Right?
    G4VisManager* visManager = new G4VisExecutive;
    //visManager -> SetVerboseLevel (1);
    visManager ->Initialize();
#endif

    //get the pointer to the User Interface manager
    G4UImanager * UI = G4UImanager::GetUIpointer();

    if (session)   // Define UI session for interactive mode.
    {
	// G4UIterminal is a (dumb) terminal.
	//UI->ApplyCommand("/control/execute myVis.mac");

#if defined(G4UI_USE_XM) || defined(G4UI_USE_WIN32) || defined(G4UI_USE_QT)
	// Customize the G4UIXm,Win32 menubar with a macro file :
	UI->ApplyCommand("/control/execute macros/gui.mac");
#endif

	session->SessionStart();
	delete session;
    }
    else           // Batch mode - not using the GUI
    {
#ifdef G4VIS_USE
	visManager->SetVerboseLevel("quiet");
#endif
	//these line will execute a macro without the GUI
	//in GEANT4 a macro is executed when it is passed to the command, /control/execute
	G4String command = "/control/execute ";
	G4String fileName = argv[1];

	/* Copy contents of macro into buffer to be written out
	 * into ROOT file
	 * */
	rundata->SetMacroFile(argv[1]);


	UI->ApplyCommand(command+fileName);
    }

    //if one used the GUI then delete it
#ifdef G4VIS_USE
    delete visManager;
#endif

    // Initialize Run manager
    // runManager->Initialize();
    G4cout<<" Running time[s]: "<< (double) ((clock() - tStart)/CLOCKS_PER_SEC)<<G4endl;
    return 0;
}
