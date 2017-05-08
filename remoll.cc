/*!
  remoll - 12 GeV Moller Simluation

  Seamus Riordan, et al.
  riordan@jlab.org

*/

#ifdef G4MULTITHREADED
#include "G4MTRunManager.hh"
typedef G4MTRunManager RunManager;
#else
#include "G4RunManager.hh"
typedef G4RunManager RunManager;
#endif

#include "G4Types.hh"
#include "G4UImanager.hh"

#include "remollRun.hh"
#include "remollRunData.hh"
#include "remollMessenger.hh"
#include "remollActionInitialization.hh"
#include "remollDetectorConstruction.hh"

//  Standard physics list
#include "G4PhysListFactory.hh"
#include "G4OpticalPhysics.hh"

#include "G4VisExecutive.hh"
#include "G4UIExecutive.hh"

#ifdef __APPLE__
#include <unistd.h>
#endif

#include <ctime>

namespace {
  void PrintUsage() {
    G4cerr << "Usage: " << G4endl;
    G4cerr << " remoll [-m macro ] [-u UIsession] [-r seed] ";
#ifdef G4MULTITHREADED
    G4cerr << "[-t nThreads] ";
#endif
    G4cerr << "[macro]" << G4endl;
  }
}


int main(int argc, char** argv) {

    // Running time measurement: start
    clock_t tStart = clock();


    // Initialize the CLHEP random engine
    unsigned int seed = time(0) + (int) getpid();
    unsigned int devrandseed = 0;
    //  /dev/urandom doens't block
    FILE *fdrand = fopen("/dev/urandom", "r");
    if (fdrand) {
	if (fread(&devrandseed, sizeof(int), 1, fdrand)) {
	  seed += devrandseed;
	} else G4cerr << "Can't read /dev/urandom." << G4endl;
	fclose(fdrand);
    }


    // Parse command line options
    G4String macro;
    G4String session;
#ifdef G4MULTITHREADED
    G4int threads = 0;
#endif
    //
    for (G4int i = 1; i < argc; ++i) {
      if      (G4String(argv[i]) == "-m") macro   = argv[++i];
      else if (G4String(argv[i]) == "-u") session = argv[++i];
      else if (G4String(argv[i]) == "-r") seed    = atoi(argv[++i]);
#ifdef G4MULTITHREADED
      else if (G4String(argv[i]) == "-t") threads = atoi(argv[++i]);
#endif
      else if (argv[i][0] != '-') macro = argv[i];
      else {
        PrintUsage();
        return 1;
      }
    }


    //-------------------------------
    // Initialization of Run manager
    //-------------------------------
    G4cout << "RunManager construction starting...." << G4endl;
    RunManager* runManager = new RunManager;
    #ifdef G4MULTITHREADED
    if (threads > 0) runManager->SetNumberOfThreads(threads);
    #endif

    // Choose the Random engine
    CLHEP::RanecuEngine defaultEngine(seed);
    G4Random::setTheEngine(&defaultEngine);
    remollRun::GetRunData()->SetSeed(seed);

    // Messenger
    remollMessenger* messenger = remollMessenger::GetInstance();

    // Detector geometry
    remollDetectorConstruction* detector = new remollDetectorConstruction();
    runManager->SetUserInitialization(detector);

    // Physics list
    G4PhysListFactory factory;
    G4VModularPhysicsList* physlist = factory.GetReferencePhysList("QGSP_BERT_HP");
    physlist->RegisterPhysics(new G4OpticalPhysics());
    runManager->SetUserInitialization(physlist);

    // Run action
    remollActionInitialization* useraction = new remollActionInitialization();
    runManager->SetUserInitialization(useraction);

    //----------------
    // Visualization:
    //----------------
    // Initialize visualization
    //
    G4VisManager* visManager = new G4VisExecutive;
    visManager->Initialize();

    // Get the pointer to the User Interface manager
    G4UImanager* UImanager = G4UImanager::GetUIpointer();

    // Define UI session for interactive mode
    if (macro.size())
    {
      // Run in batch mode
      G4String command = "/control/execute ";
      // Copy contents of macro into buffer to be written out into ROOT file
      remollRun::GetRunData()->SetMacroFile(macro);
      UImanager->ApplyCommand(command + macro);
    } else {
      // Define UI session for interactive mode
      G4UIExecutive* ui = new G4UIExecutive(argc,argv,session);
      if (ui->IsGUI())
        UImanager->ApplyCommand("/control/execute macros/gui.mac");
      ui->SessionStart();
      delete ui;
    }


    // Job termination
    // Free the store: user actions, physics_list and detector_description are
    //                 owned and deleted by the run manager, so they should not
    //                 be deleted in the main() program !
    delete visManager;
    delete runManager;


    // Running time measurement: end
    clock_t tEnd = clock();
    G4cout << " Running time[s]: "
        << double(tEnd - tStart) / double(CLOCKS_PER_SEC)
        << G4endl;


    return 0;
}
