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

#include "remollIO.hh"
#include "remollMessenger.hh"
#include "remollPhysicsList.hh"
#include "remollActionInitialization.hh"
#include "remollDetectorConstruction.hh"
#include "remollParallelWorldConstruction.hh"

#include "G4VisExecutive.hh"
#include "G4UIExecutive.hh"

#ifdef __APPLE__
#include <unistd.h>
#endif

#include <ctime>
#include <fstream>

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


    // Initialize the random seed
    G4long seed = time(0) + (int) getpid();
    // Open /dev/urandom
    std::ifstream urandom("/dev/urandom", std::ios::in | std::ios::binary);
    // If stream is open
    if (urandom) {
      urandom.read(reinterpret_cast<char*>(&seed), sizeof(seed));
      urandom.close();
    } else G4cerr << "Can't read /dev/urandom." << G4endl;


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
    G4Random::setTheSeed(seed);
    remollRun::UpdateSeed();

    // Messenger
    // TODO only thing the messenger does is set the seed, this could and should
    // be done by /random/ commands in next major version
    remollMessenger* messenger = remollMessenger::GetInstance();

    // Detector geometry
    remollDetectorConstruction* detector = new remollDetectorConstruction();
    runManager->SetUserInitialization(detector);

    // Parallel world geometry
    G4String parallelWorldName = "ParallelWorld";
    remollParallelWorldConstruction* parallelWorld = new remollParallelWorldConstruction(parallelWorldName);
    runManager->SetUserInitialization(detector);

    // Physics list
    remollPhysicsList* physlist = new remollPhysicsList();
    runManager->SetUserInitialization(physlist);

    // Run action
    remollActionInitialization* useraction = new remollActionInitialization();
    runManager->SetUserInitialization(useraction);

    //----------------
    // Visualization:
    //----------------
    // Initialize visualization
    //
    // Verbose level "warnings" (3) as is too noisy during initialization
    G4VisManager* visManager = new G4VisExecutive("quiet");
    visManager->Initialize();
    visManager->SetVerboseLevel("warnings");

    // Get the pointer to the User Interface manager
    G4UImanager* UImanager = G4UImanager::GetUIpointer();

    // Define UI session for interactive mode
    if (macro.size())
    {
      // Run in batch mode
      // Copy contents of macro into buffer to be written out into ROOT file
      remollRun::GetRunData()->SetMacroFile(macro);
      UImanager->ExecuteMacroFile(macro);
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

    // Return success
    return 0;
}
