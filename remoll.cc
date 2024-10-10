/*!
  remoll - 12 GeV Moller Simluation

  Seamus Riordan, et al.
  riordan@jlab.org

*/

#include "G4Types.hh"

#ifdef G4MULTITHREADED
#include "G4MTRunManager.hh"
typedef G4MTRunManager RunManager;
#else
#include "G4RunManager.hh"
typedef G4RunManager RunManager;
#endif

#include "G4Version.hh"
#include "G4UImanager.hh"

#include "remollIO.hh"

#include "remollRun.hh"
#include "remollRunData.hh"

#include "remollPhysicsList.hh"
#include "remollActionInitialization.hh"
#include "remollDetectorConstruction.hh"
#include "remollParallelConstruction.hh"
#include "remollGDMLReadStructure.hh"

#include "remollSearchPath.hh"

#include "G4VisExecutive.hh"
#include "G4UIExecutive.hh"

#include "TROOT.h"

#ifdef __APPLE__
#include <unistd.h>
#endif

#include <ctime>
#include <fstream>

namespace {
  void PrintUsage() {
    G4cerr << "Usage: " << G4endl;
    G4cerr << " remoll [-f] [-g geometry] [-m macro] [-u UIsession] [-r seed] [-o outputfile] ";
#ifdef G4MULTITHREADED
    G4cerr << "[-t nThreads] ";
#endif
    G4cerr << "[macro]" << G4endl;
  }
}

int main(int argc, char** argv) {

    // Running time measurement: start
    clock_t tStart = clock();

    #if ROOT_VERSION_CODE >= ROOT_VERSION(6,0,0)
    // Fix for #40: avoids LLVM/GL errors, but only for ROOT 6:
    // make sure gROOT is loaded before LLVM by using it first
    gROOT->Reset();
    #endif

    // Warn if LIBGL_ALWAYS_INDIRECT is set
    if (std::getenv("LIBGL_ALWAYS_INDIRECT") != nullptr) {
      G4cerr << "remoll: Environment variable LIBGL_ALWAYS_INDIRECT is set." << G4endl;
      G4cerr << "remoll: This may interfere with visualization. Unset wih:" << G4endl;
      G4cerr << "remoll: tcsh>  unsetenv LIBGL_ALWAYS_INDIRECT" << G4endl;
      G4cerr << "remoll: bash>  unset LIBGL_ALWAYS_INDIRECT" << G4endl;
    }

    // Initialize the random seed
    G4long seed = time(0) + (int) getpid();
    // Open /dev/urandom
    std::ifstream urandom("/dev/urandom", std::ios::in | std::ios::binary);
    // If stream is open
    if (urandom) {
      urandom.read(reinterpret_cast<char*>(&seed), sizeof(seed));
      urandom.close();
      seed = labs(seed);
    } else G4cerr << "Can't read /dev/urandom." << G4endl;


    // Parse command line options
    G4String macro;
    G4String session;
    G4String geometry_gdmlfile;
    G4String parallel_gdmlfile;
    G4String outputfile;
    __attribute__((unused)) G4bool force = false;
#ifdef G4MULTITHREADED
    G4int threads = 0;
#endif
    //
    for (G4int i = 1; i < argc; ++i) {
      if      (G4String(argv[i]) == "-m") macro    = argv[++i];
      else if (G4String(argv[i]) == "-g") geometry_gdmlfile = argv[++i];
      else if (G4String(argv[i]) == "-p") parallel_gdmlfile = argv[++i];
      else if (G4String(argv[i]) == "-u") session  = argv[++i];
      else if (G4String(argv[i]) == "-r") seed     = atol(argv[++i]);
      else if (G4String(argv[i]) == "-o") outputfile = argv[++i];
      else if (G4String(argv[i]) == "-f") force    = true;
#ifdef G4MULTITHREADED
      else if (G4String(argv[i]) == "-t") threads  = atoi(argv[++i]);
#endif
      else if (argv[i][0] != '-') macro = argv[i];
      else {
        PrintUsage();
        return 1;
      }
    }

    //-------------------------------
    // Check dependency versions
    //-------------------------------
    #if G4VERSION_NUMBER < 1060
    if (! force) {
      G4cerr << "WARNING: You are running with an older geant4 version." << G4endl;
      G4cerr << "WARNING: The encouraged version of geant4 is 10.6.2." << G4endl;
      G4cerr << "WARNING: Pass the option '-f' to ignore this warning." << G4endl;
      exit(-1);
    }
    #endif

    #if ROOT_VERSION_CODE < ROOT_VERSION(6,14,4)
    if (! force) {
      G4cerr << "WARNING: You are running with an older ROOT version." << G4endl;
      G4cerr << "WARNING: The encouraged version of ROOT is 6.14.4." << G4endl;
      G4cerr << "WARNING: Pass the option '-f' to ignore this warning." << G4endl;
      exit(-1);
    }
    #endif


    //-------------------------------
    // Initialization of Run manager
    //-------------------------------
    RunManager* runManager = new RunManager;
    #ifdef G4MULTITHREADED
    if (threads > 0) runManager->SetNumberOfThreads(threads);
    #endif

    // Set the default random seed
    G4cout << G4endl << "remoll: Random seed: " << seed << G4endl;
    G4Random::setTheSeed(seed);

    // Create remoll IO object with output file name
    if (outputfile.size() > 0)
      remollIO::GetInstance(outputfile);
    else
      remollIO::GetInstance();

    // Detector geometry
    remollGDMLReadStructure* rs = new remollGDMLReadStructure();
    G4String material_name = "material";
    remollDetectorConstruction* detector = new remollDetectorConstruction(material_name, geometry_gdmlfile,rs);
    // Parallel world geometry
    G4String parallel_name = "parallel"; // Note: name must correspond with name of G4ParallelWorldPhysics
    remollParallelConstruction* parallel = new remollParallelConstruction(parallel_name, parallel_gdmlfile);
    detector->RegisterParallelWorld(parallel);
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
    // use double precision
    #if G4VERSION_NUMBER >= 1031
    G4UImanager::UseDoublePrecisionStr(true);
    #else
    G4cout << "remoll: Warning: issue 130, double precision input may be truncated." << G4endl;
    G4cout << "remoll: see also https://github.com/JeffersonLab/remoll/issues/130" << G4endl;
    #endif


    // Define UI session for interactive mode
    G4String searchpath = ".";
    searchpath += ":" + std::string(CMAKE_INSTALL_FULL_DATADIR) + "/remoll";
    searchpath += ":" + std::string(CMAKE_INSTALL_FULL_DATADIR) + "/remoll/macros";
    if (macro.size() != 0u)
    {
      // Run in batch mode
      // Copy contents of macro into buffer to be written out into ROOT file
      UImanager->SetMacroSearchPath(searchpath);
      UImanager->ParseMacroSearchPath();
      remollRun::GetRunData()->SetMacroFile((remollSearchPath::resolve(macro)).c_str());
      UImanager->ExecuteMacroFile((remollSearchPath::resolve(macro)).c_str());
    } else {
      // Define UI session for interactive mode
      G4UIExecutive* ui = new G4UIExecutive(argc,argv,session);
      if (ui->IsGUI()) {
        UImanager->SetMacroSearchPath(searchpath);
        UImanager->ParseMacroSearchPath();
        UImanager->ExecuteMacroFile((remollSearchPath::resolve("macros/gui.mac")).c_str());
      }
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
