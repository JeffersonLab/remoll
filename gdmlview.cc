#include "G4Types.hh"
#include "G4Version.hh"

#include "G4VUserDetectorConstruction.hh"
#include "G4VUserPhysicsList.hh"
#include "G4VUserPrimaryGeneratorAction.hh"

#include "G4GDMLParser.hh"
#include "G4RunManager.hh"
#include "G4UImanager.hh"
#include "G4UIQt.hh"
#include "G4VisExecutive.hh"

#include <sys/param.h>

class DetectorConstruction : public G4VUserDetectorConstruction
{
  public:
    DetectorConstruction(const G4String& gdmlfile) { SetGDMLFile(gdmlfile); };
    virtual ~DetectorConstruction() { };

    G4VPhysicalVolume* Construct() {
      // Change directory
      char cwd[MAXPATHLEN];
      if (!getcwd(cwd,MAXPATHLEN)) {
        G4cerr << __FILE__ << " line " << __LINE__ << ": ERROR no current working directory" << G4endl;
        exit(-1);
      }
      if (chdir(fGDMLPath)) {
        G4cerr << __FILE__ << " line " << __LINE__ << ": ERROR cannot change directory" << G4endl;
        exit(-1);
      }

      // Parse GDML file
      G4GDMLParser parser;
      parser.Read(fGDMLFile);
      // Change directory back
      if (chdir(cwd)) {
        G4cerr << __FILE__ << " line " << __LINE__ << ": ERROR cannot change directory" << G4endl;
        exit(-1);
      }
      return parser.GetWorldVolume();
    };

  private:
    G4String fGDMLPath;
    G4String fGDMLFile;
    void SetGDMLFile(G4String gdmlfile) {
      size_t i = gdmlfile.rfind('/');
      if (i != std::string::npos) {
        fGDMLPath = gdmlfile.substr(0,i);
      } else fGDMLPath = ".";
      fGDMLFile = gdmlfile.substr(i + 1);
    }

};

class PhysicsList : public G4VUserPhysicsList
{
  public:
    PhysicsList() { };
    virtual ~PhysicsList() { };
  protected:
    void ConstructParticle() { };
    void ConstructProcess() { };
};

class PrimaryGeneratorAction : public G4VUserPrimaryGeneratorAction
{
  public:
    PrimaryGeneratorAction() { };
    virtual ~PrimaryGeneratorAction() { };
    virtual void GeneratePrimaries(G4Event*) { };
};

namespace
{
  void PrintUsage() {
    G4cerr << "Usage: " << G4endl;
    G4cerr << " gdmlview [gdml file]" << G4endl;
  };
}

int main(int argc, char** argv)
{
  // Parse command line options
  G4String gdmlfile;
  if (argc > 0) gdmlfile = argv[1];
  else {
    PrintUsage();
    return 1;
  }

  // Run manager
  G4RunManager* rm = new G4RunManager;
  rm->SetUserInitialization(new DetectorConstruction(gdmlfile));
  rm->SetUserInitialization(new PhysicsList);
  rm->SetUserAction(new PrimaryGeneratorAction);
  rm->Initialize();

  // Visualization
  G4VisManager* vm = new G4VisExecutive("quiet");
  vm->Initialize();

  // Start user interface
  G4UIQt* ui = new G4UIQt(argc, argv);
  ui->GetUserInterfaceWidget()->setVisible(false);
  ui->GetCoutDockWidget()->setVisible(false);
  G4UImanager* um = G4UImanager::GetUIpointer();
  um->ApplyCommand("/vis/open OGLSQt 1200x800");
  um->ApplyCommand("/vis/drawVolume worlds");
  ui->SessionStart();

  delete ui; delete vm; delete rm;
  return 0;
}
