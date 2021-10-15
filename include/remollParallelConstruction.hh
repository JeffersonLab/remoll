#ifndef __REMOLLPARALLELCONSTRUCTION_HH
#define __REMOLLPARALLELCONSTRUCTION_HH 1

#include <G4VUserParallelWorld.hh>
#include <G4GenericMessenger.hh>
#include <G4GDMLParser.hh>

#include "remollSearchPath.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;

//
/// A parallel world construction class
///
/// - void Construct()
///     creates a parallel world in the mass world and parameterized volumes
//
class remollParallelConstruction : public G4VUserParallelWorld
{
  public:
    remollParallelConstruction(const G4String& name, const G4String& gdmlfile);
    virtual ~remollParallelConstruction();

  public:

    virtual void Construct();
    virtual void ConstructSD();

  private:
    G4String fGDMLPath;
    G4String fGDMLFile;

    void SetGDMLFile(G4String gdmlfile) {
      gdmlfile = remollSearchPath::resolve(gdmlfile);
      size_t i = gdmlfile.rfind('/');
      if (i != std::string::npos) {
        fGDMLPath = gdmlfile.substr(0,i);
      } else fGDMLPath = ".";
      fGDMLFile = gdmlfile.substr(i + 1);
    }

    G4GDMLParser fGDMLParser;

    G4bool fGDMLValidate;
    G4bool fGDMLOverlapCheck;

    G4int fVerboseLevel;

    G4GenericMessenger fParallelMessenger{
      this,
      "/remoll/parallel/",
      "Remoll parallel geometry properties"};

    G4VPhysicalVolume* fWorldVolume;
    G4String           fWorldName;

  private:

    void PrintGDMLWarning() const;

    G4VPhysicalVolume* ParseGDMLFile();

    void PrintAuxiliaryInfo() const;
    void ParseAuxiliaryVisibilityInfo();
    void ParseAuxiliarySensDetInfo();

};

#endif // __REMOLLPARALLELCONSTRUCTION_HH
