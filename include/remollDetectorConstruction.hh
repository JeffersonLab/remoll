#ifndef __MOLLERDETECTORCONSTRUCTION_HH
#define __MOLLERDETECTORCONSTRUCTION_HH

#include "G4GDMLParser.hh"
#include "G4GDMLAuxStructType.hh"
#include "G4VUserDetectorConstruction.hh"
#include "G4Types.hh"

#include <vector>
#include <set>

class G4Tubs;
class G4LogicalVolume;
class G4VPhysicalVolume;
class G4VSensitiveDetector;
class G4GenericMessenger;
class G4UserLimits;

class remollGlobalField;

class remollDetectorConstruction : public G4VUserDetectorConstruction
{
  public:

    remollDetectorConstruction(const G4String& name, const G4String& gdmlfile);
    virtual ~remollDetectorConstruction();

  private:

    remollDetectorConstruction(const remollDetectorConstruction&);
    remollDetectorConstruction& operator=(remollDetectorConstruction);


  public:

    G4VPhysicalVolume* Construct();
    void ConstructSDandField();


  public:

    void SetVerboseLevel(G4int verbose) { fVerboseLevel = verbose; };

  private:

    G4int fVerboseLevel;


  private:

    G4GDMLParser *fGDMLParser;

    G4bool fGDMLValidate;
    G4bool fGDMLOverlapCheck;

    G4String fGDMLPath;
    G4String fGDMLFile;

    void SetGDMLFile(G4String gdmlfile) {
      size_t i = gdmlfile.rfind('/');
      if (i != std::string::npos) {
        fGDMLPath = gdmlfile.substr(0,i);
      } else fGDMLPath = ".";
      fGDMLFile = gdmlfile.substr(i + 1);
    }

    G4GenericMessenger* fMessenger;
    G4GenericMessenger* fGeometryMessenger;

    void ReloadGeometry(const G4String gdmlfile);


  public:

    void SetUserLimits(const G4String& type, const G4String& name, const G4String& value_units) const;
    void SetUserLimits(G4LogicalVolume* logical_volume, const G4String& type, const G4String& value_units) const;
    void SetUserLimits(G4UserLimits* userlimits, const G4String& type, const G4String& value_units) const;

    void SetUserMaxAllowedStep(G4String name, G4String value_units);
    void SetUserMaxTrackLength(G4String name, G4String value_units);
    void SetUserMaxTime(G4String name, G4String value_units);
    void SetUserMinEkine(G4String name, G4String value_units);
    void SetUserMinRange(G4String name, G4String value_units);

  private:

    G4GenericMessenger* fUserLimitsMessenger;


  public:

    void SetKryptoniteVerbose(G4int verbose) { fKryptoniteVerbose = verbose; }
    void EnableKryptonite();
    void DisableKryptonite();
    void AddKryptoniteCandidate(G4String name);
    void ListKryptoniteCandidates();
    void EnableKryptoniteVolume(G4String name);

  private:

    G4GenericMessenger* fKryptoniteMessenger;
    static G4UserLimits* fKryptoniteUserLimits;
    G4bool fKryptoniteEnable;
    G4int fKryptoniteVerbose;
    std::set<G4String> fKryptoniteCandidates;
    std::set<G4Material*> fKryptoniteMaterials;

    void SetKryptoniteUserLimits(G4VPhysicalVolume* volume = 0);

    void InitKryptoniteMaterials();


  public:

    void AbsolutePosition(G4String name, G4ThreeVector position);
    void RelativePosition(G4String name, G4ThreeVector position);
    void AbsoluteRotation(G4String name, G4ThreeVector rotation);
    void RelativeRotation(G4String name, G4ThreeVector rotation);

    //----------------------
    // global magnet section
    //----------------------
    //

    static G4ThreadLocal remollGlobalField* fGlobalField;

    G4VPhysicalVolume* fWorldVolume;
    G4String           fWorldName;

  public:

    void PrintElements();
    void PrintMaterials();
    void PrintOverlaps() {
      PrintGeometryTree(0,0,true,false);
    }
    void PrintGeometry(G4bool surfchk = false) {
      PrintGeometryTree(0,0,surfchk,true);
    }
    void PrintGeometryTree(G4VPhysicalVolume* aVolume = 0,
      G4int depth = 0, G4bool surfchk = false, G4bool print = true);

    std::vector<G4VPhysicalVolume*> GetPhysicalVolumes(
        G4VPhysicalVolume* physical_volume,
        const G4LogicalVolume*);

  private:

    void PrintGDMLWarning() const;

    G4VPhysicalVolume* ParseGDMLFile();

    G4bool HasAuxWithType(const G4GDMLAuxListType& list, const G4String& type)
    {
      return NextAuxWithType(list.begin(), list.end(), type) != list.end();
    }
    G4GDMLAuxListType::const_iterator NextAuxWithType(
        const G4GDMLAuxListType::const_iterator& begin,
        const G4GDMLAuxListType::const_iterator& end,
        const G4String& type)
    {
      return std::find_if(begin, end,
        [type](const G4GDMLAuxStructType& element) {
          return element.type.compareTo(type, G4String::ignoreCase) == 0;} );
    }

    void PrintAuxiliaryInfo() const;
    void ParseAuxiliaryTargetInfo();
    void ParseAuxiliaryUserLimits();
    void ParseAuxiliaryVisibilityInfo();
    void ParseAuxiliarySensDetInfo();

    void LoadMagneticField();

    G4int UpdateCopyNo(G4VPhysicalVolume* aVolume, G4int index = 0);

};

#endif//__MOLLERDETECTORCONSTRUCTION_HH
