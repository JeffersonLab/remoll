#ifndef __MOLLERDETECTORCONSTRUCTION_HH
#define __MOLLERDETECTORCONSTRUCTION_HH

#include "G4GDMLParser.hh"
#include "G4VUserDetectorConstruction.hh"
#include "G4Types.hh"

#include <vector>

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

  public:

    G4VPhysicalVolume* Construct();
    void ConstructSDandField();

    void ReloadGeometry(const G4String gdmlfile);

    void SetDetectorGeomFile(G4String name) {
      size_t i = name.rfind('/');
      if (i != std::string::npos) {
        fGDMLPath = name.substr(0,i);
      } else fGDMLPath = ".";
      fGDMLFile = name.substr(i + 1);
    }

    void SetUserLimits(G4String type, G4String name, G4String value_units);
    void SetUserLimit(G4UserLimits* userlimits, const G4String limit, const G4String value_units);

    void SetUserMaxAllowedStep(G4String name, G4String value_units);
    void SetUserMaxTrackLength(G4String name, G4String value_units);
    void SetUserMaxTime(G4String name, G4String value_units);
    void SetUserMinEkine(G4String name, G4String value_units);
    void SetUserMinRange(G4String name, G4String value_units);

  private:

    G4String fGDMLPath;
    G4String fGDMLFile;
    G4GDMLParser *fGDMLParser;

    G4bool fGDMLValidate;
    G4bool fGDMLOverlapCheck;

    G4GenericMessenger* fMessenger;
    G4GenericMessenger* fGeometryMessenger;
    G4GenericMessenger* fUserLimitsMessenger;

    G4int fVerboseLevel;

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

    void PrintAuxiliaryInfo() const;
    void ParseAuxiliaryTargetInfo();
    void ParseAuxiliaryUserLimits();
    void ParseAuxiliaryVisibilityInfo();
    void ParseAuxiliarySensDetInfo();

    void LoadMagneticField();

    G4int UpdateCopyNo(G4VPhysicalVolume* aVolume, G4int index = 0);

};

#endif//__MOLLERDETECTORCONSTRUCTION_HH
