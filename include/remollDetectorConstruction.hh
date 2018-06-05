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

class remollGlobalField;

class remollDetectorConstruction : public G4VUserDetectorConstruction
{
  public:

    remollDetectorConstruction(const G4String& gdmlfile);
    virtual ~remollDetectorConstruction();

  public:

    G4VPhysicalVolume* Construct();
    void ConstructSDandField();

    void ReloadGeometry(const G4String gdmlfile);

    void SetDetectorGeomFile(const G4String& name) { fGDMLFile = name; }

  private:

    G4String fGDMLFile;
    G4GDMLParser *fGDMLParser;

    G4bool fGDMLValidate;
    G4bool fGDMLOverlapCheck;

    G4GenericMessenger* fMessenger;
    G4GenericMessenger* fGeometryMessenger;

    G4int fVerboseLevel;

    //----------------------
    // global magnet section
    //----------------------
    //

    static G4ThreadLocal remollGlobalField* fGlobalField;

    G4VPhysicalVolume*      fWorldVolume;

  public:

    void PrintElements();
    void PrintMaterials();
    void PrintGeometry(G4bool surfchk = false) {
      PrintGeometryTree(0,0,surfchk);
    }
    void PrintGeometryTree(G4VPhysicalVolume* aVolume = 0,
      G4int depth = 0, G4bool surfchk = false);

    std::vector<G4VPhysicalVolume*> GetPhysicalVolumes(
        G4VPhysicalVolume* physical_volume,
        const G4LogicalVolume*);

  private:

    void PrintGDMLWarning() const;
    G4VPhysicalVolume* ParseGDMLFile();

    void PrintAuxiliaryInfo() const;
    void ParseAuxiliaryTargetInfo();
    void ParseAuxiliaryVisibilityInfo();
    void ParseAuxiliarySensDetInfo();

    void LoadMagneticField();

    G4int UpdateCopyNo(G4VPhysicalVolume* aVolume, G4int index = 0);

};

#endif//__MOLLERDETECTORCONSTRUCTION_HH
