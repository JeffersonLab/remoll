#ifndef __MOLLERDETECTORCONSTRUCTION_HH
#define __MOLLERDETECTORCONSTRUCTION_HH

#include "G4GDMLParser.hh"
#include "G4VUserDetectorConstruction.hh"
#include "remollGlobalField.hh"
#include <vector>

class G4Tubs;
class G4LogicalVolume;
class G4VPhysicalVolume;
class G4VSensitiveDetector;


class remollDetectorConstruction : public G4VUserDetectorConstruction
{
  public:

    remollDetectorConstruction();
    virtual ~remollDetectorConstruction();

  public:

    G4VPhysicalVolume* Construct();

    void CreateGlobalMagneticField();

    void SetDetectorGeomFile(const G4String&);

    remollGlobalField* GetGlobalField(){ return fGlobalField; }

    G4GDMLParser *fGDMLParser;

    //----------------------
    // global magnet section
    //----------------------
    //

    G4FieldManager*         fGlobalFieldManager;
    remollGlobalField*      fGlobalField;
    G4String                fDetFileName;


    G4VPhysicalVolume*      fWorldVolume;

  public:

    void DumpGeometricalTree(G4VPhysicalVolume* aVolume = NULL,
      G4int depth = 0,G4bool surfchk = false);

  private:

    G4int UpdateCopyNo(G4VPhysicalVolume* aVolume, G4int index = 0);

};

#endif//__MOLLERDETECTORCONSTRUCTION_HH
