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

    G4GDMLParser fGDMLParser;
  
    remollGlobalField* GetGlobalField(){ return fGlobalField; }

  private:
    //----------------------
    // global magnet section
    //----------------------
    //

    G4FieldManager*         fGlobalFieldManager;
    remollGlobalField*      fGlobalField;
    G4String                fDetFileName;
};

#endif//__MOLLERDETECTORCONSTRUCTION_HH
