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
class G4Material;
class G4Element;
class G4UniformMagField;
class G4ChordFinder;
class G4QuadrupoleMagField;

class remollDetectorConstruction : public G4VUserDetectorConstruction
{
  public:

    remollDetectorConstruction();
    virtual ~remollDetectorConstruction();

  public:

    G4VPhysicalVolume* Construct();

    void CreateGlobalMagneticField();

    void WriteGeometryFile(const G4String& filename);

    void SetDetectorGeomFile(const G4String&);
    void setTargetLength (G4double value) { fTargetLength = value; };


    void DumpGeometricalTree(G4VPhysicalVolume* aVolume,G4int depth=0);
    G4GDMLParser fGDMLParser;
  

    //----------------------
    // global magnet section
    //----------------------
    //

    G4FieldManager*         fGlobalFieldManager;
    remollGlobalField*      fGlobalField;


};

#endif//__MOLLERDETECTORCONSTRUCTION_HH
