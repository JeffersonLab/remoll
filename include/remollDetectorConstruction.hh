#ifndef __MOLLERDETECTORCONSTRUCTION_HH
#define __MOLLERDETECTORCONSTRUCTION_HH

#include "G4GDMLParser.hh"
#include "G4VUserDetectorConstruction.hh"
#include "remollMagneticField.hh"
#include <vector>

class G4Tubs;
class G4LogicalVolume;
class G4VPhysicalVolume;
class G4VSensitiveDetector;
class G4Material;
class G4Element;
class G4UniformMagField;
class G4QuadrupoleMagField;
class MollerDetectorMessenger;

class remollDetectorConstruction : public G4VUserDetectorConstruction
{
  public:

    remollDetectorConstruction();
    virtual ~remollDetectorConstruction();

  public:

    G4VPhysicalVolume* Construct();

    void CreateGlobalMagneticField();
    void ReadGlobalMagneticField();

    void ReadCollimatorGeometry(const char* filename);


    void WriteGeometryFile(const G4String& filename);

    void SetDetectorGeomFile(const G4String&);
    void SetReadGeo(G4bool value) {fReadGeoFile  = value;};
    void SetCollimGeomFile(const G4String&);
    void setTargetLength (G4double value) { fTargetLength = value; };

    void setNumColl(G4int value) { NUM_COLS = value; G4cout<<G4endl<<G4endl<<"NUM_COLS (detcons) = "<<NUM_COLS<<G4endl<<G4endl;};

    G4int getNumColl() {return NUM_COLS; };

  protected:

    //void DefineDipoleFields();
    void DefineQuadFields();
    void DefineMaterials();

  private:

    //Logical Volumes
    G4LogicalVolume* mother_logic;
    std::vector<G4LogicalVolume*> washer_logic;
    G4LogicalVolume* target_logic;
    std::vector<G4LogicalVolume*> trapeziod_logic;


    //Physical Volumes
    G4VPhysicalVolume* worldVolume;

    G4VPhysicalVolume* mother_phys;
    std::vector<G4VPhysicalVolume*> washer_phys;
    G4VPhysicalVolume* target_phys;
    std::vector<G4VPhysicalVolume*> trapeziod_phys;

    G4VPhysicalVolume* quartz_phys;

    G4Tubs*             target_solid;   // pointer to the solid Target


    G4VPhysicalVolume* ConstructDetector();

    void DumpGeometricalTree(G4VPhysicalVolume* aVolume,G4int depth=0);
    G4GDMLParser fGDMLParser;
  
    //Materials

    G4Material* Vacuum;
    G4Material* Carbon;
    G4Material* Al;
    G4Material* Copper;
    G4Material* Pb;
    G4Material* Tungsten;
    G4Material* Iron;
    G4Element* H;
    G4Material* LH2;

    //----------------------
    // global magnet section
    //----------------------
    //
    MollerGlobalMagnetField*      pGlobalMagnetField;

    G4FieldManager*         fGlobalFieldManager;
    G4ChordFinder*          fGlobalChordFinder;
    G4Mag_UsualEqRhs*       fGlobalEquation;
    G4MagIntegratorStepper* fGlobalStepper;

    G4double                fMinStep;


    MollerDetectorMessenger* detectorMessenger;
    G4String detfileName;
    G4String collimfileName;
    G4double fTargetLength;           // Full length of the target
    G4bool fReadGeoFile;

    G4int NUM_COLS;

    std::vector<G4double> collInnerRadius;
    std::vector<G4double> collOuterRadius;
    std::vector<G4double> collUpstreamZ;
    std::vector<G4double> collDownstreamZ;
    std::vector<G4String> collMaterial;

};

#endif//__MOLLERDETECTORCONSTRUCTION_HH
