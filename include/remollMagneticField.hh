#ifndef __REMOLLMAGNETICFIELD_HH
#define __REMOLLMAGNETICFIELD_HH

#include <vector>


class remollMagneticField : public G4MagneticField
{
public:

   remollMagneticField();
  
  virtual ~remollMagneticField();

  void GetFieldValue( const   G4double Point[4], G4double *Bfield ) const;  
  void GetFieldValueFromGridCell( const  G4int GridPoint_R, 
				  const  G4int GridPoint_Phi, 
				  const  G4int GridPoint_Z, 
				  G4double *BFieldGridValue ) const;

  void InitializeGrid();
  void ReadFieldMap(const char* filename);
  void PrintFieldCheck(const char* filename);
  void PrintGridCheck(const char* filename);

  void SetFieldMap_RMin      ( G4double Rmin      ) { rMinFromMap    = Rmin;      }
  void SetFieldMap_RMax      ( G4double Rmax      ) { rMaxFromMap    = Rmax;      }
  void SetFieldMap_RStepsize ( G4double Rstepsize ) { gridstepsize_r = Rstepsize; }

  void SetFieldMap_ZMin      ( G4double Zmin      ) { zMinFromMap    = Zmin;      }
  void SetFieldMap_ZMax      ( G4double Zmax      ) { zMaxFromMap    = Zmax;      }
  void SetFieldMap_ZStepsize ( G4double Zstepsize ) { gridstepsize_z = Zstepsize; }

  void SetFieldMap_PhiMin      ( G4double Phimin      ) { phiMinFromMap    = Phimin;      }
  void SetFieldMap_PhiMax      ( G4double Phimax      ) { phiMaxFromMap    = Phimax;      }
  void SetFieldMap_PhiStepsize ( G4double Phistepsize ) { gridstepsize_phi = Phistepsize; }

private:
  G4int nGridPointsInR;
  G4int nGridPointsInPhi;
  G4int nGridPointsInZ;  

  G4double rMinFromMap;
  G4double rMaxFromMap;
   
  G4double phiMinFromMap;
  G4double phiMaxFromMap;
   
  G4double zMinFromMap;
  G4double zMaxFromMap;
   
  G4double gridstepsize_r;
  G4double gridstepsize_phi;
  G4double gridstepsize_z;


  G4double Unit_Length;
  G4double Unit_Angle;
  G4double Unit_Bfield; // units of field map 

// Storage space for the table
  std::vector< std::vector< std::vector< G4double > > > BFieldGridData_X;
  std::vector< std::vector< std::vector< G4double > > > BFieldGridData_Y;
  std::vector< std::vector< std::vector< G4double > > > BFieldGridData_Z;

  G4double fZoffset;
  G4bool   invertX, invertY, invertZ;

  G4ThreeVector* BField_ANSYS;
  
  G4double BFieldScalingFactor;

  //  G4double sectorcentrephi;

};

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

#endif//__REMOLLMAGNETICFIELD_HH
