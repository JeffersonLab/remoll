#ifndef __REMOLLMAGNETICFIELD_HH
#define __REMOLLMAGNETICFIELD_HH

#include <vector>

#define __NDIM 3

#include "G4String.hh"
#include "G4MagneticField.hh"

/*!
  \class remollMagneticField
  \brief Individual field map manager
  */

class remollMagneticField : public G4MagneticField {
    /*! 
     * Moller spectrometer magnetic field class
     *
     * Use trilinear interpolation in cylindrical coordinates
     * Might be nice to use some kind of spline someday?
     * Use vectors to store multidimensional arrays
     *
     * Units are meters, degrees, and Tesla
     * Coordinate ordering will be r, phi, z
     * We will only deal with phi interval [-pi,pi]
     *
     * Field maps are of the form
     *
     * #Rpoints		rmin	rmax
     * #Phipoints	phimin	phimax
     * #Zpoints		zmin	zmax
     * # xtants (7 for septant form, 1 for full geometry)
     * r   phi   z    br   bphi   bz
     * ......
     *
     */

    public:
	remollMagneticField( G4String );
	virtual ~remollMagneticField();

	void GetFieldValue(const G4double Point[4], G4double *Bfield) const;

	void InitializeGrid();
	void ReadFieldMap();

	void SetFieldScale(G4double s);
	void SetMagnetCurrent(G4double s);

	void SetZoffset(G4double z){ fZoffset= z; }

	G4String GetName();

	enum Coord_t { kR, kPhi, kZ };

	G4bool IsInit(){ return fInit; }

        G4bool IsInBoundingBox(const G4double* p) const {
          if (p[2] - fZoffset < fMin[kZ] || p[2] - fZoffset > fMax[kZ]) return false;
          if (p[0] < -fMax[kR] || p[0] > fMax[kR]) return false;
          if (p[1] < -fMax[kR] || p[1] > fMax[kR]) return false;
          return true;
        }

    private:
	G4String fFilename;

	G4int fN[__NDIM];
	G4double fMin[__NDIM], fMax[__NDIM];
	G4double fFileMin[__NDIM], fFileMax[__NDIM];

	G4int fNxtant; // Number of *tants (septants, or whatever)
	G4double fPhi0, fPhiLow, fxtantSize;

	// Storage space for the table
	std::vector< std::vector< std::vector< G4double > > > fBFieldData[__NDIM];

	G4double fZoffset, fPhiOffset;
	G4double fZMapOffset, fPhiMapOffset;

	G4double fFieldScale; // Scale overall field by this amount
	G4double fMagCurrent0; // Scale overall field by this amount

	G4bool fInit;
};


#endif//__REMOLLMAGNETICFIELD_HH
