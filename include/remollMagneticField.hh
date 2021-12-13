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

    private:

        enum EInterpolationType {kLinear, kCubic};

    public:

	remollMagneticField(const G4String&);
	virtual ~remollMagneticField() { };

	void AddFieldValue(const G4double point[4], G4double *field) const;
	void GetFieldValue(const G4double point[4], G4double *field) const;

	void SetFieldScale(G4double scale) { fFieldScale = scale; }
	void SetRefCurrent(G4double current) { fRefCurrent = current; }
	void SetCurrent(G4double current) { SetFieldScale(current/fRefCurrent); }

	void SetZoffset(G4double z) { fZoffset = z; }

	const G4String& GetName() const { return fName; }

	enum Coord_t { kR, kPhi, kZ };

        G4bool IsInBoundingBox(const G4double* p) const {
          if (p[2] - fZoffset < fMin[kZ] || p[2] - fZoffset > fMax[kZ]) return false;
          if (p[0] < -fMax[kR] || p[0] > fMax[kR]) return false;
          if (p[1] < -fMax[kR] || p[1] > fMax[kR]) return false;
          return true;
        }

    private:
	G4String fName;
	G4String fFilename;

	size_t fN[__NDIM];
	G4double fUnit[__NDIM], fMin[__NDIM], fMax[__NDIM], fStep[__NDIM];
	G4double fFileMin[__NDIM], fFileMax[__NDIM];

	G4int fNxtant; // Number of *tants (septants, or whatever)
	G4double fPhi0, fPhiLow, fxtantSize;

	// Storage space for the table
	std::vector< std::vector< std::vector< G4double > > > fBFieldData[__NDIM];

	G4double fZoffset, fPhiOffset;
	G4double fZMapOffset, fPhiMapOffset;

	G4double fFieldScale; // Scale overall field by this amount
	G4double fRefCurrent; // Reference current for magnetic field

    private:

        EInterpolationType fInterpolationType;

    public:

        void SetInterpolationType(EInterpolationType type) {
            fInterpolationType = type;
        }
        void SetInterpolationType(const G4String& type) {
            if (type == "linear") SetInterpolationType(kLinear);
            if (type == "cubic") SetInterpolationType(kCubic);
        }
        EInterpolationType GetInterpolationType() const {
            return fInterpolationType;
        }

    private:

        static const char kLinearMap[8][3];
        static const char kCubicMap[64][3];

        double _linearInterpolate(const double p[2 << 0], double x) const {
            return p[0] + x * (p[1] - p[0]);
        }
        double _bilinearInterpolate(const double p[2 << 1], const double x[2]) const {
            double c[2];
            c[0] = _linearInterpolate(&(p[0]), x[0]);
            c[1] = _linearInterpolate(&(p[2]), x[0]);
            return _linearInterpolate(c, x[1]);
        }
        double _trilinearInterpolate(const double p[2 << 2], const double x[3]) const {
            double c[2];
            c[0] = _bilinearInterpolate(&(p[0]), &(x[0]));
            c[1] = _bilinearInterpolate(&(p[4]), &(x[0]));
            return _linearInterpolate(c, x[2]);
        }

        double _cubicInterpolate(const double p[4 << 0], double x) const {
            return p[1] +
                   0.5 * x * (p[2] - p[0] +
                              x * (2. * p[0] - 5. * p[1] + 4. * p[2] - p[3] +
                                   x * (3. * (p[1] - p[2]) + p[3] - p[0])));
        }
        double _bicubicInterpolate(const double p[4 << 1], const double x[2]) const {
            double c[4];
            c[0] = _cubicInterpolate(&(p[0]),  x[1]);
            c[1] = _cubicInterpolate(&(p[4]),  x[1]);
            c[2] = _cubicInterpolate(&(p[8]),  x[1]);
            c[3] = _cubicInterpolate(&(p[12]), x[1]);
            return _cubicInterpolate(c, x[0]);
        }
        double _tricubicInterpolate(const double p[4 << 2], const double x[3]) const {
            double c[4];
            c[0] = _bicubicInterpolate(&(p[0]),  &(x[1]));
            c[1] = _bicubicInterpolate(&(p[16]), &(x[1]));
            c[2] = _bicubicInterpolate(&(p[32]), &(x[1]));
            c[3] = _bicubicInterpolate(&(p[48]), &(x[1]));
            return _cubicInterpolate(c, x[0]);
        }
};


#endif//__REMOLLMAGNETICFIELD_HH
