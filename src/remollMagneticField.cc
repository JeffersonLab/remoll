#include "remollMagneticField.hh"
#include "G4UImanager.hh"

#include "G4PhysicalConstants.hh"
#include "G4SystemOfUnits.hh"
#include "G4ThreeVector.hh"

#include "G4SystemOfUnits.hh"
#include "G4PhysicalConstants.hh"

#include "remollSearchPath.hh"

#include <iostream>
#include <fstream>

#include <assert.h>
#include <math.h>

// Boost headers
#ifdef __USE_BOOST_IOSTREAMS
// This supports gzipped iostreams as magnetic field maps.
// Compile with -D __USE_BOOST_IOSTREAMS to use.
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/filtering_streambuf.hpp>
#include <boost/iostreams/copy.hpp>
#include <boost/iostreams/filter/gzip.hpp>
#include <boost/iostreams/device/file.hpp>
#endif

// Set location of cell vertices
// Note the different endianness
const char remollMagneticField::kLinearMap[8][3] = {
    {0, 0, 0},  // 00
    {1, 0, 0},
    {0, 1, 0},
    {1, 1, 0},
    {0, 0, 1},  // 04
    {1, 0, 1},
    {0, 1, 1},
    {1, 1, 1},
};
const char remollMagneticField::kCubicMap[64][3] = {
    {-1, -1, -1},  // 00
    {-1, -1, 0},
    {-1, -1, 1},
    {-1, -1, 2},
    {-1, 0, -1},  // 04
    {-1, 0, 0},
    {-1, 0, 1},
    {-1, 0, 2},
    {-1, 1, -1},  // 08
    {-1, 1, 0},
    {-1, 1, 1},
    {-1, 1, 2},
    {-1, 2, -1},  // 12
    {-1, 2, 0},
    {-1, 2, 1},
    {-1, 2, 2},
    {0, -1, -1},  // 16
    {0, -1, 0},
    {0, -1, 1},
    {0, -1, 2},
    {0, 0, -1},  // 20
    {0, 0, 0},
    {0, 0, 1},
    {0, 0, 2},
    {0, 1, -1},  // 24
    {0, 1, 0},
    {0, 1, 1},
    {0, 1, 2},
    {0, 2, -1},  // 28
    {0, 2, 0},
    {0, 2, 1},
    {0, 2, 2},
    {1, -1, -1},  // 22
    {1, -1, 0},
    {1, -1, 1},
    {1, -1, 2},
    {1, 0, -1},  // 26
    {1, 0, 0},
    {1, 0, 1},
    {1, 0, 2},
    {1, 1, -1},  // 40
    {1, 1, 0},
    {1, 1, 1},
    {1, 1, 2},
    {1, 2, -1},  // 44
    {1, 2, 0},
    {1, 2, 1},
    {1, 2, 2},
    {2, -1, -1},  // 48
    {2, -1, 0},
    {2, -1, 1},
    {2, -1, 2},
    {2, 0, -1},  // 52
    {2, 0, 0},
    {2, 0, 1},
    {2, 0, 2},
    {2, 1, -1},  // 56
    {2, 1, 0},
    {2, 1, 1},
    {2, 1, 2},
    {2, 2, -1},  // 60
    {2, 2, 0},
    {2, 2, 1},
    {2, 2, 2},
};

remollMagneticField::remollMagneticField(const G4String& filename)
: fName(filename),
  fFilename(filename),
  fN{0,0,0},
  fUnit{m,degree,m},
  fMin{0.0},
  fMax{0.0},
  fStep{0.0},
  fFileMin{0.0},
  fFileMax{0.0},
  fNxtant(0),
  fPhi0(0.0),
  fPhiLow(0.0),
  fxtantSize(0.0),
  fZoffset(0.0),
  fPhiOffset(0.0),
  fFieldScale(1.0),
  fRefCurrent(0.0),
  fInterpolationType(kLinear)
{
    const G4double eps = 1e-6;

    G4cout << "--------------------------------------------------------------------------------" << G4endl;
    G4cout << __PRETTY_FUNCTION__ << ": Reading the field grid from " << fFilename << G4endl;
    G4cout << "--------------------------------------------------------------------------------" << G4endl;

#ifdef __USE_BOOST_IOSTREAMS
    // Create Boost istream
    boost::iostreams::filtering_istream inputfile;
    // If the filename has .gz somewhere (hopefully the end)
    if (fFilename.find(".gz") != std::string::npos) {
      fFilename = remollSearchPath::resolve(fFilename);
      boost::iostreams::file_source source_gz(fFilename);
      if (source_gz.is_open()) {
        // Add gzip decompressor to stream
        inputfile.push(boost::iostreams::gzip_decompressor());
        // Set file as source
        inputfile.push(source_gz);
      } else {
        G4cerr << "Unable to open input file " << fFilename << G4endl;
        exit(1);
      }
    } else {
      // Try to add .gz at end of filename
      fFilename = remollSearchPath::resolve(fFilename + ".gz");
      boost::iostreams::file_source source_gz(fFilename);
      if (source_gz.is_open()) {
        // Add gzip decompressor to stream
        inputfile.push(boost::iostreams::gzip_decompressor());
        // Set file as source
        inputfile.push(source_gz);
      } else {
        // Try to load filename without gz
        boost::iostreams::file_source source_txt(fFilename);
        if (source_txt.is_open()) {
          // Set file as source
          inputfile.push(source_txt);
        } else {
          G4cerr << "Unable to open input file " << fFilename << G4endl;
          exit(1);
        }
      }
    }
#else
    // Create STL ifstream
    std::ifstream inputfile;
    // If the filename has .gz somewhere, fail ungracefully
    if (fFilename.find(".gz") != std::string::npos) {
      G4cerr << "Compressed input files not supported!" << G4endl;
      exit(1);
    }
    // Set file as source
    fFilename = remollSearchPath::resolve(std::string(fFilename));
    inputfile.open(fFilename.data());
#endif

    if (!inputfile.good() ){
	G4cerr << "Error " << __FILE__ << " line " << __LINE__ << G4endl << G4endl << G4endl << G4endl << G4endl 
	    << ": File " << fFilename << " could not open.  Aborting" << G4endl
	    << ": ERROR - Suggested MOLLER Field Map files available at http://hallaweb.jlab.org/12GeV/Moller/downloads/remoll/" << G4endl << G4endl << G4endl << G4endl << G4endl;
	exit(1);
    }

    // Variable that will contain single lines
    std::string inputline;

    // Read in data about grid
    for (size_t cidx = kR; cidx <= kZ; cidx++) {
        getline(inputfile,inputline);
        if (std::istringstream(inputline) >> fN[cidx] >> fMin[cidx] >> fMax[cidx]) {
            fMin[cidx] *= fUnit[cidx];
            fMax[cidx] *= fUnit[cidx];
            fStep[cidx] = (fMax[cidx] - fMin[cidx]) / (fN[cidx] - 1);
            G4cout << "N,min,max,step[" << cidx << "] = " << fN[cidx] << ","
                   << fMin[cidx] << "," << fMax[cidx] << "," << fStep[cidx] << G4endl;
        } else {
	    G4cerr << "Error " << __FILE__ << " line " << __LINE__
		<< ": File " << fFilename << " contains unreadable header.  Aborting" << G4endl;
	    exit(1);
	}
    }

    // Read in grid offsets
    getline(inputfile,inputline);
    if (std::istringstream(inputline) >> fPhiMapOffset >> fZMapOffset) {
        G4cout << "PhiMapOffset,ZMapOffset = " << fPhiMapOffset << ","
            << fZMapOffset << G4endl;
    } else {
	G4cerr << "Error " << __FILE__ << " line " << __LINE__
	    << ": File " << fFilename << " contains unreadable header.  Aborting" << G4endl;
	exit(1);
    }

    fPhiMapOffset *= fUnit[kPhi];
    fZMapOffset   *= fUnit[kZ];

    // Read in extants
    getline(inputfile,inputline);
    if (std::istringstream(inputline) >> fNxtant) {
        G4cout << "Nxtant = " << fNxtant << G4endl;
    } else {
	G4cerr << "Error " << __FILE__ << " line " << __LINE__
	    << ": File " << fFilename << " contains unreadable header.  Aborting" << G4endl;
	exit(1);
    }

    fxtantSize = 2.0*pi/fNxtant;

    //////////////////////////////////////////////////////////////////////
    getline(inputfile,inputline);
    if (std::istringstream(inputline) >> fRefCurrent) {
        G4cout << "RefCurrent = " << fRefCurrent << G4endl;
    } else {
	G4cerr << "Error " << __FILE__ << " line " << __LINE__ 
	    << ": File " << fFilename << " contains unreadable header.  Aborting" << G4endl;
	exit(1);
    }

    // Sanity check on header data

    if( !( fMin[kR] >= 0.0 && fMin[kR] < fMax[kR] &&
	   -180.0*deg <= fMin[kPhi] && fMin[kPhi] <= 180.0*deg &&
	   -180.0*deg <= fMax[kPhi] && fMax[kPhi] <= 180.0*deg &&
	   fMin[kPhi]  < fMax[kPhi] &&
	   fMin[kZ] < fMax[kZ] &&
	   fN[kR] > 0 && fN[kPhi] > 0 && fN[kZ] > 0 &&
	   fNxtant > 0
       	 )
      ){
	G4cerr << "Error " << __FILE__ << " line " << __LINE__ 
	    << ": File " << fFilename << " contains invalid header data.  Aborting" << G4endl;
	G4cerr<<"fMin[kR] >= 0.0: "<<bool(fMin[kR] >= 0.0)<<" "<<fMin[kR]/fUnit[kR]<<G4endl
	      <<"fMin[kR] <= fMax[kR]: "<<bool( fMin[kR] <= fMax[kR])<<" "<<fMax[kR]/fUnit[kR]<<G4endl
	      <<"-180.0 < fMin[kPhi]: "<<bool( -180.0*deg < fMin[kPhi])<<" "<<fMin[kPhi]/fUnit[kPhi]<<G4endl
	      <<"fMin[kPhi] < 180.0: "<<bool(fMin[kPhi] < 180.0*deg)<<G4endl
	      <<"-180.0 <= fMax[kPhi]: "<<bool(-180.0 <= fMax[kPhi])<<" "<<fMax[kPhi]/fUnit[kPhi]<<G4endl
	      <<"fMax[kPhi] < 180.0: "<<bool(fMax[kPhi] < 180.0 )<<G4endl
	      <<"fMin[kPhi]  < fMax[kPhi]: "<<bool(fMin[kPhi]  < fMax[kPhi] )<<G4endl
	      <<"fMin[kZ] < fMax[kZ]: "<<bool(fMin[kZ] < fMax[kZ] )<<" "<<fMin[kZ]/fUnit[kZ]<<" "<<fMax[kZ]/fUnit[kZ]<<G4endl
	      <<"fN[kR] > 0: "<<bool(fN[kR] > 0 )<<" "<<fN[kR]<<G4endl
	      <<"fN[kPhi] > 0: "<<bool(fN[kPhi] > 0 )<<" "<<fN[kPhi]<<G4endl
	      <<"fN[kZ] > 0: "<<bool(fN[kZ] > 0 )<<" "<<fN[kZ]<<G4endl
	      <<"fNxtant > 0: "<<bool(fNxtant > 0 )<<" "<<fNxtant<<G4endl<<G4endl;
	exit(1);
    }

    for (size_t cidx = kR; cidx <= kZ; cidx++) {
	fFileMin[cidx] = fMin[cidx];
	fFileMax[cidx] = fMax[cidx];
    }

    fMin[kPhi] += fPhiMapOffset;
    fMax[kPhi] += fPhiMapOffset;
    // Put between -180 and 180
    fMin[kPhi] = fmodf(fMin[kPhi] + pi/2.0, pi) - pi/2.0;
    fMax[kPhi] = fmodf(fMax[kPhi] + pi/2.0, pi) - pi/2.0;

    double mapphirange = fMax[kPhi] - fMin[kPhi];

    if( mapphirange < 0.0 ){
	mapphirange += 2.0*pi;
    }

    fMin[kZ]   += fZMapOffset;
    fMax[kZ]   += fZMapOffset;

    ///////////////////////////////////

    if( !( fMin[kPhi] >= -pi && fMin[kPhi] <= pi ) ){
	G4cerr << "Error " << __FILE__ << " line " << __LINE__ 
	    << ": File " << fFilename << " header contains invalid phi range.  Aborting" << G4endl;
	exit(1);
    }

    if( fabs( fabs((mapphirange - fxtantSize)/mapphirange)) > 0.03 ){
	G4cerr << "Warning " << __FILE__ << " line " << __LINE__ << G4endl
	    << "File " << fFilename << " header contains too narrow phi range for given xtants." << G4endl
            << "Warning:   Proceeding assuming null field in non-described regions" << G4endl
            << (fMax[kPhi] - fMin[kPhi])/degree << " deg range < " <<  fxtantSize/degree << " deg xtant" << G4endl;
    }

    if( fabs(fabs(mapphirange - fxtantSize) / (mapphirange/fN[kPhi]) - 1) < 0.03 ){
	G4cerr << "Warning " << __FILE__ << " line " << __LINE__ << G4endl
	    << "File " << fFilename << " header contains a gap in the phi range which seems" << G4endl
            << "to correspond perfectly with a slice in phi. This will result in a gap in coverage." << G4endl;
    }


    fPhi0   = (fMax[kPhi] + fMin[kPhi])/2.0;
    if( fMax[kPhi] < fMin[kPhi] ){
	fPhi0 += pi;
    }

    fPhiLow = fPhi0 - fxtantSize/2.0;
    if( fPhiLow < -pi ){
	fPhiLow += 2.0*pi;
    }

    // Dynamically size 3D vectors to what we need to store the file
    for (size_t cidx = kR; cidx <= kZ; cidx++) {
	// Set up storage space for table
	fBFieldData[cidx].clear();
	fBFieldData[cidx].resize(fN[kR],
		std::vector<std::vector<G4double>>(fN[kPhi],
			std::vector<G4double>(fN[kZ], 0.0)));
    }

    // Read in values
    G4int nlines = 0;
    for (size_t zidx = 0; zidx < fN[kZ]; zidx++) {
	for (size_t pidx = 0; pidx < fN[kPhi]; pidx++) {
	    for (size_t ridx = 0; ridx < fN[kR]; ridx++) {

	        getline(inputfile,inputline);

		// Read in field values and assign units
		G4double r, phi, z, bx, by, bz;
		if (std::istringstream(inputline) >> r >> phi >> z >> bx >> by >> bz) {
                  nlines++;
		} else {
		    G4cerr << "Error " << __FILE__ << " line " << __LINE__ 
			<< ": File " << fFilename << " contains invalid data.  Aborting" << G4endl;
		    exit(1);
		}

		// Check that we're reading in properly framed data
		if( fabs(r*fUnit[kR] - (fMin[kR] + ridx * fStep[kR]) ) > eps ){
		    G4cerr << "Error " << __FILE__ << " line " << __LINE__ 
		           << ": File " << fFilename << " contains bad data framing in R.  Aborting" << G4endl;
		    G4cerr << "Index ("<< ridx << ", " << pidx << ", " <<  zidx <<  ")  Expected " << (fMin[kR] + ridx * fStep[kR])/m << " read " << r << G4endl;
		    exit(1);
		}

		if( fabs(phi*fUnit[kPhi] - (fFileMin[kPhi] + pidx * fStep[kPhi]) ) > eps ){
		    G4cerr << "Error " << __FILE__ << " line " << __LINE__ 
		           << ": File " << fFilename << " contains bad data framing in Phi.  Aborting" << G4endl;
		    G4cerr << "Index ("<< ridx << ", " << pidx << ", " <<  zidx <<  ")  Expected " << fFileMin[kPhi]/degree << " read " << phi << G4endl;
		    exit(1);
		}
		if( fabs(z*fUnit[kZ] - (fFileMin[kZ] + zidx * fStep[kZ]) ) > eps ){
		    G4cerr << "Error " << __FILE__ << " line " << __LINE__ 
		           << ": File " << fFilename << " contains bad data framing in Z.  Aborting" << G4endl;
		    G4cerr << "Index ("<< ridx << ", " << pidx << ", " <<  zidx <<  ")  Expected " << fFileMin[kZ]/m << " read " << z << G4endl;
		    exit(1);
		}

		// Set the grid values to the values which have been read-in
		fBFieldData[0][ridx][pidx][zidx] = bx*tesla;
		fBFieldData[1][ridx][pidx][zidx] = by*tesla;
		fBFieldData[2][ridx][pidx][zidx] = bz*tesla;

	    }
	}
    }

    G4cout << "... done reading " << nlines << " lines." << G4endl<< G4endl;
}

void remollMagneticField::GetFieldValue(const G4double point[4], G4double *field ) const
{
    // set to zero
    field[0] = 0.0;
    field[1] = 0.0;
    field[2] = 0.0;
    // add values
    AddFieldValue(point, field);
}

void remollMagneticField::AddFieldValue(const G4double point[4], G4double *field ) const
{
    // Check the bounding box
    if (! IsInBoundingBox(point)) return;

    // First we have to translate into polar or cylindric coordinates
    // since the field maps are given in cylindric coordinates and the
    // interpolation will be done in cylindric coordinates as well.

    // Then we need to translate to cartesian components to give them
    // back to the field manager

    // 1. First calculate r and z
    G4double r = sqrt(point[0]*point[0] + point[1]*point[1]);
    G4double z = point[2] - fZoffset;

    // Check that the point is a valid number
    if( std::isnan(r) || std::isinf(r) ||
	std::isnan(z) || std::isinf(z) ){

	G4cerr << __FILE__ << " line " << __LINE__ << ": ERROR bad conversion to cylindrical coordinates" << G4endl;
	G4cerr << "Point  ( " << point[0]/m << ", " << point[1]/m << ", " << point[2]/m << " ) m" << G4endl;

	exit(1);
    }

    // Check that the point is within the defined region
    if( r >= fMax[kR] || r < fMin[kR] ||
	z >= fMax[kZ] || z < fMin[kZ] ){
	return;
    }

    // 2. Next calculate phi (slower)
    G4double phi = atan2(point[1],point[0]);

    // Check that the point is a valid number
    if( std::isnan(phi) || std::isinf(phi) ){

	G4cerr << __FILE__ << " line " << __LINE__ << ": ERROR bad conversion to cylindrical coordinates" << G4endl;
	G4cerr << "Point  ( " << point[0]/m << ", " << point[1]/m << ", " << point[2]/m << " ) m" << G4endl;

	exit(1);
    }

    // Get xtant number and fraction into xtant
    G4double dxtant;
    G4double dphi =
	phi - fPhiLow >= 0.0 ?
		modf( (  phi           - fPhiLow)/fxtantSize, &dxtant ):
		modf( ( (2.0*pi + phi) - fPhiLow)/fxtantSize, &dxtant ); // Wrap around

    G4int xtant = (G4int) dxtant;

    // Local phi (in file coordinates)
    G4double lphi = dphi*fxtantSize + fPhiLow - fPhiMapOffset;
    if( lphi < -pi ){ lphi += 2.0*pi; }
    if( lphi >  pi ){ lphi -= 2.0*pi; }

    if( !( xtant >= 0 && xtant < fNxtant ) ){

	G4cerr << "Error:  " << __PRETTY_FUNCTION__ << " line " << __LINE__ << ":" << G4endl
               << "  xtant calculation failed. xtant " <<  xtant << " ( " << dxtant << " )  found where "
               << fNxtant << " is specified.  phi = " << phi/deg << " deg" << G4endl;

	exit(1);
    }

    // Check that the point is within the defined region
    // before interpolation.  If it is outside, the field is zero
    if( lphi >= fFileMax[kPhi] || lphi < fFileMin[kPhi] ){
	return;
    }

    // Ensure we're going to get our grid indices correct
    assert( fFileMin[kPhi] <= lphi && lphi < fFileMax[kPhi] );


    // 3. Get interpolation variables

    // the N-1 here is fencepost problem
    G4double x[__NDIM] = {0};
    G4double didx[__NDIM] = {0};
    x[kR]   = modf( ( r - fMin[kR] )*(fN[kR]-1)/( fMax[kR] - fMin[kR] ),            &(didx[kR])   );
    x[kPhi] = modf( ( lphi - fFileMin[kPhi] )*(fN[kPhi]-1)/( fFileMax[kPhi] - fFileMin[kPhi] ), &(didx[kPhi]) );
    x[kZ]   = modf( ( z - fMin[kZ] )*(fN[kZ]-1)/( fMax[kZ] - fMin[kZ] ),            &(didx[kZ])   );

    // Cast these to integers for indexing and check
    size_t idx[__NDIM] = {0};
    for (size_t cidx = 0; cidx < __NDIM; cidx++) {
        idx[cidx] = size_t(didx[cidx]);
    }
    assert( 0 <= idx[kR]   && idx[kR]   < fN[kR] );
    assert( 0 <= idx[kPhi] && idx[kPhi] < fN[kPhi] );
    assert( 0 <= idx[kZ]   && idx[kZ]   < fN[kZ] );

    // Flag edge cases and treat at best as linear
    EInterpolationType type = fInterpolationType;
    if (idx[kR] == 0 || idx[kR] == fN[kR] - 2
     || idx[kZ] == 0 || idx[kZ] == fN[kZ] - 2) {
        type = kLinear;
    }
    G4int phi_wrap = INT_MAX;
    if (fNxtant == 1 && fxtantSize > 1.9*pi) {
        phi_wrap = fN[kPhi] - 1;
    } else if (idx[kPhi] == 0 || idx[kPhi] == fN[kPhi] - 2) {
    }

    // number of cell vertices
    size_t n = 64;
    const char (*map)[3] = kCubicMap;
    switch (type) {
       case kLinear:
           map = kLinearMap;
           n = 8;
           break;
       case kCubic:
           map = kCubicMap;
           n = 64;
           break;
    }

    // values of cell vertices
    thread_local G4double values[__NDIM][64];
    for (size_t i = 0; i < n; i++) {
        for (size_t cidx = 0; cidx < __NDIM; cidx++) {
            values[cidx][i] =
                    fBFieldData[cidx]
                           [idx[kR] + map[i][kR]]
                           [(idx[kPhi] + map[i][kPhi] + fN[kPhi] - 1) % phi_wrap] // wrap around
                           [idx[kZ] + map[i][kZ]];
        }
    }

    // Interpolate
    G4ThreeVector Bcart(0.0,0.0,0.0);
    for(int cidx = 0; cidx < __NDIM; cidx++ ){
        switch (type) {
            case kLinear: {
                Bcart[cidx] = _trilinearInterpolate(values[cidx], x);
                break;
            }
            case kCubic: {
                Bcart[cidx] = _tricubicInterpolate(values[cidx], x);
                break;
            }
        }
    }

    // rotate by local phi, offset, and xtant
    Bcart.rotateZ(lphi + fPhiMapOffset + xtant*fxtantSize);

    // scale field
    Bcart *= fFieldScale;

    // add to original field
    field[0] += Bcart.x();
    field[1] += Bcart.y();
    field[2] += Bcart.z();
}


