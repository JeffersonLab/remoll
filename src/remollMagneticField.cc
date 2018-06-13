#include "remollMagneticField.hh"
#include "G4UImanager.hh"

#include "G4PhysicalConstants.hh"
#include "G4SystemOfUnits.hh"
#include "G4ThreeVector.hh"

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

remollMagneticField::remollMagneticField( G4String filename ){ 

    fFilename = filename;

    /*!  Initialize grid variables
     */

    for( int cidx = kR; cidx < kZ; cidx++ ){
	fN[cidx] = -1;
	fMin[cidx] = -1e9;
	fMax[cidx] = -2e9;
    }

    fPhi0 = -1e9;

    fZoffset = 0.0;
    fInit = false;
    fMagCurrent0 = -1e9;

    fFieldScale = 1.0;

    ReadFieldMap();
}

remollMagneticField::~remollMagneticField(){ 
}

G4String remollMagneticField::GetName(){ 
    if( !fInit ){
	G4cerr << "WARNING " << __FILE__ << " line " << __LINE__ 
	    << ": access uninitialized field." << G4endl;
	return G4String("");
    }

    return fFilename;
}

void remollMagneticField::SetFieldScale(G4double s){ 
    fFieldScale = s;
    G4cout << fFilename << " scale set to " << s << G4endl;
    return;
}

void remollMagneticField::SetMagnetCurrent(G4double s){ 
    if( fMagCurrent0 > 0.0 ){
       	SetFieldScale(s/fMagCurrent0);
    } else {
    	G4cerr << "Warning:  " << __FILE__ << " line " << __LINE__ 
	    << ": Field current not specified in map " << fFilename << " - Ignoring and proceeding " << G4endl;
    }
    return;
}


void remollMagneticField::InitializeGrid() {
    /*!  
     * Dynamically size 3D vectors to what we need to store the file
     * These should already be set from values read in from the fieldmap
     */

    if( fN[kR] <= 0 || fN[kPhi] <= 0 || fN[kZ] <= 0 ){
	G4cerr << "Error " << __FILE__ << " line " << __LINE__ 
	    << ": grid size invalid.  Aborting" << G4endl;
	exit(1);
    }

    G4cout << "Initializing field map grid for " << fFilename << G4endl;
    G4int cidx, ridx, pidx, zidx;

    for( cidx = kR; cidx <= kZ; cidx++ ){
	// Set up storage space for table
	fBFieldData[cidx].clear();
	fBFieldData[cidx].resize(fN[kR]);

	for( ridx = 0; ridx < fN[kR]; ridx++) {
	    fBFieldData[cidx][ridx].resize(fN[kPhi]);

	    for( pidx=0; pidx<fN[kPhi]; pidx++) {
		fBFieldData[cidx][ridx][pidx].resize(fN[kZ]);

		for( zidx=0; zidx<fN[kZ]; zidx++) {
		    fBFieldData[cidx][ridx][pidx][zidx] = 0.0;
		} // end of z
	    } // end of phi
	} // end of r
    } // end coordinate index

    G4cout << "Map grid for " << fFilename << " initialized" << G4endl;

    return;
}

void remollMagneticField::ReadFieldMap(){
    const G4double eps = 1e-6;

    G4cout << "--------------------------------------------------------------------------------" << G4endl;
    G4cout << __PRETTY_FUNCTION__ << ": Reading the field grid from " << fFilename << G4endl; 
    G4cout << "--------------------------------------------------------------------------------" << G4endl;

    G4int ridx = 0, pidx=0, zidx=0;

    G4double raw_R_m,raw_Z_m,raw_Phi_deg;
    G4double val_R=0,val_Z=0,val_Phi=0;
    G4double br,bp,bz;

    // open the field map file
    G4int nlines = 0;
    // int nread;
    G4int cidx;


#ifdef __USE_BOOST_IOSTREAMS
    // Create Boost istream
    boost::iostreams::filtering_istream inputfile;
    // If the filename has .gz somewhere (hopefully the end)
    if (fFilename.find(".gz") != std::string::npos) {
      // Add gzip decompressor to stream
      inputfile.push(boost::iostreams::gzip_decompressor());
    }
    // Set file as source
    inputfile.push(boost::iostreams::file_source(fFilename));
#else
    // Create STL ifstream
    std::ifstream inputfile;
    // If the filename has .gz somewhere, fail ungracefully
    if (fFilename.find(".gz") != std::string::npos) {
      G4cerr << "Compressed input files not supported!" << G4endl;
      exit(1);
    }
    // Set file as source
    inputfile.open(fFilename.data());
#endif

    if (!inputfile.good() ){
	G4cerr << "Error " << __FILE__ << " line " << __LINE__ 
	    << ": File " << fFilename << " could not open.  Aborting" << G4endl;
	exit(1);
    }

    // Variable that will contain single lines
    std::string inputline;

    // Read in data about grid
    for( cidx = kR; cidx <= kZ; cidx++ ){
        getline(inputfile,inputline);
        if (std::istringstream(inputline) >> fN[cidx] >> fMin[cidx] >> fMax[cidx]) {
            G4cout << "N,min,max[" << cidx << "] = " << fN[cidx] << ","
              << fMin[cidx] << "," << fMax[cidx] << G4endl;
        } else {
	    G4cerr << "Error " << __FILE__ << " line " << __LINE__ 
		<< ": File " << fFilename << " contains unreadable header.  Aborting" << G4endl;
	    exit(1);
	}
    }

    getline(inputfile,inputline);
    if (std::istringstream(inputline) >> fPhiMapOffset >> fZMapOffset) {
        G4cout << "PhiMapOffset,ZMapOffset = " << fPhiMapOffset << ","
            << fZMapOffset << G4endl;
    } else {
	G4cerr << "Error " << __FILE__ << " line " << __LINE__
	    << ": File " << fFilename << " contains unreadable header.  Aborting" << G4endl;
	exit(1);
    }

    fPhiMapOffset *= deg;
    fZMapOffset   *= m;

    getline(inputfile,inputline);
    if (std::istringstream(inputline) >> fNxtant) {
        G4cout << "Nxtant = " << fNxtant << G4endl;
    } else {
	G4cerr << "Error " << __FILE__ << " line " << __LINE__ 
	    << ": File " << fFilename << " contains unreadable header.  Aborting" << G4endl;
	exit(1);
    }
    G4cout << __PRETTY_FUNCTION__ << ": N xtants = " << fNxtant << G4endl; 

    fxtantSize = 2.0*pi/fNxtant;

    //////////////////////////////////////////////////////////////////////
    getline(inputfile,inputline);
    if (std::istringstream(inputline) >> fMagCurrent0) {
        G4cout << "MagCurrent0 = " << fMagCurrent0 << G4endl;
    } else {
	G4cerr << "Error " << __FILE__ << " line " << __LINE__ 
	    << ": File " << fFilename << " contains unreadable header.  Aborting" << G4endl;
	exit(1);
    }
    G4cout << __PRETTY_FUNCTION__ << ": field current = " << fMagCurrent0 << " A" << G4endl; 

    // Sanity check on header data

    if( !( fMin[kR] >= 0.0 && fMin[kR] < fMax[kR] &&
	 	-180.0 < fMin[kPhi] && fMin[kPhi] < 180.0 && 
	 	-180.0 < fMax[kPhi] && fMax[kPhi] < 180.0 && 
	 	fMin[kPhi]  < fMax[kPhi] &&
		fMin[kZ] < fMax[kZ] &&
		fN[kR] > 0 && fN[kPhi] > 0 && fN[kZ] > 0 &&
		fNxtant > 0
       	 )
      ){
	G4cerr << "Error " << __FILE__ << " line " << __LINE__ 
	    << ": File " << fFilename << " contains invalid header data.  Aborting" << G4endl;
	exit(1);
    }
    
    // Get in proper units

    fMin[kR] *= m; 
    fMax[kR] *= m;
    fMin[kPhi] *= degree;
    fMax[kPhi] *= degree; 
    fMin[kZ] *= m;
    fMax[kZ] *= m;

    for( cidx = kR; cidx <= kZ; cidx++ ){
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

    if( mapphirange < fxtantSize ){
	G4cerr << "Warning " << __FILE__ << " line " << __LINE__ 
	    << ": File " << fFilename << " header contains too narrow phi range for given xtants." << G4endl <<  "Warning:   Proceeding assuming null field in non-described regions" << G4endl << (fMax[kPhi] - fMin[kPhi])/degree << " deg range < " <<  fxtantSize/degree << " deg xtant" << G4endl;
    }


    fPhi0   = (fMax[kPhi] + fMin[kPhi])/2.0;
    if( fMax[kPhi] < fMin[kPhi] ){
	fPhi0 += pi;
    }

    fPhiLow = fPhi0 - fxtantSize/2.0;
    if( fPhiLow < -pi ){
	fPhiLow += 2.0*pi;
    }

    InitializeGrid();


    for( zidx = 0; zidx < fN[kZ]; zidx++ ){
	for( pidx = 0; pidx < fN[kPhi]; pidx++ ){
	    for( ridx = 0; ridx < fN[kR]; ridx++ ){

	        getline(inputfile,inputline);

		// Read in field values and assign units
		if (std::istringstream(inputline) >> raw_R_m >> raw_Phi_deg >> raw_Z_m >> br >> bp >> bz) {
                  nlines++;
		} else {
		    G4cerr << "Error " << __FILE__ << " line " << __LINE__ 
			<< ": File " << fFilename << " contains invalid data.  Aborting" << G4endl;
		    exit(1);
		}

		////////////////////////////////////////////////////////////////////
		// This checks that we're reading in properly framed data
		// This can probably be condensed
		if( ridx == 0 ){
		    if( fabs(raw_R_m*m - fMin[kR]) > eps ){
			G4cerr << "Error " << __FILE__ << " line " << __LINE__ 
			    << ": File " << fFilename << " contains bad data framing in R.  Aborting" << G4endl;
			G4cerr << "Index ("<< ridx << ", " << pidx << ", " <<  zidx <<  ")  Expected " << fMin[kR]/m << " read " << raw_R_m << G4endl;
			exit(1);
		    }
		}
		if( pidx == 0 ){
		    if( fabs(raw_Phi_deg*deg - fFileMin[kPhi]) > eps ){
			G4cerr << "Error " << __FILE__ << " line " << __LINE__ 
			    << ": File " << fFilename << " contains bad data framing in Phi.  Aborting" << G4endl;
			G4cerr << "Index ("<< ridx << ", " << pidx << ", " <<  zidx <<  ")  Expected " << fFileMin[kPhi]/degree << " read " << raw_Phi_deg << G4endl;
			exit(1);
		    }
		}
		if( zidx == 0 ){
		    if( fabs(raw_Z_m*m - fFileMin[kZ]) > eps ){
			G4cerr << "Error " << __FILE__ << " line " << __LINE__ 
			    << ": File " << fFilename << " contains bad data framing in Z.  Aborting" << G4endl;
			G4cerr << "Index ("<< ridx << ", " << pidx << ", " <<  zidx <<  ")  Expected " << fFileMin[kZ]/m << " read " << raw_Z_m << G4endl;
			exit(1);
		    }
		}
		if( ridx == fN[kR]-1 ){
		    if( fabs(raw_R_m*m - fMax[kR]) > eps ){
			G4cerr << "Error " << __FILE__ << " line " << __LINE__ 
			    << ": File " << fFilename << " contains bad data framing in R.  Aborting" << G4endl;
			G4cerr << "Index ("<< ridx << ", " << pidx << ", " <<  zidx <<  ")  Expected " << fMax[kR]/m << " read " << raw_R_m << G4endl;
			exit(1);
		    }
		}
		if( pidx == fN[kPhi]-1 ){
		    if( fabs(raw_Phi_deg*deg - fFileMax[kPhi]) > eps ){
			G4cerr << "Error " << __FILE__ << " line " << __LINE__ 
			    << ": File " << fFilename << " contains bad data framing in Phi.  Aborting" << G4endl;
			G4cerr << "Index ("<< ridx << ", " << pidx << ", " <<  zidx <<  ")  Expected " << fFileMax[kPhi]/m << " read " << raw_Phi_deg << G4endl;
			exit(1);
		    }
		}
		if( zidx == fN[kZ]-1 ){
		    if( fabs(raw_Z_m*m - fFileMax[kZ]) > eps ){
			G4cerr << "Error " << __FILE__ << " line " << __LINE__ 
			    << ": File " << fFilename << " contains bad data framing in Z.  Aborting" << G4endl;
			G4cerr << "Index ("<< ridx << ", " << pidx << ", " <<  zidx <<  ")  Expected " << fFileMax[kZ]/m << " read " << raw_Z_m << G4endl;
			exit(1);
		    }
		}
		////////////////////////////////////////////////////////////////////

		/* convert to proper units */
		val_R   = raw_R_m*m;
		val_Z   = raw_Z_m*m;
		val_Phi = raw_Phi_deg*degree;

		// Set the grid values to the values which have been read-in
		fBFieldData[kR][ridx][pidx][zidx]   = br*tesla;
		fBFieldData[kPhi][ridx][pidx][zidx] = bp*tesla;
		fBFieldData[kZ][ridx][pidx][zidx]   = bz*tesla;

	    }
	}
    }

    fInit = true;
    G4cout << "... done reading " << nlines << " lines." << G4endl<< G4endl;

}

void remollMagneticField::GetFieldValue(const G4double Point[4], G4double *Bfield ) const {

    // First we have to translate into polar or cylindric coordinates
    // since the field maps are given in cylindric coordinates and the 
    // interpolation will be done in cylindric coordinates as well.

    // Then we need to translate to cartesian components to give them
    // back to the field manager

    G4double r, phi, z, dxtant;
    G4double   x[__NDIM], didx[__NDIM];
    G4int    idx[__NDIM], xtant;
    G4double dphi, lphi; 

    phi = atan2(Point[1],Point[0]);
    r   = sqrt(Point[0]*Point[0] + Point[1]*Point[1]);
    z   = Point[2] - fZoffset;

    if( std::isnan(phi) || std::isinf(phi) ||
	    std::isnan(r) || std::isinf(r) ||
	    std::isnan(z) || std::isinf(z) ){

	G4cerr << __FILE__ << " line " << __LINE__ << ": ERROR bad conversion to cylindrical coordinates" << G4endl;
	G4cerr << "Point  ( " << Point[0]/m << ", " << Point[1]/m << ", " << Point[2]/m << " ) m" << G4endl;

	exit(1);
    }
	

    // Get xtant number and fraction into xtant
    dphi = 
	phi - fPhiLow >= 0.0 ? modf( (phi - fPhiLow)/fxtantSize, &dxtant ) : 
	modf( ( (2.0*pi + phi) - fPhiLow)/fxtantSize, &dxtant ); // Wrap around

    xtant = (G4int) dxtant;

    // Local phi (in file coordinates)
    lphi = dphi*fxtantSize + fPhiLow - fPhiMapOffset;
    if( lphi < -pi ){ lphi += 2.0*pi; }
    if( lphi >  pi ){ lphi -= 2.0*pi; }

    if( !( xtant >= 0 && xtant < fNxtant ) ){
	G4cerr << "Error:  " << __PRETTY_FUNCTION__ << " line " << __LINE__ << ":" << G4endl << "  xtant calculation failed. xtant " <<  xtant << " ( " << dxtant << " )  found where " << fNxtant << " is specified.  phi = " << phi/deg << " deg" << G4endl;
	exit(1);
    }

    // Check that the point is within the defined region
    // before interpolation.  If it is outside, the field is zero

    if( r >= fMax[kR] || r < fMin[kR] ||
	    lphi >= fFileMax[kPhi] || lphi < fFileMin[kPhi] ||
	    z    >= fMax[kZ]   || z    < fMin[kZ] ) {

	Bfield[0] = 0.0;
	Bfield[1] = 0.0;
	Bfield[2] = 0.0;

	return;
    }

    // Ensure we're going to get our grid indices correct
    assert( fMin[kR]   <= r    &&    r < fMax[kR] );
    assert( fFileMin[kPhi] <= lphi && lphi < fFileMax[kPhi] );
    assert( fMin[kZ]   <= z    &&    z < fMax[kZ] );

    int cidx;

    // Get interoplation variables
    // the N-1 here is fencepost problem
    x[kR]   = modf( ( r - fMin[kR] )*(fN[kR]-1)/( fMax[kR] - fMin[kR] ),            &(didx[kR])   );
    x[kPhi] = modf( ( lphi - fFileMin[kPhi] )*(fN[kPhi]-1)/( fFileMax[kPhi] - fFileMin[kPhi] ), &(didx[kPhi]) );
    x[kZ]   = modf( ( z - fMin[kZ] )*(fN[kZ]-1)/( fMax[kZ] - fMin[kZ] ),            &(didx[kZ])   );

    // Cast these to integers for indexing and check
    for( cidx = 0; cidx < __NDIM; cidx++ ){ idx[cidx] = (G4int) didx[cidx]; }

    assert( 0 <= idx[kR]   && idx[kR]   < fN[kR] );
    assert( 0 <= idx[kPhi] && idx[kPhi] < fN[kPhi] );
    assert( 0 <= idx[kZ]   && idx[kZ]   < fN[kZ] );

    // Interpolate
    for( cidx = 0; cidx < __NDIM; cidx++ ){ idx[cidx] = (G4int) didx[cidx]; }

    G4double Bint[__NDIM];
    G4double c00, c10, c01, c11, c0, c1;

    for( cidx = 0; cidx < __NDIM; cidx++ ){ 

	c00 = fBFieldData[cidx][idx[kR]][idx[kPhi]][idx[kZ]]*(1.0-x[kR])
	    + fBFieldData[cidx][idx[kR]+1][idx[kPhi]][idx[kZ]]*x[kR];
	c10 = fBFieldData[cidx][idx[kR]][idx[kPhi]+1][idx[kZ]]*(1.0-x[kR])
	    + fBFieldData[cidx][idx[kR]+1][idx[kPhi]+1][idx[kZ]]*x[kR];
	c01 = fBFieldData[cidx][idx[kR]][idx[kPhi]][idx[kZ]+1]*(1.0-x[kR])
	    + fBFieldData[cidx][idx[kR]+1][idx[kPhi]][idx[kZ]+1]*x[kR];
	c11 = fBFieldData[cidx][idx[kR]][idx[kPhi]+1][idx[kZ]+1]*(1.0-x[kR])
	    + fBFieldData[cidx][idx[kR]+1][idx[kPhi]+1][idx[kZ]+1]*x[kR];

	c0  = c00*(1.0-x[kPhi]) + c10*x[kPhi];
	c1  = c01*(1.0-x[kPhi]) + c11*x[kPhi];

	Bint[cidx] = c0*(1.0-x[kZ])+c1*x[kZ];
    }

    G4ThreeVector Bcart = G4ThreeVector(Bint[kR], Bint[kPhi], Bint[kZ]);
    Bcart.rotateZ(lphi + fPhiMapOffset);      // this changes coordinates from Br, Bphi to Bx, By
    // Now we are cartesian, which is what we need to feed Geant4 (yay)

    Bcart.rotateZ(xtant*fxtantSize);  // rotate into our xtant
    Bfield[0] = Bcart.x()*fFieldScale;
    Bfield[1] = Bcart.y()*fFieldScale;
    Bfield[2] = Bcart.z()*fFieldScale;
} 


