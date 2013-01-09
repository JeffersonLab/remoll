
#include "remollMagneticField.hh"

// root includes
#include "TMath.h"

remollMagneticField::remollMagneticField() 
	:invertX(false),invertY(false),invertZ(false)
{    
	// needed later for field rotation
	BField_ANSYS = new G4ThreeVector();

	Unit_Length = cm;
	Unit_Angle  = degree;
	Unit_Bfield = tesla;
}

remollMagneticField::~remollMagneticField()
{
	delete BField_ANSYS ;
}

void remollMagneticField::InitializeGrid()
{

	// lets calculate how many data points we have for each variable
	// (get rid of hard coded definition)
	//   nGridPointsInR   =  G4int ( (rMaxFromMap   - rMinFromMap)   /gridstepsize_r   ) + 1;
	//   nGridPointsInZ   =  G4int ( (zMaxFromMap   - zMinFromMap)   /gridstepsize_z   ) + 1;
	//   nGridPointsInPhi =  G4int ( (phiMaxFromMap - phiMinFromMap) /gridstepsize_phi ) + 1;


	nGridPointsInR   =   static_cast<G4int> (( (rMaxFromMap   - rMinFromMap)   /gridstepsize_r   ) + 1.000001);
	nGridPointsInZ   =   static_cast<G4int> (( (zMaxFromMap   - zMinFromMap)   /gridstepsize_z   ) + 1.000001);
	nGridPointsInPhi =   static_cast<G4int> (( (phiMaxFromMap - phiMinFromMap) /gridstepsize_phi ) + 1.000001);

	G4cout << G4endl<<"--------------------------------------------------------------------------------"<< G4endl;
	G4cout << "R: reading " << nGridPointsInR << " points from " << rMinFromMap/cm << " cm to " << rMaxFromMap/cm 
		   << " cm in steps of " << gridstepsize_r/cm << " cm" << G4endl;
	G4cout << "Phi: reading " << nGridPointsInPhi << " points from " << phiMinFromMap/deg << " deg to " << phiMaxFromMap/deg 
		   << " deg in steps of " << gridstepsize_phi/deg << " deg" << G4endl;
	G4cout << "Z: reading " << nGridPointsInZ/m << " points from " << zMinFromMap/m << " m to " << zMaxFromMap/m 
		   << " m in steps of " << gridstepsize_z/m << " m" << G4endl;
	G4cout << "A total of " << nGridPointsInR*nGridPointsInPhi*nGridPointsInZ << " lines." << G4endl;

	BFieldGridData_X.clear();
	BFieldGridData_Y.clear();
	BFieldGridData_Z.clear();


	// Set up storage space for table
	BFieldGridData_X.resize(nGridPointsInZ);
	BFieldGridData_Y.resize(nGridPointsInZ); 
	BFieldGridData_Z.resize(nGridPointsInZ);

	for (int iGridPointInZ = 0; iGridPointInZ < nGridPointsInZ; iGridPointInZ++) 
    {
		BFieldGridData_X[iGridPointInZ].resize(nGridPointsInPhi);
		BFieldGridData_Y[iGridPointInZ].resize(nGridPointsInPhi);
		BFieldGridData_Z[iGridPointInZ].resize(nGridPointsInPhi);  
     
		for (int iGridPointInPhi=0; iGridPointInPhi<nGridPointsInPhi; iGridPointInPhi++) 
		{
			BFieldGridData_X[iGridPointInZ][iGridPointInPhi].resize(nGridPointsInR);
			BFieldGridData_Y[iGridPointInZ][iGridPointInPhi].resize(nGridPointsInR);
			BFieldGridData_Z[iGridPointInZ][iGridPointInPhi].resize(nGridPointsInR);

			for (int iGridPointInR=0; iGridPointInR<nGridPointsInR; iGridPointInR++) 
			{
				// initialize the data matrix
				BFieldGridData_X[iGridPointInZ][iGridPointInPhi][iGridPointInR] = 0.0*Unit_Bfield;
				BFieldGridData_Y[iGridPointInZ][iGridPointInPhi][iGridPointInR] = 0.0*Unit_Bfield;
				BFieldGridData_Z[iGridPointInZ][iGridPointInPhi][iGridPointInR] = 0.0*Unit_Bfield;

			} // end of R
		} // end of Phi
    } // end of Z

}

void remollMagneticField::ReadFieldMap(const char* filename)
{

	G4cout << "--------------------------------------------------------------------------------" << G4endl;
	G4cout << " Magnetic field: Reading the field grid from " << filename          << G4endl; 
	G4cout << "--------------------------------------------------------------------------------" << G4endl;

	G4int r_index=0, phi_index=0, z_index=0;

	G4double raw_R_m,raw_Z_m,raw_Phi_deg;
	G4double val_R=0,val_Z=0,val_Phi=0;
	G4double bx,by,bz;

	G4double rpos=0, phipos=0, zpos=0;
	G4double epsilon = 0.00000001;

	// open the field map file
	G4int nlines = 0;
	std::ifstream inputfile;
	inputfile.open(filename); 

	//G4cout << filename << " opened for reading." << G4endl;
	//G4cout << "Points in R, Phi, Z:"<< nGridPointsInR <<" "<< nGridPointsInPhi <<" "<< nGridPointsInZ <<G4endl;

//	while (1) {
	while (nlines<nGridPointsInR*nGridPointsInPhi*nGridPointsInZ) {
		
// Read in field values and assign units
		inputfile >> raw_R_m >> raw_Phi_deg >> raw_Z_m  >> bx >> by >> bz; 
		val_R   = raw_R_m*m;
		val_Z   = raw_Z_m*m;
		val_Phi = raw_Phi_deg*1.0*degree;

// Calculate r, phi, z as the read-in proceeds to check for errors.
		rpos=rMinFromMap + r_index*gridstepsize_r;
		phipos=phiMinFromMap + phi_index*gridstepsize_phi;
		zpos=zMinFromMap + z_index*gridstepsize_z;
		
// Print out first and last 5 values
		if (nlines==0) {
			G4cout<<G4endl<<" First and last 5 lines in field map "<< filename<<": "<<G4endl<<G4endl;
			G4cout << "  R    z   phi   Br    Bphi    Bz   r_i   z_i   phi_i"<<G4endl;
			G4cout << "(cm)  (m) (deg)  (T)    (T)   (T)   "<<G4endl;
		}
		if ((nlines < 5) || (nlines >= (nGridPointsInR * nGridPointsInPhi * nGridPointsInZ)-5)) 
		{
			printf("%4.1f %4.1f %3.0f %9.6f %9.6f %9.6f %3i %2i %2i\n",
				   val_R/cm, val_Z/m, val_Phi/degree, bx, by, bz, r_index, z_index, phi_index);				   
		}

// Do some checks

        // Checking positions
		if (fabs(val_Z-zpos)>epsilon || fabs(val_Phi-phipos)>epsilon || fabs(val_R-rpos)>epsilon) {
			G4cout << G4endl;
			printf("ERROR in readin coordinate at %2i %2i %2i:", r_index, phi_index, z_index);   
			printf("\n       Got  %f %f %f,  expecting  %f %f %f",val_R/cm, val_Phi/degree, val_Z/m, rpos/cm, phipos/degree, zpos/m);
			printf("\n       with difference %e %e %e greater than %e\n",fabs(val_R-rpos)/cm, fabs(val_Phi-phipos)/degree, fabs(val_Z-zpos)/m,  epsilon);
		}

        // Checking minimum values from the map for z, phi and r
		if (z_index==0) {
			if (val_Z!=zMinFromMap) {
				G4cout << G4endl<< "val_Z!=zMinFromMap at z_index==0  "  <<nlines<<" "<< val_Z/m << "  " << zMinFromMap/m << "\n";
			}
		}
		if (phi_index==0) {
			if (val_Phi!=phiMinFromMap) {
				G4cout << G4endl<< "val_Phi!=phiMinFromMap at phi_index==0  " <<nlines<<" "<< val_Phi/degree << "  " << phiMinFromMap/degree << "\n";
			}
		}
		if (r_index==0) {
			if (val_R!=rMinFromMap) {
				G4cout << G4endl<< "val_R!=rMinFromMap at r_index==0  " <<nlines<<" "<< val_R/cm << "  " << rMinFromMap/cm << "\n";
			}
		}

		// Check whether we're overwriting any grid points - a good signal of errors
		if (BFieldGridData_X[z_index][phi_index][r_index] != 0 ||
			BFieldGridData_Y[z_index][phi_index][r_index] != 0 ||
			BFieldGridData_Z[z_index][phi_index][r_index] != 0) {
			printf("Overwriting bin %i %2i %2i %2i  %f %f %f  %f %f %f\n", nlines, r_index, phi_index, z_index,  
				   BFieldGridData_X[z_index][phi_index][r_index],
				   BFieldGridData_Y[z_index][phi_index][r_index],
				   BFieldGridData_Z[z_index][phi_index][r_index],
				   bx*Unit_Bfield,
				   by*Unit_Bfield,
				   bz*Unit_Bfield);
		}

// Set the grid values to the values which have been read-in
 		BFieldGridData_X[z_index][phi_index][r_index] = bx*Unit_Bfield;
 		BFieldGridData_Y[z_index][phi_index][r_index] = by*Unit_Bfield;
 		BFieldGridData_Z[z_index][phi_index][r_index] = bz*Unit_Bfield;

		if (0) {
			G4cout << G4endl;
			printf("r phi z  Br Bphi  Bz: %2i %2i %2i  %f %f %f\n",r_index, phi_index, z_index, bx, by ,bz);
//				   bx*Unit_Bfield,
//				   by*Unit_Bfield,
//				   bz*Unit_Bfield, tesla);
		}	


		if (!inputfile.good()) break;

		//G4cout<<nlines<<" "<<r_index<<" "<<phi_index<<" "<<z_index<<G4endl;
		nlines++;
		r_index++;

// "restart" indexing for r, phi for the next phi, z point 
		if (r_index==nGridPointsInR) {
			r_index=0;
			phi_index++;
		}
		if (phi_index==nGridPointsInPhi) {
			phi_index=0;
			z_index++;
		}
		if (z_index>nGridPointsInZ) {
			//z_index=nGridPointsInZ-1;
			G4cerr << "ERROR: z_index>nGridPointsInZ\n";
		}  
	}

	inputfile.close();
	
	G4cout << "... done reading " << nlines << " lines." << G4endl<< G4endl;

}

void remollMagneticField::GetFieldValue(const G4double Point[4], G4double *Bfield ) const
{

	G4int debug=0;
	G4bool printout=0;

	// First we have to translate into polar or cylindric coordinates
	// since the field maps are given in cylindric coordinates and the 
    // interpolation will be done in cylindric coordinates as well.

    // Then we need to translate to cartesian components to give them
    // back to the field manager

	G4double xyRadius;
	G4double phi;
	G4double z;

	G4double r_local;
	G4double phi_local;
	G4double z_local;


	G4int r_index;  
	G4int phi_index;
	G4int z_index;  

	// When the track node matches one of the octants then we will rotate 
    // the track node vector (X,Y,Z) to the location of the field map
	// which is defined for phi=-25 to 25 degrees

    // DevelNote: Need to update these examples once the orientation is changed:
	// example1: phi = -5deg   -> 
	// example2: phi = -16deg  ->                         

	phi = TMath::ATan2(Point[1]/mm,Point[0]/mm);
	//G4cout << "phi=" << phi/degree << "\n";
	xyRadius = sqrt(Point[0]/mm*Point[0]/mm + Point[1]/mm*Point[1]/mm)*mm;
	z        = Point[2];

	// Now convert absolute phi into a phi relative to the sector center
	G4int numsectors = 7;
	G4double sectoropeningangle = 2*pi/numsectors;
	G4double sectorcentrephi = pi/numsectors;  //center of septant "0"??? 
//	G4double sectorcentrephi = 3*pi/numsectors;  //center of septant "0"; get funny results for sectorcentrephi=sectoropeningangle
   
	G4double anglefromcentre = phi - sectorcentrephi;
	G4int segmentnumber;
	if (anglefromcentre < 0) segmentnumber = (int)(anglefromcentre/sectoropeningangle-0.5);
	else segmentnumber = (int)(anglefromcentre/sectoropeningangle+0.5);
	G4double deltaphi= segmentnumber*sectoropeningangle;
	G4double newphi = anglefromcentre - deltaphi;
	if (fabs(newphi) > sectoropeningangle/2) G4cout << "\n\nWARNING: newphi too big!\n\n\n";
	
	if (debug>0) {
		G4cout << "X[mm]=" << Point[0]/mm << ", Y[mm]=" << Point[1]/mm << ", Z[mm]=" << Point[2]/mm; // << G4endl;   
		G4cout << ", xyR[mm]=" << xyRadius/mm << ", Phi=" << phi/degree << G4endl; 
		G4cout << "anglefromcentre=" <<  anglefromcentre/degree 
			   <<  " segmentnumber=" <<  segmentnumber 
			   <<  " deltaphi=" <<  deltaphi/degree 
			   <<  " newphi=" << newphi/degree <<  "\n";
	}

//	if (segmentnumber<-2) G4cout<<anglefromcentre/sectoropeningangle-0.5<<" "<<anglefromcentre/deg<<" "<<segmentnumber<<" "<<deltaphi/deg<<" "<<phi/deg<<" "<<newphi/deg<<G4endl;

	phi = newphi;

 
	// Check that the point is within the defined region
	// It's important that it is never equal to the MaxFromMap 
    // because then there's a segmentation violation

	if ( xyRadius/mm >=   rMinFromMap/mm &&     xyRadius/mm <   rMaxFromMap/mm &&
		 phi/degree >= phiMinFromMap/degree &&  phi/degree < phiMaxFromMap/degree && 
		 z/mm >=   zMinFromMap/mm &&            z/mm <   zMaxFromMap/mm ) 
    {

		G4double r_fraction   = ( (xyRadius/mm) -   (rMinFromMap/mm) )   / (  (rMaxFromMap/mm)    -   (rMinFromMap/mm)) ;
		G4double phi_fraction = ((phi/degree)   - (phiMinFromMap/degree))/ ((phiMaxFromMap/degree)-(phiMinFromMap/degree));   
		G4double z_fraction   = ( (z/mm)        -   (zMinFromMap/mm) )   / (  (zMaxFromMap/mm)    -   (zMinFromMap/mm)) ;

        // DevelNote: Is this left over from G0?
		//if (invertX) { xfraction = 1 - xfraction;}
		//if (invertY) { yfraction = 1 - yfraction;}
		//if (invertZ) { zfraction = 1 - zfraction;}

		// Need addresses of these to pass to modf below.
		// modf uses its second argument as an OUTPUT argument.
		G4double r_dindex, phi_dindex, z_dindex;
    
		// Position of the point within the cuboid defined by the
		// nearest surrounding tabulated points

		// double  modf ( double x , double * ipart );
		//
		// Split floating-point value into fractional and integer parts.
		// Breaks x in two parts: the integer (stored in location pointed by ipart) and the fraction (return value). 
		//
		// See http://www.cplusplus.com/ref/cmath/modf.html

		r_local   = ( modf(  r_fraction*(nGridPointsInR   -1),   &r_dindex));
		phi_local = ( modf(phi_fraction*(nGridPointsInPhi -1), &phi_dindex));
		z_local   = ( modf(  z_fraction*(nGridPointsInZ   -1),   &z_dindex));
    
		// The indices of the nearest tabulated point whose coordinates
		// are all less than those of the given point
		// 
		// some comments about using static_cast
		// http://www.research.att.com/~bs/bs_faq2.html#static-cast
		// 

		r_index   = static_cast<G4int>(r_dindex);
		phi_index   = static_cast<G4int>(phi_dindex);
		z_index   = static_cast<G4int>(z_dindex);

		if (0) {
			G4cout << "Fraction r,phi,z: " << r_fraction << " " << phi_fraction << " " << z_fraction << G4endl;
			G4cout << "Local    r,phi,z: " << r_local << " " << phi_local << " " << z_local << G4endl;
			G4cout << "Index    r,phi,z: " << r_index << " " << phi_index << " " << z_index << G4endl;
		}
		if (printout) {
			printf("r,phi,z: %f %f %f   fraction %f %f %f   local %f %f %f   index %2i %2i %2i\n", 
				   xyRadius/m, phi/degree, z/m, r_fraction ,phi_fraction ,z_fraction, r_local ,phi_local ,z_local, r_index ,phi_index ,z_index);
		}

		// Geant4 crashes when phi_index == 45 . I don't understand this ...
	
		// interpolated Bfield given by the ANSYS grid values and coordinate system
		G4double Bx_ANSYS, By_ANSYS, Bz_ANSYS;
	
		// Full 3-dimensional version: trilinear interpolation
		Bx_ANSYS =
			BFieldGridData_X[z_index  ][phi_index  ][r_index  ] * (1-z_local) * (1-phi_local) * (1-r_local) +
			BFieldGridData_X[z_index  ][phi_index  ][r_index+1] * (1-z_local) * (1-phi_local) *    r_local  +
			BFieldGridData_X[z_index  ][phi_index+1][r_index  ] * (1-z_local) *    phi_local  * (1-r_local) +
			BFieldGridData_X[z_index  ][phi_index+1][r_index+1] * (1-z_local) *    phi_local  *    r_local  +
			BFieldGridData_X[z_index+1][phi_index  ][r_index  ] *    z_local  * (1-phi_local) * (1-r_local) +
			BFieldGridData_X[z_index+1][phi_index  ][r_index+1] *    z_local  * (1-phi_local) *    r_local  +
			BFieldGridData_X[z_index+1][phi_index+1][r_index  ] *    z_local  *    phi_local  * (1-r_local) +
			BFieldGridData_X[z_index+1][phi_index+1][r_index+1] *    z_local  *    phi_local  *    r_local ;
	
		By_ANSYS =
			BFieldGridData_Y[z_index  ][phi_index  ][r_index  ] * (1-z_local) * (1-phi_local) * (1-r_local) +
			BFieldGridData_Y[z_index  ][phi_index  ][r_index+1] * (1-z_local) * (1-phi_local) *    r_local  +
			BFieldGridData_Y[z_index  ][phi_index+1][r_index  ] * (1-z_local) *    phi_local  * (1-r_local) +
			BFieldGridData_Y[z_index  ][phi_index+1][r_index+1] * (1-z_local) *    phi_local  *    r_local  +
			BFieldGridData_Y[z_index+1][phi_index  ][r_index  ] *    z_local  * (1-phi_local) * (1-r_local) +
			BFieldGridData_Y[z_index+1][phi_index  ][r_index+1] *    z_local  * (1-phi_local) *    r_local  +
			BFieldGridData_Y[z_index+1][phi_index+1][r_index  ] *    z_local  *    phi_local  * (1-r_local) +
			BFieldGridData_Y[z_index+1][phi_index+1][r_index+1] *    z_local  *    phi_local  *    r_local ;

		Bz_ANSYS =
			BFieldGridData_Z[z_index  ][phi_index  ][r_index  ] * (1-z_local) * (1-phi_local) * (1-r_local) +
			BFieldGridData_Z[z_index  ][phi_index  ][r_index+1] * (1-z_local) * (1-phi_local) *    r_local  +
			BFieldGridData_Z[z_index  ][phi_index+1][r_index  ] * (1-z_local) *    phi_local  * (1-r_local) +
			BFieldGridData_Z[z_index  ][phi_index+1][r_index+1] * (1-z_local) *    phi_local  *    r_local  +
			BFieldGridData_Z[z_index+1][phi_index  ][r_index  ] *    z_local  * (1-phi_local) * (1-r_local) +
			BFieldGridData_Z[z_index+1][phi_index  ][r_index+1] *    z_local  * (1-phi_local) *    r_local  +
			BFieldGridData_Z[z_index+1][phi_index+1][r_index  ] *    z_local  *    phi_local  * (1-r_local) +
			BFieldGridData_Z[z_index+1][phi_index+1][r_index+1] *    z_local  *    phi_local  *    r_local ;
	
		if (printout) {
			printf("weights:  %12.4f %12.4f %12.4f %12.4f %12.4f %12.4f %12.4f %12.4f\n",
				   (1-z_local) * (1-phi_local) * (1-r_local) ,
				   (1-z_local) * (1-phi_local) *    r_local  ,
				   (1-z_local) *    phi_local  * (1-r_local) ,
				   (1-z_local) *    phi_local  *    r_local  ,
				   z_local  * (1-phi_local) * (1-r_local) ,
				   z_local  * (1-phi_local) *    r_local  ,
				   z_local  *    phi_local  * (1-r_local) ,
				   z_local  *    phi_local  *    r_local );

			printf("r:        %12.8f %12.8f %12.8f %12.8f %12.8f %12.8f %12.8f %12.8f\n",
				   BFieldGridData_X[z_index  ][phi_index  ][r_index  ]/tesla,
				   BFieldGridData_X[z_index  ][phi_index  ][r_index+1]/tesla,
				   BFieldGridData_X[z_index  ][phi_index+1][r_index  ]/tesla,
				   BFieldGridData_X[z_index  ][phi_index+1][r_index+1]/tesla,
				   BFieldGridData_X[z_index+1][phi_index  ][r_index  ]/tesla,
				   BFieldGridData_X[z_index+1][phi_index  ][r_index+1]/tesla,
				   BFieldGridData_X[z_index+1][phi_index+1][r_index  ]/tesla,
				   BFieldGridData_X[z_index+1][phi_index+1][r_index+1]/tesla);

			printf("phi:      %12.8f %12.8f %12.8f %12.8f %12.8f %12.8f %12.8f %12.8f\n",
				   BFieldGridData_Y[z_index  ][phi_index  ][r_index  ]/tesla,
				   BFieldGridData_Y[z_index  ][phi_index  ][r_index+1]/tesla,
				   BFieldGridData_Y[z_index  ][phi_index+1][r_index  ]/tesla,
				   BFieldGridData_Y[z_index  ][phi_index+1][r_index+1]/tesla,
				   BFieldGridData_Y[z_index+1][phi_index  ][r_index  ]/tesla,
				   BFieldGridData_Y[z_index+1][phi_index  ][r_index+1]/tesla,
				   BFieldGridData_Y[z_index+1][phi_index+1][r_index  ]/tesla,
				   BFieldGridData_Y[z_index+1][phi_index+1][r_index+1]/tesla);

			printf("z:        %12.8f %12.8f %12.8f %12.8f %12.8f %12.8f %12.8f %12.8f\n",
				   BFieldGridData_Z[z_index  ][phi_index  ][r_index  ]/tesla,
				   BFieldGridData_Z[z_index  ][phi_index  ][r_index+1]/tesla,
				   BFieldGridData_Z[z_index  ][phi_index+1][r_index  ]/tesla,
				   BFieldGridData_Z[z_index  ][phi_index+1][r_index+1]/tesla,
				   BFieldGridData_Z[z_index+1][phi_index  ][r_index  ]/tesla,
				   BFieldGridData_Z[z_index+1][phi_index  ][r_index+1]/tesla,
				   BFieldGridData_Z[z_index+1][phi_index+1][r_index  ]/tesla,
				   BFieldGridData_Z[z_index+1][phi_index+1][r_index+1]/tesla);
		}

		if (0) {
			printf("%2i %2i %2i  %f %f %f   %10.2f %.4f %.4f    %10f %10f %10f   percent diff:  %10f  %10f  %10f\n",
				   r_index, phi_index, z_index, r_local, phi_local, z_local, 
				   xyRadius, phi/degree, z, Bx_ANSYS*10000,By_ANSYS*10000,Bz_ANSYS*10000,
				   100-xyRadius/Bx_ANSYS/100000, 100-(phi/degree)/By_ANSYS/10000, 100-z/Bz_ANSYS/10000000);
		}

		// G4cout << " Bfield_X = " << Bfield[0] << " , "
		//  	 << " Bfield_Y = " << Bfield[2] << " , "
		//  	 << " Bfield_Z = " << Bfield[3] 
		// 	 << G4endl;
	
	
		// We have to rotate the magnetic field derived from the field around the z axis,
		// because the field map is defined in a virtual range of Phi=-25 to 25 degrees (ANSYS coord.)
		// The rotation angle depends on which octant has been hit
	
		BField_ANSYS->setX(Bx_ANSYS);
		BField_ANSYS->setY(By_ANSYS);
		BField_ANSYS->setZ(Bz_ANSYS);
	
		BField_ANSYS->rotateZ(phi);      // this changes coordinates from Br, Bphi to Bx, By
		//BField_ANSYS->rotateZ(deltaphi); // this rotates Bx, By back into the correct sector
		//BField_ANSYS->rotateZ(deltaphi-sectorcentrephi); // this rotates Bx, By back into the correct sector
		//BField_ANSYS->rotateZ(sectorcentrephi-deltaphi); // this rotates Bx, By back into the correct sector
		BField_ANSYS->rotateZ(deltaphi+sectorcentrephi); // this rotates Bx, By back into the correct sector
		Bfield[0] = (BField_ANSYS->x());
		Bfield[1] = (BField_ANSYS->y());
		Bfield[2] = (BField_ANSYS->z());

		if (0) {
			G4cout << "phi=" << phi/degree << "  x=" << Point[0]/cm << " cm y=" << Point[1]/cm << " cm z=" << Point[2]/m << " m     "
				   << " br=" <<  Bx_ANSYS/tesla 
				   << " bphi=" <<  By_ANSYS/tesla 
				   << " bz=" <<  Bz_ANSYS/tesla 
				   << " bx=" <<  BField_ANSYS->x()
				   << " by=" <<  BField_ANSYS->y()
				   << " bx=" <<  Bfield[0]/tesla 
				   << " by=" <<  Bfield[1]/tesla 
				   << "\n";
		}

	} else {  // particle is outside the QTOR field region
		if (0 || printout) {
			G4cout << "NOT in map:    zMinFromMap/mm=" << zMinFromMap/mm << "   ";
			if ((xyRadius/mm <= rMinFromMap/mm)) G4cout << "radius small ";
			if ((xyRadius/mm >= rMaxFromMap/mm)) G4cout << "radius big ";
			if ((phi/degree <= phiMinFromMap/degree)) G4cout << " phi small ";
			if ((phi/degree >= phiMaxFromMap/degree)) G4cout << " phi big ";
			if ((z/mm <= zMinFromMap/mm)) G4cout << " z small ";
			if ((z/mm >= zMaxFromMap/mm)) G4cout << " z big          ";
			G4cout << G4endl;
		}
		Bfield[0] = 0.0;
		Bfield[1] = 0.0;
		Bfield[2] = 0.0;
    
	} // end if ( xyRadius/mm >= ......) 
	if (debug>0) {
		G4cout << "Bfield[0]=" << Bfield[0]*tesla << " Tesla, " 
			   << "Bfield[1]=" << Bfield[1]*tesla << " Tesla, " 
			   << "Bfield[2]=" << Bfield[2]*tesla << " Tesla\n";
	}

} //end of remollMagneticField::GetFieldValue()

void remollMagneticField::PrintFieldCheck(const char* filenamechar)
{

	G4cout << G4endl << "###### Calling remollMagneticField::PrintFieldCheck() " << G4endl << G4endl;

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//DevelNote: ~ Copied this portion from field map read-in temporarily
		// **** This code checks the interpolated fields with that at a symmetric phi point.
//       if (0) {
// 	G4double phi_dindex2;
// 	G4double phi_fraction2 = ((-phi/degree) - (phiMinFromMap/degree))/ ((phiMaxFromMap/degree)-(phiMinFromMap/degree));   
// 	G4double phi_local2 = ( modf((phi_fraction2)*(nGridPointsInPhi -1), &phi_dindex2));
// 	G4int phi_index2 = static_cast<G4int>(phi_dindex2+1);
// 	G4double Bx_ANSYS2 =
// 	  BFieldGridData_X[z_index  ][phi_index2  ][r_index  ] * (1-z_local) * (1-phi_local2) * (1-r_local) +
// 	  BFieldGridData_X[z_index  ][phi_index2  ][r_index+1] * (1-z_local) * (1-phi_local2) *    r_local  +
// 	  BFieldGridData_X[z_index  ][phi_index2+1][r_index  ] * (1-z_local) *    phi_local2  * (1-r_local) +
// 	  BFieldGridData_X[z_index  ][phi_index2+1][r_index+1] * (1-z_local) *    phi_local2  *    r_local  +
// 	  BFieldGridData_X[z_index+1][phi_index2  ][r_index  ] *    z_local  * (1-phi_local2) * (1-r_local) +
// 	  BFieldGridData_X[z_index+1][phi_index2  ][r_index+1] *    z_local  * (1-phi_local2) *    r_local  +
// 	  BFieldGridData_X[z_index+1][phi_index2+1][r_index  ] *    z_local  *    phi_local2  * (1-r_local) +
// 	  BFieldGridData_X[z_index+1][phi_index2+1][r_index+1] *    z_local  *    phi_local2  *    r_local ;
	
// 	G4double By_ANSYS2 =
// 	  BFieldGridData_Y[z_index  ][phi_index2  ][r_index  ] * (1-z_local) * (1-phi_local2) * (1-r_local) +
// 	  BFieldGridData_Y[z_index  ][phi_index2  ][r_index+1] * (1-z_local) * (1-phi_local2) *    r_local  +
// 	  BFieldGridData_Y[z_index  ][phi_index2+1][r_index  ] * (1-z_local) *    phi_local2  * (1-r_local) +
// 	  BFieldGridData_Y[z_index  ][phi_index2+1][r_index+1] * (1-z_local) *    phi_local2  *    r_local  +
// 	  BFieldGridData_Y[z_index+1][phi_index2  ][r_index  ] *    z_local  * (1-phi_local2) * (1-r_local) +
// 	  BFieldGridData_Y[z_index+1][phi_index2  ][r_index+1] *    z_local  * (1-phi_local2) *    r_local  +
// 	  BFieldGridData_Y[z_index+1][phi_index2+1][r_index  ] *    z_local  *    phi_local2  * (1-r_local) +
// 	  BFieldGridData_Y[z_index+1][phi_index2+1][r_index+1] *    z_local  *    phi_local2  *    r_local ;

// 	G4double Bz_ANSYS2 =
// 	  BFieldGridData_Z[z_index  ][phi_index2  ][r_index  ] * (1-z_local) * (1-phi_local2) * (1-r_local) +
// 	  BFieldGridData_Z[z_index  ][phi_index2  ][r_index+1] * (1-z_local) * (1-phi_local2) *    r_local  +
// 	  BFieldGridData_Z[z_index  ][phi_index2+1][r_index  ] * (1-z_local) *    phi_local2  * (1-r_local) +
// 	  BFieldGridData_Z[z_index  ][phi_index2+1][r_index+1] * (1-z_local) *    phi_local2  *    r_local  +
// 	  BFieldGridData_Z[z_index+1][phi_index2  ][r_index  ] *    z_local  * (1-phi_local2) * (1-r_local) +
// 	  BFieldGridData_Z[z_index+1][phi_index2  ][r_index+1] *    z_local  * (1-phi_local2) *    r_local  +
// 	  BFieldGridData_Z[z_index+1][phi_index2+1][r_index  ] *    z_local  *    phi_local2  * (1-r_local) +
// 	  BFieldGridData_Z[z_index+1][phi_index2+1][r_index+1] *    z_local  *    phi_local2  *    r_local ;
//  	G4double Bzdiff = fabs(Bz_ANSYS+Bz_ANSYS2);
//  	if (Bzdiff>0.0000009) {
//  	  printf("%2i %2i %2i %2i %f %f %f   %f  %f  %f %f  Bz    %10f != %10f    %f  %f  %f\n",
//  		 z_index, phi_index, phi_index2, r_index, z, phi, xyRadius, phi_fraction, phi_fraction2, 
// 		 phi_local,phi_local2,Bz_ANSYS,Bz_ANSYS2,Bzdiff,Bx_ANSYS2,By_ANSYS2);
//  	}

// 	G4double Br = BFieldGridData_X[z_index  ][phi_index  ][r_index  ];
// 	G4double Br2 = BFieldGridData_X[z_index  ][phi_index2  ][r_index  ];
// 	if (fabs(Br+Br2)>0.0000009) 
// 	  {
// 	  printf("%2i %2i %2i %f %f %f   %f  %f  %i %i Br    %10f != %10f    %f\n",
// 		 z_index, phi_index, r_index, z, phi, xyRadius, phi_fraction,phi_fraction2,phi_index,phi_index2,Br,Br2,fabs(Br-Br2));
// 	  }
// 	G4double Bphi = BFieldGridData_Y[z_index  ][phi_index  ][r_index  ];
// 	G4double Bphi2 = BFieldGridData_Y[z_index  ][phi_index2  ][r_index  ];
// 	if (fabs(Bphi-Bphi2)>0.0000009) {
// 	  printf("%2i %2i %2i %f %f %f   %f  %f  %i %i Bphi  %10f != %10f    %f\n",
// 		  z_index, phi_index, r_index, z, phi, xyRadius, phi_fraction,phi_fraction2,
// 		 phi_index,phi_index2,Bphi,Bphi2,fabs(Bphi-Bphi2));
// 	}
// 	G4double Bz = BFieldGridData_Z[z_index  ][phi_index  ][r_index  ];
// 	G4double Bz2 = BFieldGridData_Z[z_index  ][phi_index2  ][r_index  ];
// 	G4double Bzdiff = fabs(Bz+Bz2);
// 	if (Bzdiff>0.0000009) {
// 	  printf("%2i %2i %2i %f %f %f   %f  %f  %i %i Bz    %10f != %10f    %f\n",
// 		 z_index, phi_index, r_index, z, phi, xyRadius, phi_fraction,phi_fraction2,phi_index,phi_index2,Bz,Bz2,Bzdiff);
// 	}
//~ End of copied code
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  	FILE *checkFile;
// 	char filenamechar[200];
// 	if (0) {
// 		time_t rawtime;
// 		tm * ptm;
// 		time ( &rawtime );
// 		ptm = gmtime ( &rawtime );
// 		snprintf(filenamechar,200,"magneticfieldgrid_readincheck_%02i%02i%02i.txt",ptm->tm_hour,ptm->tm_min,ptm->tm_sec);
// 	} else {
// 		snprintf(filenamechar,200,"magneticfieldgrid_readincheck.txt");
// 	}
	G4cout << "Printing check of magnet field input to " << filenamechar << G4endl;
	checkFile = fopen (filenamechar,"w");
//	fprintf(checkFile,"test\n");
  	G4double pointin[4], bfieldout[3];
  	pointin[0]=rMinFromMap;
  	pointin[1]=phiMinFromMap;
  	pointin[2]=zMinFromMap;
 	pointin[3]=0;
//  	GetFieldValue(pointin, bfieldout);
// // 	fprintf(checkFile,"   %.3f   %.3f   %.3f %10.5f %10.5f %10.5f \n",
// //  			pointin[0],pointin[1]/degree,pointin[2],bfieldout[0],bfieldout[1]/degree,bfieldout[2]);
//  	printf("   %.3f   %.3f   %.3f %10.5f %10.5f %10.5f \n",
// 		   pointin[0],pointin[1]/degree,pointin[2],bfieldout[0],bfieldout[1]/degree,bfieldout[2]);
	G4double smallfactor=1.00000001;
	G4double epsilon=0.00000001*tesla;
	if (1) {
		// keep zcount < zMaxFromMap since it will return o any way etc. for r and phi
		for (G4double zcount = zMinFromMap; zcount< zMaxFromMap; zcount=zcount+gridstepsize_z) {   
			pointin[2]=smallfactor*zcount;
			for (G4double phicount = phiMinFromMap; phicount< phiMaxFromMap; phicount=phicount+gridstepsize_phi) {
				for (G4double rcount = rMinFromMap; rcount< rMaxFromMap; rcount=rcount+gridstepsize_r) {
					pointin[0]=smallfactor*rcount*cos(phicount);
					pointin[1]=smallfactor*rcount*sin(phicount);
					//pointin[0]=rcount;
					//pointin[1]=phicount;
					GetFieldValue(pointin, bfieldout);
					G4double rfield = bfieldout[0]*cos(phicount) + bfieldout[1]*sin(phicount);
					G4double phifield = - bfieldout[0]*sin(phicount) + bfieldout[1]*cos(phicount);
					//G4double rfield = bfieldout[0];
					//G4double phifield = bfieldout[1];
					if (1) {
						fprintf(checkFile,"%8.3f %8.3f %8.3f %10.5f %10.5f %10.5f \n",
								//pointin[0]/m,pointin[1]/degree,pointin[2]/m,
								rcount/m, phicount/degree, zcount/m,
								rfield/tesla,phifield/tesla,bfieldout[2]/tesla);
					}
					if (fabs(rfield)<epsilon && fabs(phifield)<epsilon && fabs(bfieldout[2])<epsilon) {
						if (zcount > zMinFromMap &&
							phicount > phiMinFromMap &&
							rcount > rMinFromMap) {
							printf("Zero field:  r phi x y z | Br Bphi Bx By Bz | %8.3f %8.3f %8.3f %8.3f %8.3f | %10.5f %10.5f %10.5f %10.5f %10.5f     tesla=%f\n",
								   //pointin[0]/m,pointin[1]/degree,pointin[2]/m,
								   rcount/m, phicount/degree, pointin[0]/m,pointin[1]/m, zcount/m,
								   rfield/tesla,phifield/tesla,bfieldout[0]/tesla,bfieldout[1]/tesla,bfieldout[2]/tesla,tesla);
							//bfieldout[0]/tesla,bfieldout[1]/tesla,bfieldout[2]/tesla);
						}
					}
				}
			}
		}
	}
 	fclose(checkFile);
	G4cout << G4endl << "###### Leaving remollMagneticField::PrintFieldCheck() " << G4endl << G4endl;

}

void remollMagneticField::GetFieldValueFromGridCell( const G4int GridPoint_R,
														const G4int GridPoint_Phi, 
														const G4int GridPoint_Z, 
														G4double *BFieldGridValue ) const
{
	// get the value of the cell ix, iy,iz
	BFieldGridValue[0] = BFieldGridData_X[GridPoint_Z][GridPoint_Phi][GridPoint_R]  ;
	BFieldGridValue[1] = BFieldGridData_Y[GridPoint_Z][GridPoint_Phi][GridPoint_R]  ;
	BFieldGridValue[2] = BFieldGridData_Z[GridPoint_Z][GridPoint_Phi][GridPoint_R]  ;

}

void remollMagneticField::PrintGridCheck(const char* filenamechar)
{

	G4cout<<"###### Calling remollMagneticField::PrintGridCheck() "<<G4endl;
	G4cout<<"###### Calls remollMagneticField::GetFieldValueFromGridCell() "<<G4endl<<G4endl;
	FILE * ReadinDiagnosticFile;
	G4double BFieldGridValue[3];
	ReadinDiagnosticFile = fopen (filenamechar,"w");
	for (int iGridPointInZ = 0; iGridPointInZ < nGridPointsInZ; iGridPointInZ++) {
		for (int iGridPointInPhi=0; iGridPointInPhi<nGridPointsInPhi; iGridPointInPhi++) {
			for (int iGridPointInR=0; iGridPointInR<nGridPointsInR; iGridPointInR++) {
				//printf("%2i %2i %2i  ",
				//	   iGridPointInR, iGridPointInPhi, iGridPointInZ);
				GetFieldValueFromGridCell(iGridPointInR, iGridPointInPhi, iGridPointInZ, BFieldGridValue );
				//printf("%f %f %f\n",
				//		BFieldGridValue[0]/tesla, BFieldGridValue[1]/tesla, BFieldGridValue[2]/tesla);
				fprintf(ReadinDiagnosticFile,"%2i %2i %2i  %f %f %f\n",
						iGridPointInR, iGridPointInPhi, iGridPointInZ, 
						BFieldGridValue[0]/tesla, BFieldGridValue[1]/tesla, BFieldGridValue[2]/tesla);
			}
		}
	}
	fclose (ReadinDiagnosticFile);

	G4cout<<"###### Leaving remollMagneticField::PrintGridCheck() "<<G4endl<<G4endl;
}

