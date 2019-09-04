//hyperon.h setup parameters for hyperon-gen.cpp

double vertex[3], xv, yv, zv;	//hyperon vertex, (xv,yv,zv)
double pL[4];			//hyperon lab 4 momentum
double pK[4];			// kaon lab 4 momentum
double pKmag;			// magnitude of kaon lab momentum
double pKtrans;			// kaon transverse momentum
double pKlong;			// kaon longitudinal momentum in gamma+p center of mass frame
double sign;			// sign of Lorentz transformation, sign=+1 Lab->rest frame
double pos[3], xL, yL, zL;	//decay point in lab of hyperon
double sL[4], sL0, sLx, sLy, sLz;	// hyperon polarization 4 vector in lab
double sR[4], sR0 = 0., sRx, sRy, sRz;	//hyperon polariztion 4 vector in rest frame
double k0 = 11.;		// incident electron energy and bremsstrahlung end point
double beta11 = 0.9214, gamma11 = 2.5733;	// beta and gamma for k0=11 GeV: beta11=k0/(k0+mp), gamma11=1/sqrt(1-beta11*beta11)
double mlambda = 1.115683, mproton = 0.938272, mkaon = 0.493677, me = 0.000511, mgamma = 0.;
double tgtLength = 150., rhoLH2 = .07085, X0 = 63.04;	// LH2 target length, density, radiation length
double Dt = 0.1, rho_p = 4.267e22;	// step length in z axis and proton density
double microbarn = 1.e-30;	// units in cm^2
double rastx = 0.5, rasty = 0.5;//raster full widths
double pKmin = 1.;		// minimum kaon lab momentum
double pKmax;			//maximum kaon lab momentum
double pi = 3.14159, twopi = 6.283185;
double s11;			// 2k0*mp+mp*mp for k0=11 GeV
double pcmax11;			// maximum cm energy for k0=11 GeV
double pKtransmax0 = 2.0;	// tentative value
double px, mx;			// missing momentum and mass for given pKmag, pKtrans and photon energy
double Kmin;			// minimum photon energy for given pKmag, pKtrans for lambda production
double Photons[100], PhotonSum;	//photon spectrum between Kmin and k0, sum of Photons
double LT[4][4];		// Lorentz transformation
double dPt = 0.05, dPk = 0.1;	// bin size for transverse and total kaon momentum
//
//ctau needed for range of hyperon from vertex to decay point in Lab


// parameters for   lambda,  sigma+,   sigma-
int hyp = 0;
const
char*  name[] = { "lambda", "sigma+", "sigma-" };
double mhyp[] = { 1.115683,  1.18937, 1.192642 };
double ctau[] = {     7.89,    2.404,     7.89 };
double Frac[] = {      0.5,    0.167,    0.333 };


long int Nvertex;		// number of kaon vertex generations
long int Ngamma = 50;		// number of gamma energies between Kmin and k0
long int Ncnt = 50;		// count off the number of events
