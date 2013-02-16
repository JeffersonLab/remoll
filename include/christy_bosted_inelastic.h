#ifndef __CHRISTY_BOSTED_INELASTIC_HH
#define __CHRISTY_BOSTED_INELASTIC_HH

/*  Inelastic cross section parameterizations by Peter Bosted and Eric Christy
 *  Proton:  arxiv 0712.3731v4
 *  Proton/neutron average (fermi smearing removed):  arxiv 0711.0159v4

  call with :

  double sigma_p( double E [GeV], double th [rad], double Ep [GeV] );
  double sigma_n( double E, double th, double Ep );
 
  returns cross sections in nb

 Seamus Riordan
 sriordan@physics.umass.edu
 October 11, 2012
 */

#include "G4PhysicalConstants.hh"

#include <math.h>

double AT_p( int i, double Q2 ){
    if( i < 0 || i >= 7 ){
	fprintf(stderr, "%s: %s line %d - Error bad index\n", __PRETTY_FUNCTION__, __FILE__, __LINE__ );
	exit(1);
    }

    double AT0[7] = {7.780, 6.335, 0.603, 2.330, 1.979, 0.0225, 3.419};
    double a[7] = {4.229, 6823.2, 21.240, -0.288, -0.562, 462.130, 0.0};
    double b[7] = {1.260, 33521.0, 0.056, 0.186, 0.390, 0.192, 0.000};
    double c[7] = {2.124, 2.569, 2.486, 0.064, 0.549, 1.914, 1.0};

    double t0 = AT0[i]/pow(1.0+Q2/0.91,c[i]);
    double t1 = 1.0 + a[i]*Q2/(1.0+b[i]*Q2);

    return t0*t1;
}

double AT_d( int i, double Q2 ){
    if( i < 0 || i >= 7 ){
	fprintf(stderr, "%s: %s line %d - Error bad index\n", __PRETTY_FUNCTION__, __FILE__, __LINE__ );
	exit(1);
    }

    double AT0[7] = {8.122, 6.110, 0.043, 2.088, 0.023, 0.023, 3.319};
    double a[7] = {5.19, -34.64, 191.50, -0.30, -0.46, 541.90, 0.0};
    double b[7] = {3.29, 900.00, 0.22, 0.20, 0.24, 0.22, 0.0};
    double c[7] = {1.870, 1.717, 2.119, 0.001, 1.204, 2.168, 2.0};

    double t0 = AT0[i]/pow(1.0+Q2/0.91,c[i]);
    double t1 = 1.0 + a[i]*Q2/(1.0+b[i]*Q2);

    return t0*t1;
}

double AL_p( int i, double Q2 ){
    if( i < 0 || i >= 7 ){
	fprintf(stderr, "%s: %s line %d - Error bad index\n", __PRETTY_FUNCTION__, __FILE__, __LINE__ );
	exit(1);
    }

    if( i == 1 ) return 0.0;

    double AL0[7] = {29.4140, 0.0, 157.92, 4.2160, 13.7640, 5.5124, 11.0};
    double d[7] = {19.910, 0.0, 97.046, 0.038, 0.314, 0.054, 1.895};
    double e[7] = {0.226, 0.0, 0.310, 1.218, 3.0, 1.309, 0.514};

    double t0 = AL0[i]*Q2/(1.0 + d[i]*Q2);
    double t1 = exp(-e[i]*Q2);

    return t0*t1;
}



double Gamma_p( int i, int j, double W2 ){
    /*
    if( i < 0 || i >= 7 || j < 0 || j >= 3 ){
	fprintf(stderr, "%s: %s line %d - Error bad index\n", __PRETTY_FUNCTION__, __FILE__, __LINE__ );
	exit(1);
    }
    */

    double Mp = proton_mass_c2/GeV;
    double M[7] = {1.230, 1.530, 1.506, 1.698, 1.665, 1.433, 1.934};
    double Gamma[7] = {0.136, 0.220, 0.083, 0.096, 0.109, 0.379, 0.380};
    double l[7] = {1.0, 0.0, 2.0, 3.0, 0.0, 1.0, 3.0};
    double X;
    if( i == 0 ){ X = 0.1446; } else { X = 0.215; }

    double pcm, pcmM, Ecm, EcmM;

    pcm  = 0.0;
    pcmM = 0.0;

    double m;

    if( j == 0 || j == 2 ){
	// single pion or eta
	if( j == 0 ){ m = 0.140; } else { m = 0.538; }

	if( sqrt(W2) < Mp+m ) return 0.0;
	if( M[i] < Mp+m ) return 0.0;

	/*
	pcm  = 0.5*sqrt( ( pow(W2 - Mp*Mp - m*m,2.0) - 4.0*m*m*Mp*Mp )/W2 );
	pcmM = 0.5*sqrt( pow(M[i]*M[i] - Mp*Mp - m*m, 2.0) - 4.0*m*m*Mp*Mp )/M[i];
	*/

	Ecm  = (W2 + m*m - Mp*Mp)/2.0/sqrt(W2);
	EcmM = (M[i]*M[i] + m*m - Mp*Mp)/2.0/M[i];

	pcm  = sqrt(Ecm*Ecm - m*m);
	pcmM = sqrt(EcmM*EcmM - m*m);

	double t0 = Gamma[i];
	double t1 = pow(pcm/pcmM, 2.0*l[i]+1.0);
	double t2 = pow((pcmM*pcmM + X*X)/(pcm*pcm+X*X), l[i]);

	return t0*t1*t2;
    }
    if( j == 1 ){
	// Double pion
	m = 0.140*2.0;

	if( sqrt(W2) < Mp+m ) return 0.0;
	if( M[i] < Mp+m ) return 0.0;

	/*
	pcm  = 0.5*sqrt( ( pow(W2 - Mp*Mp - m*m, 2.0) - 4.0*m*m*Mp*Mp )/W2 );
	pcmM = 0.5*sqrt( pow(M[i]*M[i] - Mp*Mp - m*m, 2.0) - 4.0*m*m*Mp*Mp )/M[i];
	*/

	Ecm  = (W2 + m*m - Mp*Mp)/2.0/sqrt(W2);
	EcmM = (M[i]*M[i] + m*m - Mp*Mp)/2.0/M[i];

	pcm  = sqrt(Ecm*Ecm - m*m);
	pcmM = sqrt(EcmM*EcmM - m*m);

	double t0 = Gamma[i]*sqrt(W2)/M[i];
	double t1 = pow(pcm/pcmM, 2.0*l[i]+4.0);
	double t2 = pow((pcmM*pcmM + X*X)/(pcm*pcm+X*X), l[i]+2.0);

	return t0*t1*t2;
    }
    if( j == 3 ){
	// Photon
	if( sqrt(W2) < Mp ) return 0.0;

	pcm  = 0.5*(W2 - Mp*Mp)/sqrt(W2);
	pcmM = 0.5*(M[i]*M[i] - Mp*Mp)/M[i];

	double t0 = Gamma[i];
	double t1 = pow(pcm/pcmM,2.0);
	double t2 = (pcmM*pcmM + X*X)/(pcm*pcm+X*X);

	return t0*t1*t2;
    }

    return 0.0;
}




double BW_p( int i, double W2 ){
    if( i < 0 || i >= 7 ){
	fprintf(stderr, "%s: %s line %d - Error bad index\n", __PRETTY_FUNCTION__, __FILE__, __LINE__ );
	exit(1);
    }

    int j;
    double Mp = proton_mass_c2/GeV;
    double M[7] = {1.230, 1.530, 1.506, 1.698, 1.665, 1.433, 1.934};
    double Gamma[7] = {0.136, 0.220, 0.083, 0.096, 0.109, 0.379, 0.380};
    double beta[7][3] = {
	{1.0, 0.0, 0.0},
	{0.45, 0.10, 0.45},
	{0.65, 0.35, 0.00},
	{0.65, 0.35, 0.00},
	{0.4, 0.5, 0.1},
	{0.65, 0.35, 0.00},
	{0.5, 0.5, 0.0}
    };

    if( sqrt(W2) < Mp ) return 0.0;

    double Gamma_photon_i = Gamma_p(i,3,W2);

    double K    = 0.5*(W2 - Mp*Mp)/Mp;
    double Kcm  = 0.5*(W2 - Mp*Mp)/sqrt(W2);
    double Ki    = 0.5*(M[i]*M[i]-Mp*Mp)/Mp;
    double Kicm  = 0.5*(M[i]*M[i]-Mp*Mp)/M[i];

    double t0 = Ki*Kicm/(K*Kcm);


    double Gamma_total_i = 0.0;
    for( j = 0; j < 3; j++ ){
	Gamma_total_i += beta[i][j]*Gamma_p(i, j, W2);
    }

    double t1 = Gamma_total_i*Gamma_photon_i/Gamma[i];
    double t2 = pow(W2-M[i]*M[i],2.0) + pow(M[i]*Gamma_total_i,2.0);

    return t0*t1/t2;
}

double BW_d( int i, double W2 ){
    if( i < 0 || i >= 7 ){
	fprintf(stderr, "%s: %s line %d - Error bad index\n", __PRETTY_FUNCTION__, __FILE__, __LINE__ );
	exit(1);
    }

    int j;
    double Mp = proton_mass_c2/GeV;
    double M[7] = {1.230, 1.530, 1.506, 1.698, 1.665, 1.433, 1.934};
    double Gamma[7] = {0.136, 0.220, 0.083, 0.096, 0.109, 0.379, 0.380};
    double beta[7][3] = {
	{1.0, 0.0, 0.0},
	{0.50, 0.00, 0.50},
	{0.65, 0.35, 0.00},
	{0.65, 0.35, 0.00},
	{0.4, 0.6, 0.0},
	{0.65, 0.35, 0.00},
	{0.6, 0.4, 0.0}
    };

    if( sqrt(W2) < Mp ) return 0.0;

    double Gamma_photon_i = Gamma_p(i,3,W2);

    double K    = 0.5*(W2 - Mp*Mp)/Mp;
    double Kcm  = 0.5*(W2 - Mp*Mp)/sqrt(W2);
    double Ki    = 0.5*(M[i]*M[i]-Mp*Mp)/Mp;
    double Kicm  = 0.5*(M[i]*M[i]-Mp*Mp)/M[i];

    double t0 = Ki*Kicm/(K*Kcm);


    double Gamma_total_i = 0.0;
    for( j = 0; j < 3; j++ ){
	Gamma_total_i += beta[i][j]*Gamma_p(i, j, W2);
    }

    double t1 = Gamma_total_i*Gamma_photon_i/Gamma[i];
    double t2 = pow(W2-M[i]*M[i],2.0) + pow(M[i]*Gamma_total_i,2.0);

    return t0*t1/t2;
}



double sigmaR_T_p( double W2, double Q2 ){
    // Resonant cross section

    double W = sqrt(W2);
    double sum = 0.0;

    int i;

    for( i = 0; i < 7; i++ ){
	sum += BW_p(i,W2)*AT_p(i,Q2)*AT_p(i,Q2);
    }

    return W*sum;
}

double sigmaR_L_p( double W2, double Q2 ){
    // Resonant cross section

    double W = sqrt(W2);
    double sum = 0.0;

    int i;

    for( i = 0; i < 7; i++ ){
	sum += BW_p(i,W2)*AL_p(i,Q2)*AL_p(i,Q2);
    }

    return W*sum;
}

double sigmaR_T_d( double W2, double Q2 ){
    // Resonant cross section

    double W = sqrt(W2);
    double sum = 0.0;

    int i;

    for( i = 0; i < 7; i++ ){
	sum += BW_d(i,W2)*AT_d(i,Q2)*AT_d(i,Q2);
    }

    return W*sum;
}

double sigmaNR_T_p( double W2, double Q2 ){
    // Non resonant transverse term
    int i;
    double Mp = proton_mass_c2/GeV;
    double mpi = 0.135;
    double Q02 = 0.05;
    double xp = 1.0/( 1 + (W2 - pow(Mp+mpi,2.0))/(Q2 + Q02) );

    double DW = sqrt(W2) - mpi - Mp;

    if( DW < 0.0 ) return 0.0;

    double sigma0[2] = {246.06, -89.36};
    double aT[2] = {0.067496, 0.20977};
    double bT[2] = {1.3501, 1.5715};
    double cT[2] = {0.12054, 0.090736};
    double dT[2] = {-0.0038495, 0.010362};

    double sum = 0.0;
    double t0, t1;
    for( i = 0; i < 2; i++ ){
	t0 = sigma0[i]*pow(DW,i+1.5);
	t1 = pow(Q2 + aT[i], bT[i]+cT[i]*Q2+dT[i]*Q2*Q2);
	
	sum += xp*t0/t1;
    }

    return sum;
}

double sigmaNR_T_d( double W2, double Q2 ){
    // Non resonant transverse term
    int i;
    double Mp = 0.938;
    double mpi = 0.135;
    double c6 = 0.05;
    double xp = 1.0/( 1 + (W2 - pow(Mp+mpi,2.0))/(Q2 + c6) );

    double DW = sqrt(W2) - mpi - Mp;

    if( DW < 0.0 ) return 0.0;

    double c1[2] = {226.6, -75.3};
    double c2[2] = {0.0764, 0.1776};
    double c3[2] = {1.4570, 1.6360};
    double c4[2] = {0.1318, 0.1350};
    double c5[2] = {-0.005596, 0.005883};

    double sum = 0.0;
    double t0, t1;
    for( i = 0; i < 2; i++ ){
	t0 = c1[i]*pow(DW,i+1.5);
	t1 = pow(Q2 + c2[i], c3[i]+c4[i]*Q2+c5[i]*Q2*Q2);
	
	sum += xp*t0/t1;
    }

    return sum;
}


double sigmaNR_L_p( double W2, double Q2 ){
    // Non resonant transverse term
    double Mp = proton_mass_c2/GeV;
    double m0 = 4.2802;
    double mpi = 0.140;
    double Q02 = 0.125;

    double t = log( log( (Q2+m0)/(0.33*0.33) ) /log(m0/(0.33*0.33)));
    double xp = 1.0/( 1 + (W2 - pow(Mp+mpi,2.0))/(Q2 + Q02) );
    double nu = (W2 + Q2 - Mp*Mp)/2.0/Mp;
    double x  = Q2/(2.0*Mp*nu);

    // Kinematically allowed?
    if( xp > 1.0 || xp < 0.0 ){ return 0.0; }

    double sigma0 = 86.7;
    double aL = 0.0;
    double bL = 4.0294;
    double cL = 3.1285;
    double dL = 0.3340;
    double eL = 4.9623;

    double t0, t1, t2;

    t0 = sigma0*pow(1.0-xp, aL*t+bL)/(1.0-x);
    t1 = pow(Q2, cL)/pow( Q2 + Q02, 1.0+cL);
    t2 = pow(xp, dL + t*eL);

    return t0*t1*t2;
}

double sigma_p( double E, double th, double Ep ){
    double Mp = proton_mass_c2/GeV;
    double alpha = 1.0/137.0;
    double nu = E-Ep;

    double Q2 = 2.0*E*Ep*(1.0-cos(th));
    double W2 = Mp*Mp + 2.0*Mp*nu - Q2;

    if( W2 < Mp*Mp || E < Ep){ 
	G4cerr << __FILE__ << " line " << __LINE__ << ":  WARNING " << __FUNCTION__ << " passed bad kinematics " << G4endl;
	G4cerr << "W2 = " << W2 << " GeV2, E = " << E << " GeV, E' = " << Ep << " GeV" << G4endl;
	return 0.0;
    }

    double sigma_T = sigmaR_T_p(W2, Q2) + sigmaNR_T_p(W2,Q2);
    double sigma_L = sigmaR_L_p(W2, Q2) + sigmaNR_L_p(W2,Q2);


    double eps = 1.0/( 1.0 + 2.0*(1.0 + nu*nu/Q2)*tan(th/2.0)*tan(th/2.0));

    double Gamma = alpha*Ep*(W2-Mp*Mp)/(pow(2.0*3.14159,2.0)*Q2*Mp*E*(1.0-eps));

    // Everything should come out in ub, 1e3 puts to nb/GeV/str

    double retval = Gamma*(sigma_T + eps*sigma_L)*1e3;

    if( std::isinf(retval) || std::isnan(retval) || retval < 0.0 ){
	G4cerr << __FILE__ << " line " << __LINE__ << ":  ERROR " << __FUNCTION__ << " returning bad value" << G4endl;
	G4cerr << Gamma << " " << sigma_T << " " << eps << " " << sigma_L << G4endl;
    }

    return retval;
}

double Rp( double E, double th, double Ep ){
    double Mp = 0.938;
    double nu = E-Ep;

    double Q2 = 2.0*E*Ep*(1.0-cos(th));
    double W2 = Mp*Mp + 2.0*Mp*nu - Q2;

    double sigma_T = sigmaR_T_p(W2, Q2) + sigmaNR_T_p(W2,Q2);
    double sigma_L = sigmaR_L_p(W2, Q2) + sigmaNR_L_p(W2,Q2);

    if( sigma_T > 0.0 ){
	return sigma_L/sigma_T;
    } else {
	return 0.0;
    }
}

double sigma_d( double E, double th, double Ep ){
    double Mp = proton_mass_c2/GeV;
    double alpha = 1.0/137.0;
    double nu = E-Ep;

    double Q2 = 2.0*E*Ep*(1.0-cos(th));
    double W2 = Mp*Mp + 2.0*Mp*nu - Q2;

    if( W2 < Mp*Mp || E < Ep){ 
	G4cerr << __FILE__ << " line " << __LINE__ << ":  WARNING " << __FUNCTION__ << " passed bad kinematics " << G4endl;
	G4cerr << "W2 = " << W2 << " GeV2, E = " << E << " GeV, E' = " << Ep << " GeV" << G4endl;
	return 0.0;
    }


    double sigma_T = sigmaR_T_d(W2, Q2) + sigmaNR_T_d(W2,Q2);
    double sigma_L = Rp(E,th,Ep)*sigma_T; // Use Rp ~ Rn ~ Rd


    double eps = 1.0/( 1.0 + 2.0*(1.0 + nu*nu/Q2)*tan(th/2.0)*tan(th/2.0));

    double Gamma = alpha*Ep*(W2-Mp*Mp)/(pow(2.0*3.14159,2.0)*Q2*Mp*E*(1.0-eps));

    // Everything should come out in ub, 1e3 puts to nb/GeV/str
    // Note this is averaged between the two nucleons

    double retval = Gamma*(sigma_T + eps*sigma_L)*1e3;

    if( std::isinf(retval) || std::isnan(retval) || retval < 0.0 ){
	G4cerr << __FILE__ << " line " << __LINE__ << ":  ERROR " << __FUNCTION__ << " returning bad value" << G4endl;
	G4cerr << Gamma << " " << sigma_T << " " << eps << " " << sigma_L << G4endl;
    }

    return retval;


}

double sigma_n( double E, double th, double Ep ){

    // sigma_d is averaged between n and p so we have a factor 2
    return 2.0*sigma_d(E, th, Ep)-sigma_p(E, th, Ep);
}

#endif//__CHRISTY_BOSTED_INELASTIC_HH
















