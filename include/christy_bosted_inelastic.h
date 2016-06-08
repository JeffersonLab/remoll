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

double AT_p( int i, double Q2 ) {
    if( i < 0 || i >= 7 ) {
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

double AT_d( int i, double Q2 ) {
    if( i < 0 || i >= 7 ) {
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

double AL_p( int i, double Q2 ) {
    if( i < 0 || i >= 7 ) {
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

double Gamma_p( int i, int j, double W2 ) {
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
    if( i == 0 ) {
        X = 0.1446;
    }
    else {
        X = 0.215;
    }

    double pcm, pcmM, Ecm, EcmM;

    pcm  = 0.0;
    pcmM = 0.0;

    double m;

    if( j == 0 || j == 2 ) {
        // single pion or eta
        if( j == 0 ) {
            m = 0.140;
        }
        else {
            m = 0.538;
        }

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
    if( j == 1 ) {
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
    if( j == 3 ) {
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

double BW_p( int i, double W2 ) {
    if( i < 0 || i >= 7 ) {
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
    for( j = 0; j < 3; j++ ) {
        Gamma_total_i += beta[i][j]*Gamma_p(i, j, W2);
    }

    double t1 = Gamma_total_i*Gamma_photon_i/Gamma[i];
    double t2 = pow(W2-M[i]*M[i],2.0) + pow(M[i]*Gamma_total_i,2.0);

    return t0*t1/t2;
}

double BW_d( int i, double W2 ) {
    if( i < 0 || i >= 7 ) {
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
    for( j = 0; j < 3; j++ ) {
        Gamma_total_i += beta[i][j]*Gamma_p(i, j, W2);
    }

    double t1 = Gamma_total_i*Gamma_photon_i/Gamma[i];
    double t2 = pow(W2-M[i]*M[i],2.0) + pow(M[i]*Gamma_total_i,2.0);

    return t0*t1/t2;
}

double sigmaR_T_p( double W2, double Q2 ) {
    // Resonant cross section

    double W = sqrt(W2);
    double sum = 0.0;

    int i;

    for( i = 0; i < 7; i++ ) {
        sum += BW_p(i,W2)*AT_p(i,Q2)*AT_p(i,Q2);
    }

    return W*sum;
}

double sigmaR_L_p( double W2, double Q2 ) {
    // Resonant cross section

    double W = sqrt(W2);
    double sum = 0.0;

    int i;

    for( i = 0; i < 7; i++ ) {
        sum += BW_p(i,W2)*AL_p(i,Q2)*AL_p(i,Q2);
    }

    return W*sum;
}

double sigmaR_T_d( double W2, double Q2 ) {
    // Resonant cross section

    double W = sqrt(W2);
    double sum = 0.0;

    int i;

    for( i = 0; i < 7; i++ ) {
        sum += BW_d(i,W2)*AT_d(i,Q2)*AT_d(i,Q2);
    }

    return W*sum;
}

double sigmaNR_T_p( double W2, double Q2 ) {
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
    for( i = 0; i < 2; i++ ) {
        t0 = sigma0[i]*pow(DW,i+1.5);
        t1 = pow(Q2 + aT[i], bT[i]+cT[i]*Q2+dT[i]*Q2*Q2);

        sum += xp*t0/t1;
    }

    return sum;
}

double sigmaNR_T_d( double W2, double Q2 ) {
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
    for( i = 0; i < 2; i++ ) {
        t0 = c1[i]*pow(DW,i+1.5);
        t1 = pow(Q2 + c2[i], c3[i]+c4[i]*Q2+c5[i]*Q2*Q2);

        sum += xp*t0/t1;
    }

    return sum;
}

double sigmaNR_L_p( double W2, double Q2 ) {
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
    if( xp > 1.0 || xp < 0.0 ) {
        return 0.0;
    }

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

double sigma_p( double E, double th, double Ep ) {
    double Mp = proton_mass_c2/GeV;
    double alpha = 1.0/137.0;
    double nu = E-Ep;

    double Q2 = 2.0*E*Ep*(1.0-cos(th));
    double W2 = Mp*Mp + 2.0*Mp*nu - Q2;

    if( W2 < Mp*Mp || E < Ep) {
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

    if( std::isinf(retval) || std::isnan(retval) || retval < 0.0 ) {
        G4cerr << __FILE__ << " line " << __LINE__ << ":  ERROR " << __FUNCTION__ << " returning bad value" << G4endl;
        G4cerr << Gamma << " " << sigma_T << " " << eps << " " << sigma_L << G4endl;
    }

    return retval;
}

double Rp( double E, double th, double Ep ) {
    double Mp = 0.938;
    double nu = E-Ep;

    double Q2 = 2.0*E*Ep*(1.0-cos(th));
    double W2 = Mp*Mp + 2.0*Mp*nu - Q2;

    double sigma_T = sigmaR_T_p(W2, Q2) + sigmaNR_T_p(W2,Q2);
    double sigma_L = sigmaR_L_p(W2, Q2) + sigmaNR_L_p(W2,Q2);

    if( sigma_T > 0.0 ) {
        return sigma_L/sigma_T;
    } else {
        return 0.0;
    }
}

double sigma_d( double E, double th, double Ep ) {
    double Mp = proton_mass_c2/GeV;
    double alpha = 1.0/137.0;
    double nu = E-Ep;

    double Q2 = 2.0*E*Ep*(1.0-cos(th));
    double W2 = Mp*Mp + 2.0*Mp*nu - Q2;

    if( W2 < Mp*Mp || E < Ep) {
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

    if( std::isinf(retval) || std::isnan(retval) || retval < 0.0 ) {
        G4cerr << __FILE__ << " line " << __LINE__ << ":  ERROR " << __FUNCTION__ << " returning bad value" << G4endl;
        G4cerr << Gamma << " " << sigma_T << " " << eps << " " << sigma_L << G4endl;
    }

    return retval;


}

double sigma_n( double E, double th, double Ep ) {

    // sigma_d is averaged between n and p so we have a factor 2
    return 2.0*sigma_d(E, th, Ep)-sigma_p(E, th, Ep);
}

//ooooooooooooooo0000000000000oooooooooooooooo
//Qweak Bosted Code
// the ones that have a 'checked' on them were verified to have the same content as the orginal Bosted Code
// F1F2IN09 -- returns the F1 and F2 structure functions for a nucleus with Z and A numbers
// christy507 -- returns F1, R, sigmaT, sigmaL (last two calculated by resmod507_v2)
// resmodd -- returns F1 for average of free proton and neutron
// resmod507_v2 -- calculates sigma T or L
// MEC2009 -- fit to low q2 dip region (purely empirical)
// fitemc -- take into account the EMC effect
// based on:
//    http://arxiv.org/pdf/1203.2262v2.pdf --> Empirical fit to e-nucleus scattering.
//    http://arxiv.org/pdf/0712.3731v4.pdf --> e-proton cross sections and resonance fit
//    http://arxiv.org/pdf/0711.0159v4.pdf --> e-D and e-n fits
//  data can be found at:
//   http://arxiv.org/pdf/1202.1457v1.pdf -> thesis of V. Mamyan (tables at the back)
//   http://faculty.virginia.edu/qes-archive/QES-data.php --> for QE data
//ooooooooooooooo0000000000000oooooooooooooooo

void christy507(G4double w2,G4double q2,G4double &F1,
                G4double &R, G4double &sigT, G4double &sigL);
G4int resmodd(G4double w2, G4double q2,
	      G4double xval[50], G4double &sig);
G4double resmod507_v2(G4double sf,G4double w2,
                      G4double q2,G4double xval[50]);
G4double MEC2009(G4double q2,G4double w2, G4int A);
G4double fitemc(G4double X, G4int A);

//returns 0 on success
//returns -1 for failure of resmodd
//returns -2 for A < 3
//returns 1 for nan/inf F1/F2
G4int F1F2IN09(G4int Z, G4int IA, G4double qsq,
	       G4double wsq, G4double &F1, G4double &F2)
{
    /*--------------------------------------------------------------------
     Fit to inelastic cross sections for A(e,e')X
     valid for all W<3 GeV and all Q2<10 GeV2

     Inputs: Z, A (real*8) are Z and A of nucleus
             (use Z=0., A=1. to get free neutron)
             Qsq (real*8) is 4-vector momentum transfer squared (positive in
                         chosen metric)
             Wsq (real*8) is invarinat mass squared of final state calculated
                         assuming electron scattered from a free proton

     outputs: F1, F2 (real*8) are structure functions per nucleus
     Version of 10/20/2006 P. Bosted
    --------------------------------------------------------------------*/

    G4double W, x;
    G4double nu,siglp,sigtp,F1pp,F1dp;
    G4double W1,W2,sigt,Rc,sigl,F1d, F1p,qv;
    G4double DW2DPF,Wsqp,pf,kf,Es,dw2des,Fyuse;
    G4double x4, emcfac;
    G4int ism;
    G4double PM = proton_mass_c2/GeV;

    // This is for exp(-xx**2/2.), from teste.f
    G4double XXp[99] = {
        -3.000,-2.939,-2.878,-2.816,-2.755,-2.694,-2.633,-2.571,-2.510,
        -2.449,-2.388,-2.327,-2.265,-2.204,-2.143,-2.082,-2.020,-1.959,
        -1.898,-1.837,-1.776,-1.714,-1.653,-1.592,-1.531,-1.469,-1.408,
        -1.347,-1.286,-1.224,-1.163,-1.102,-1.041,-0.980,-0.918,-0.857,
        -0.796,-0.735,-0.673,-0.612,-0.551,-0.490,-0.429,-0.367,-0.306,
        -0.245,-0.184,-0.122,-0.061, 0.000, 0.061, 0.122, 0.184, 0.245,
        0.306, 0.367, 0.429, 0.490, 0.551, 0.612, 0.673, 0.735, 0.796,
        0.857, 0.918, 0.980, 1.041, 1.102, 1.163, 1.224, 1.286, 1.347,
        1.408, 1.469, 1.531, 1.592, 1.653, 1.714, 1.776, 1.837, 1.898,
        1.959, 2.020, 2.082, 2.143, 2.204, 2.265, 2.327, 2.388, 2.449,
        2.510, 2.571, 2.633, 2.694, 2.755, 2.816, 2.878, 2.939, 3.000
    };
    // these are 100x bigger for convenience
    G4double fyp[99] = {
        0.0272,0.0326,0.0390,0.0464,0.0551,0.0651,0.0766,0.0898,0.1049,
        0.1221,0.1416,0.1636,0.1883,0.2159,0.2466,0.2807,0.3182,0.3595,
        0.4045,0.4535,0.5066,0.5637,0.6249,0.6901,0.7593,0.8324,0.9090,
        0.9890,1.0720,1.1577,1.2454,1.3349,1.4254,1.5163,1.6070,1.6968,
        1.7849,1.8705,1.9529,2.0313,2.1049,2.1731,2.2350,2.2901,2.3379,
        2.3776,2.4090,2.4317,2.4454,2.4500,2.4454,2.4317,2.4090,2.3776,
        2.3379,2.2901,2.2350,2.1731,2.1049,2.0313,1.9529,1.8705,1.7849,
        1.6968,1.6070,1.5163,1.4254,1.3349,1.2454,1.1577,1.0720,0.9890,
        0.9090,0.8324,0.7593,0.6901,0.6249,0.5637,0.5066,0.4535,0.4045,
        0.3595,0.3182,0.2807,0.2466,0.2159,0.1883,0.1636,0.1416,0.1221,
        0.1049,0.0898,0.0766,0.0651,0.0551,0.0464,0.0390,0.0326,0.0272
    };

    // deuteron fit parameters
    G4double xvald0[50] = {
        0.1964E+01, 0.1086E+01, 0.5313E-02, 0.1265E+01, 0.8000E+01,
        0.2979E+00, 0.1354E+00, 0.2200E+00, 0.8296E-01, 0.9578E-01,
        0.1094E+00, 0.3794E+00, 0.8122E+01, 0.5189E+01, 0.3290E+01,
        0.1870E+01, 0.6110E+01,-0.3464E+02, 0.9000E+03, 0.1717E+01,
        0.4335E-01, 0.1915E+03, 0.2232E+00, 0.2119E+01, 0.2088E+01,
        -0.3029E+00, 0.2012E+00, 0.1104E-02, 0.2276E-01,-0.4562E+00,
        0.2397E+00, 0.1204E+01, 0.2321E-01, 0.5419E+03, 0.2247E+00,
        0.2168E+01, 0.2266E+03, 0.7649E-01, 0.1457E+01, 0.1318E+00,
        -0.7534E+02, 0.1776E+00, 0.1636E+01, 0.1350E+00,-0.5596E-02,
        0.5883E-02, 0.1934E+01, 0.3800E+00, 0.3319E+01, 0.1446E+00
    };

    G4double P[24] = {
        5.1377e-03,   9.8071e-01,   4.6379e-02,   1.6433e+00,
        6.9826e+00,  -2.2655e-01,   1.1095e-01,   2.7945e-02,
        4.0643e-01,   1.6076e+00,  -7.5460e+00,   4.4418e+00,
        -3.7464e-01,   1.0414e-01,  -2.6852e-01,   9.6653e-01,
        -1.9055e+00,   9.8965e-01,   2.0613e+02,  -4.5536e-02,
        2.4902e-01,  -1.3728e-01,   2.9201e+01,   4.9280e-03
    };

    G4double F1M;
    nu = (wsq - PM*PM + qsq) / 2. / PM;
    qv = sqrt(nu*nu + qsq);

    if (wsq <= 0.0) {
      W = 0.0;
      x = 0.0;
    }else{
      W  = sqrt(wsq);
      x  = qsq / (2.0 * PM * nu);
    }

    if (IA <= 2) return -2;

    // For nuclei
    sigt = 0.;
    sigl = 0.;
    F1d = 0.;
    F1p = 0.;

    // Modifed to use Superscaling from Sick, Donnelly, Maieron,
    // nucl-th/0109032
    if(IA==2) {
        kf=0.085;
        Es=0.0022;
    }
    else if(IA==3) {
        kf=0.115;
        Es=0.001;
    }
    else if(IA>3 && IA<=7) {
        kf=0.19;
        Es=0.017;
    }
    else if(IA>7 && IA<=16) {
        kf=0.228;
        Es=0.0165;
    }
    else if(IA>16 && IA<=25) {
        kf=0.230;
        Es=0.025;
    }
    else if(IA>25 && IA<=38) {
        kf=0.236;
        Es=0.018;
    }
    else if(IA>38 && IA<=55) {
        kf=0.241;
        Es=0.028;
    }
    else if(IA>55 && IA<=60) {
        kf=0.241;
        Es=0.018;
    }
    else //at this point IA should be larger than 60
    {   kf=0.245;
        Es=0.028;
    }
    
    // adjust pf to give right width based on kf
    pf = 0.5 * kf;
    // assume this is 2 * pf * qv
    DW2DPF = 2. * qv;
    dw2des = 2. * (nu + PM) ;
    // switched to using 99 bins!
    for (ism = 0; ism < 99; ism++) {
        Fyuse = fyp[ism]/100.;
        Wsqp = wsq + XXp[ism] * pf * DW2DPF - Es * dw2des;
        if (Wsqp > 1.159) {
            christy507(Wsqp,qsq,F1pp,Rc,sigtp,siglp);
            int bad=resmodd(Wsqp,qsq,xvald0,F1dp);
	    if(bad) return -1;
            F1d = F1d + F1dp * Fyuse;
            F1p = F1p + F1pp * Fyuse;
            sigt = sigt + sigtp * Fyuse;
            sigl = sigl + siglp * Fyuse;
        }
    }

    Rc = 0.;
    if(sigt > 0.) Rc = sigl / sigt;
    W1 = (2. * Z * F1d + (IA - 2. * Z) * (2. * F1d - F1p)) / PM;
    W1= W1*(1.0+P[13]*x+P[14]*pow(x,2)+P[15]*pow(x,3)+P[16]*pow(x,4)+P[17]*pow(x,5));
    if(W > 0.0)  W1=W1*pow((1.0+(P[20]*W+P[21]*pow(W,2))/(1.0+P[22]*qsq)),2);
    F1M = MEC2009(qsq,wsq,IA);
    W1 = W1 + F1M;
    if(wsq > 0.0 ) Rc = Rc * ( 1.0 + P[6] + P[23]*IA );
    W2 = W1 * (1. + Rc) / (1. + nu*nu / qsq);
    x4 = qsq / 2. / PM / nu;
    emcfac = fitemc(x4, IA);
    F1 = PM * W1 * emcfac;
    F2 = nu * W2 * emcfac;
    if(std::isnan(F1) || std::isinf(F1) || std::isnan(F2) || std::isinf(F2)){
      G4cerr << "Error! nan/inf "<<__FILE__ << " line " << __LINE__ <<" "<<G4endl;
      G4cerr <<" "<< F1 <<" "<<F2<<" "<< W1 <<" "<<W2<<" "<<F1M<<" "<<PM<<" "<<nu<<" "<<emcfac<<G4endl;
      return 1;
    }
    return 0;
}

//-----------------------------------------------------------------//
//always calculates values
void christy507(G4double w2,G4double q2,G4double &F1,
                G4double &R, G4double &sigT, G4double &sigL)//checked
{
//   M.E. Christy and P.E. Bosted, ``Empirical Fit to Precision
//    Inclusive Electron-Proton Cross Sections in the Resonance Region'',
//    (arXiv:0712.3731). To be submitted to Phys. Rev. C.

    G4double xval1[50],xvalL[50];

    G4double mp = proton_mass_c2/GeV;
    G4double mp2 = mp*mp;
    G4double pi = 3.141593;
    G4double alpha = 1./137.036;

    G4double xval[100] = {
        0.12298E+01,0.15304E+01,0.15057E+01,0.16980E+01,0.16650E+01,
        0.14333E+01,0.13573E+00,0.22000E+00,0.82956E-01,0.95782E-01,
        0.10936E+00,0.37944E+00,0.77805E+01,0.42291E+01,0.12598E+01,
        0.21242E+01,0.63351E+01,0.68232E+04,0.33521E+05,0.25686E+01,
        0.60347E+00,0.21240E+02,0.55746E-01,0.24886E+01,0.23305E+01,
        -.28789E+00,0.18607E+00,0.63534E-01,0.19790E+01,-.56175E+00,
        0.38964E+00,0.54883E+00,0.22506E-01,0.46213E+03,0.19221E+00,
        0.19141E+01,0.24606E+03,0.67469E-01,0.13501E+01,0.12054E+00,
        -.89360E+02,0.20977E+00,0.15715E+01,0.90736E-01,-.38495E-02,
        0.10362E-01,0.19341E+01,0.38000E+00,0.34187E+01,0.14462E+00,
        0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,
        0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,
        0.00000E+00,0.00000E+00,0.29414E+02,0.19910E+02,0.22587E+00,
        0.00000E+00,0.00000E+00,0.38565E+04,0.65717E+00,0.00000E+00,
        0.15792E+03,0.97046E+02,0.31042E+00,0.00000E+00,0.42160E+01,
        0.38200E-01,0.12182E+01,0.00000E+00,0.13764E+02,0.31393E+00,
        0.29997E+01,0.00000E+00,0.55124E+01,0.53743E-01,0.13091E+01,
        0.00000E+00,0.86746E+02,0.40864E-04,0.40294E+01,0.31285E+01,
        0.33403E+00,0.49623E+01,0.00000E+00,0.00000E+00,0.11000E+02,
        0.18951E+01,0.51376E+00,0.00000E+00,0.42802E+01,0.00000E+00
    };

    for (G4int i = 0; i<50; i++) {
        xval1[i] = xval[i];
        xvalL[i] = xval[50+i] ;
        if(i < 12) xvalL[i] = xval1[i];
    }
    xvalL[42] = xval1[46];
    xvalL[43] = xval1[47];
    xvalL[49] = xval1[49];

    sigT = resmod507_v2(1,w2,q2,xval1);
    sigL = resmod507_v2(2,w2,q2,xvalL);

    F1 = sigT*(w2-mp2)/8./pi/pi/alpha/0.3894e3;
//     G4double xb = q2/(w2+q2-mp2);
    //G4double FL = sigL*2.*xb*(w2-mp2)/8./pi/pi/alpha/0.3894e3;//seems to be not needed for anything
    R = sigL/sigT;

    return;
}

// -------------------------------------------------------------------------//
//on q2 or w2 out of range it returns -1
G4int resmodd(G4double w2, G4double q2,
	      G4double xval[50], G4double &sig) 
{
    //! returns F1 for average of free proton and neutron
    //! for given W2, Q2

    G4double W,mp,mp2,mass[7],width[7];
    G4double height[7],rescoef[6][4];
    G4double nr_coef[3][4],wdif,sig_nr;
    G4double sigr[7];
    G4double mpi,meta,intwidth[7],k,kcm,kr[7],kcmr[7],ppicm,ppi2cm;
    G4double petacm,ppicmr[7],ppi2cmr[7],petacmr[7],epicmr[7],epi2cmr[7];
    G4double eetacmr[7],epicm,epi2cm,eetacm,br[7][2],ang[7],sigrsv[7];
    G4double pgam[7],pwid[7][2],x0[7],dip;
    G4double sig_res,xpr,alpha,pi,F1;
    G4int i,j,num,iw;

    G4double xval0[12] = {
        0.12298E+01,0.15304E+01,0.15057E+01,0.16980E+01,0.16650E+01,
        0.14333E+01,0.13573E+00,0.22000E+00,0.82956E-01,0.95782E-01,
        0.10936E+00,0.37944E+00
    };

    G4double w2sv;

    mp = proton_mass_c2/GeV;
    mpi = 0.135;
    meta = 0.547;
    mp2 = mp*mp;
    pi = 3.141593;
    alpha = 1./137.036;

    sig = 0.;

    if(w2 < 1.07327*1.07327 || w2 > 25 || q2 < 0.0 || q2 > 11.0) {
	G4cerr << "ERROR: " << __FILE__ << " line " << __LINE__ << G4endl;	
	G4cerr << " W2/Q2 check failed: should be 1.07327^2<W2<25 and 0 < Q2 < 11" << G4endl;
	G4cerr << "   but are w2 q2: " << w2 << " " << q2 << G4endl;
	return -1;
    }
    
    // ! branching ratios
    br[0][0] = 1.0;
    br[1][0] = 0.5;
    br[2][0] = 0.65;
    br[3][0] = 0.65;
    br[4][0] = 0.4;
    br[5][0] = 0.65;
    br[6][0] = 0.6;

    // ! angular momenta
    ang[0] = 1.;      // !!!  P33(1232)
    ang[1] = 0.;      // !!!  S11(1535)
    ang[2] = 2.;      // !!!  D13(1520)
    ang[3] = 3.;      // !!!  F15(1680)
    ang[4] = 0.;      // !!!  S15(1650)
    ang[5] = 1.;      // !!!  P11(1440) roper
    ang[6] = 3.;      // !!!  ? 4th resonance region

    // ! x0 parameter
    for (i = 0; i < 7; i++) x0[i] = 0.215;
    x0[0] = 0.1446;

    // ! out branching ratio
    for (i = 0; i < 7; i++) br[i][1] = 1.-br[i][0];

    // ! remember w2
    w2sv = w2;

    // ! uses xvals of 1-12, 47, and 48
    // ! move masses, wdiths into local variables
    // ! pyb changed to be fixed
    num = 0;
    for (i = 0; i < 6; i++) {
        num = num + 1;
        mass[i] = xval0[i];
    }
    for (i = 0; i < 6; i++) {
        num = num + 1;
        intwidth[i] = xval0[num-1];
    }
    
    //! changed to allow delta width, mass to vary
    //! taken out again since xval[1] used in MEC
    mass[6] = 1.9341;
    intwidth[6] = 0.380;

    iw = G4int(1000.*sqrt(w2));
    W = 0.001 * (iw+0.5);        
    w2 = W*W;
    wdif = W - (mp + mpi);
        
    // ! Calculate kinematics needed for threshold Relativistic B-W        
    k = (w2 - mp2) / 2. / mp;            
    kcm = (w2 - mp2) / 2. / W;    
    epicm = (w2 + mpi*mpi -mp2 ) / 2. / W;    
    ppicm = pow(std::max(0.0,(epicm*epicm - mpi*mpi)),0.5);    
    epi2cm = (w2 + pow((2.*mpi),2) -mp2 ) / 2. / W;    
    ppi2cm = pow(std::max(0.0,(epi2cm*epi2cm - pow((2.*mpi),2))),0.5);    
    eetacm = (w2 + meta*meta -mp2 ) / 2. / W;   
    petacm =  pow(std::max(0.0,(eetacm*eetacm - meta*meta)),0.5);            
    for (i = 0; i < 7; i++) {    
      kr[i] = (mass[i]*mass[i]-mp2)/2./mp;      
      kcmr[i] = (mass[i]*mass[i]-mp2)/2./mass[i];      
      epicmr[i] = (mass[i]*mass[i] + mpi*mpi -mp2 )/2./mass[i];      
      ppicmr[i] = pow(std::max(0.0,(epicmr[i]*epicmr[i] - mpi*mpi)),0.5);      
      epi2cmr[i] = (mass[i]*mass[i] + pow(2.*mpi,2) -mp2 )/2./mass[i];      
      ppi2cmr[i] = pow(std::max(0.0,(epi2cmr[i]*epi2cmr[i] - pow((2.*mpi),2))),0.5);      
      eetacmr[i] = (mass[i]*mass[i] + meta*meta -mp2 )/2./mass[i];      
      petacmr[i] =  pow(std::max(0.0,(eetacmr[i]*eetacmr[i] - meta*meta)),0.5);
            
      // ! Calculate partial widths      
      pwid[i][0] = intwidth[i]*pow((ppicm/ppicmr[i]),(2.*ang[i]+1.))*pow(((ppicmr[i]*ppicmr[i]+x0[i]*x0[i])/(ppicm*ppicm+x0[i]*x0[i])),ang[i]);            

      if(i != 1)       
	pwid[i][1] = intwidth[i]*pow((ppi2cm/ppi2cmr[i]),(2.*ang[i]+4.))*
		     pow(((ppi2cmr[i]*ppi2cmr[i]+x0[i]*x0[i])/(ppi2cm*ppi2cm+x0[i]*x0[i])),(ang[i]+2.))* W / mass[i];        	
      else  
	pwid[i][1] =  intwidth[1]*pow((petacm/petacmr[i]),(2.*ang[i]+1.))*
		      pow(((petacmr[i]*petacmr[i]+x0[i]*x0[i])/(petacm*petacm+x0[i]*x0[i])),ang[i]);        	
    
      pgam[i] = pow((kcm/kcmr[i]),2)*(kcmr[i]*kcmr[i]+x0[i]*x0[i])/(kcm*kcm+x0[i]*x0[i]);        	
      pgam[i] = intwidth[i]*pgam[i];        	
      width[i] = br[i][0]*pwid[i][0]+br[i][1]*pwid[i][1];        	
      sigr[i] = width[i] * pgam[i] / (pow(w2 - mass[i]*mass[i],2) + pow(mass[i]*width[i],2)) * kr[i] / k * kcmr[i] / kcm / intwidth[i];      
    }
    
    w2 = w2sv;

    // ! get parameters into local variables
    num = 12;
    //! resonance height coefficients. xvals of 13-36
    for (i = 0; i < 6; i++) {
        for (j = 0; j < 4; j++) {
            num = num + 1;
            rescoef[i][j]=xval[num-1];
        }
    }
    // !  Non-Res coefficients xvals of 37-44
    for (i = 0; i < 2; i++) {
        for (j = 0; j < 4; j++) {
            num = num + 1;
            nr_coef[i][j]=xval[num-1];
        }
    }

    // ! Begin resonance Q^2 dependence calculations   CCC
    // ! uses xvals 49
    for (i = 0; i < 6; i++) {
        height[i] = rescoef[i][0]*(1.+rescoef[i][1] * q2 / (1. + rescoef[i][2] * q2))/pow((1. + q2/0.91),rescoef[i][3]);
    }
    dip = 1./pow((1. + q2 / 0.91),2);
    height[6] = xval[48]*dip;
    sig_res = 0.;    
    for (i = 0; i < 7; i++) {
        sigrsv[i] =  height[i]*height[i] * sigr[i];
        sig_res = sig_res + sigrsv[i];
    }
    
    sig_res = sig_res * sqrt(w2);

    //! Begin non-resonant part uses xvals 45, 46, 50
    //! Depends on both W2 and Q2 so can't easily precalculate
    sig_nr = 0.;
    xpr = 1.+(w2-pow(mp+mpi,2))/(q2+0.05);
    xpr = 1./xpr;
    W = pow(w2,0.5);
    wdif = W - (mp + mpi);
    for (i = 0; i < 2; i++) {
        sig_nr = sig_nr +(nr_coef[i][0]*pow(wdif,((2*(i+1)+1)/2.)))/pow(q2+nr_coef[i][1],(nr_coef[i][2]+nr_coef[i][3]*q2+xval[44+i]*q2*q2));
    }
    sig_nr = sig_nr * xpr;
    sig = sig_res + sig_nr;

    F1 = sig * (w2-mp2)/8./pi/pi/alpha/0.3894e3;
    sig = F1;

    return 0;
}

//------------------------------------------------------------------//
// Used for Aluminum inelastic...
// always returns a calculated value (called resmod507 in the fortran code)
G4double resmod507_v2(G4double sf,G4double w2,
                      G4double q2,G4double xval[50])
{

    G4double W,mp,mp2,xb;
    G4double mass[7] = {0,0,0,0,0,0,0};
    G4double width[7] = {0,0,0,0,0,0,0};
    G4double height[7] = {0,0,0,0,0,0,0};
    G4double intwidth[7] = {0,0,0,0,0,0,0};
    G4double rescoef[6][4];
    G4double nr_coef[3][4],sigr[7],wdif[2],sig_nr,h_nr[3];
    G4double mpi,meta,k,kcm,kr[7],kcmr[7],ppicm,ppi2cm;
    G4double petacm,ppicmr[7],ppi2cmr[7],petacmr[7],epicmr[7],epi2cmr[7];
    G4double eetacmr[7],epicm,epi2cm,eetacm,br[7][3],ang[7];
    G4double pgam[7],pwid[7][3],x0[7],q20;
    G4double sig_res,t,xpr[2],m0,sig;
    G4int i,j,num;

    mp = proton_mass_c2/GeV;
    mpi = 0.135;
    meta = 0.547;
    mp2 = mp*mp;
    W = sqrt(w2);
    wdif[0] = W - (mp + mpi);
    wdif[1] = W - (mp + 2.*mpi);

    m0 = 0.125;
    if(sf == 2) m0 = xval[48];
    if(sf == 1) {
        q20 = 0.05;
    } else {
        q20 = 0.125;
    }

    // single pion branching ratios  CCCC

    br[0][0] = 1.0;       //!!!  P33(1232)
    br[1][0] = 0.45;      //!!!  S11(1535)
    br[2][0] = 0.65;      //!!!  D13(1520)
    br[3][0] = 0.65;      //!!!  F15(1680)
    br[4][0] = 0.4;       //!!!  S11(1650)
    br[5][0] = 0.65;      //!!!  P11(1440) roper
    br[6][0] = 0.50 ;     //!!!  F37(1950)

    br[0][2] = 0.0;       //!!!  P33(1232)
    br[1][2] = 0.45;      //!!!  S11(1535)
    br[2][2] = 0.0;       //!!!  D13(1520)
    br[3][2] = 0.0;       //!!!  F15(1680)
    br[4][2] = 0.1;       //!!!  S11(1650)
    br[5][2] = 0.0;       //!!!  P11(1440) roper
    br[6][2] = 0.0;       //!!!  F37(1950)

    // 2-pion branching ratios  CCCC

    for (i = 0; i < 7; i++) {
        br[i][1] = 1.-br[i][0]-br[i][2];
    }

    // Meson angular momentum   CCCC
    ang[0] = 1.;       //!!!  P33(1232)
    ang[1] = 0.;       //!!!  S11(1535)
    ang[2] = 2.;       //!!!  D13(1520)
    ang[3] = 3.;       //!!!  F15(1680)
    ang[4] = 0.;       //!!!  S15(1650)
    ang[5] = 1.;       //!!!  P11(1440) roper
    ang[6] = 3.;       //!!!  F37(1950)

    for (i = 0; i < 7; i++) { //!!!  resonance damping parameter  !!!
        x0[i] = 0.215;
    }
    x0[0] = 0.15;
    x0[0] = xval[49];

    xb = q2/(q2+w2-mp2);
    xpr[0] = 1.+(w2-pow((mp+mpi),2))/(q2+q20);
    xpr[0] = 1./xpr[0];
    xpr[1] = 1.+(w2-pow((mp+mpi+mpi),2))/(q2+q20);
    xpr[1] = 1./xpr[1];

    t = log(log((q2+m0)/0.330/0.330)/log(m0/0.330/0.330));

    // Calculate kinematics needed for threshold Relativistic B-W

    k = (w2 - mp2)/2./mp;
    kcm = (w2-mp2)/2./W;

    epicm = (w2 + mpi*mpi -mp2 )/2./W;
    ppicm = pow(std::max(0.0,(epicm*epicm - mpi*mpi)),0.5);
    epi2cm = (w2 + pow((2.*mpi),2) -mp2 )/2./W;
    ppi2cm = pow(std::max(0.0,(epi2cm*epi2cm - pow((2.*mpi),2))),0.5);
    eetacm = (w2 + meta*meta -mp2 )/2./W;
    petacm =  pow(std::max(0.0,(eetacm*eetacm - meta*meta)),0.5);

    num = 0;

    for (i = 0; i < 6; i++) { // !!!  Read in resonance masses     !!!
        num = num + 1;
        mass[i] = xval[i];
    }
    for (i = 0; i < 6; i++) { // !!!  Read in resonance widths     !!!
        num = num + 1;
        intwidth[i] = xval[num-1];
        width[i] = intwidth[i];
    }
    if (sf == 2) { //      !!!  Put in 4th resonance region  !!!
        mass[6] = xval[42];
        intwidth[6] = xval[43];
        width[6] = intwidth[6];
    } else {
        mass[6] = xval[46];
        intwidth[6] = xval[47];
        width[6] = intwidth[6];
    }

    for (i = 0; i < 7; i++) {
        kr[i] = (mass[i]*mass[i]-mp2)/2./mp;
        kcmr[i] = (mass[i]*mass[i]-mp2)/2./mass[i];
        epicmr[i] = (mass[i]*mass[i] + mpi*mpi -mp2 )/2./mass[i];
        ppicmr[i] = pow(std::max(0.0,(epicmr[i]*epicmr[i] - mpi*mpi)),0.5);
        epi2cmr[i] = (mass[i]*mass[i] + pow((2.*mpi),2) -mp2 )/2./mass[i];
        ppi2cmr[i] = pow(std::max(0.0,(epi2cmr[i]*epi2cmr[i] - pow((2.*mpi),2))),0.5);
        eetacmr[i] = (mass[i]*mass[i] + meta*meta -mp2 )/2./mass[i];
        petacmr[i] =  pow(std::max(0.0,(eetacmr[i]*eetacmr[i] - meta*meta)),0.5);

        // CCC   Calculate partial widths   CCC

        pwid[i][0] = intwidth[i]*pow((ppicm/ppicmr[i]),(2.*ang[i]+1.))*pow(((ppicmr[i]*ppicmr[i]+x0[i]*x0[i])/(ppicm*ppicm+x0[i]*x0[i])),ang[i]); //      !!!  1-pion decay mode

        pwid[i][1] = intwidth[i]*pow((ppi2cm/ppi2cmr[i]),(2.*ang[i]+4.))*pow(((ppi2cmr[i]*ppi2cmr[i]+x0[i]*x0[i])/(ppi2cm*ppi2cm+x0[i]*x0[i])),(ang[i]+2)); //  !!!  2-pion decay mode

        pwid[i][1] = W/mass[i]*pwid[i][1];
        pwid[i][2] = 0.; //          !!!  eta decay mode

        if(i == 1 || i == 4) {
            pwid[i][2] =  intwidth[i]*pow((petacm/petacmr[i]),(2.*ang[i]+1.))*pow(((petacmr[i]*petacmr[i]+x0[i]*x0[i])/(petacm*petacm+x0[i]*x0[i])),ang[i]); // !!!  eta decay only for S11's
        }

        pgam[i] = pow((kcm/kcmr[i]),2)*(pow(kcmr[i],2)+x0[i]*x0[i])/(kcm*kcm+x0[i]*x0[i]);
        pgam[i] = intwidth[i]*pgam[i];
        width[i] = br[i][0]*pwid[i][0]+br[i][1]*pwid[i][1]+br[i][2]*pwid[i][2];
    }

    //CCC    End resonance kinematics and Widths calculations   CCC

    // CCC    Begin resonance Q^2 dependence calculations   CCC

    for (i = 0; i < 6; i++) {
        for (j = 0; j < 4; j++) {
            num = num + 1;
            rescoef[i][j]=xval[num-1];
        }

        if(sf == 1) {
            height[i] = rescoef[i][0]*(1.+rescoef[i][1]*q2/(1.+rescoef[i][2]*q2))/pow((1.+q2/0.91),rescoef[i][3]);
        } else {
            height[i] = rescoef[i][0]*q2/(1.+rescoef[i][1]*q2)*exp(-1.*rescoef[i][2]*q2);
        }

        height[i] = height[i]*height[i];
    }

    if(sf == 2) { // !!!  4th resonance region  !!!
        height[6] = xval[44]*q2/(1.+xval[45]*q2)*exp(-1.*xval[46]*q2);
    } else {
        height[6] = xval[48]/(1.+q2/0.91);
    }

    height[6] = height[6]*height[6];

    // CCC    End resonance Q^2 dependence calculations   CCC

    for (i = 0; i < 3; i++) { // !!!  Non-Res coefficients  !!!
        for (j = 0; j < 4; j++) {
            num = num + 1;
            nr_coef[i][j]=xval[num-1];
        }
    }

    // CCC   Calculate Breit-Wigners for all resonances   CCC

    sig_res = 0.0;
    for (i = 0; i < 7; i++) {
        sigr[i] = width[i]*pgam[i]/(pow(w2 - mass[i]*mass[i],2.) + pow(mass[i]*width[i],2.));
        sigr[i] = height[i]*kr[i]/k*kcmr[i]/kcm*sigr[i]/intwidth[i];
        sig_res = sig_res + sigr[i];
    }
    sig_res = sig_res*W;
    // CCC    Finish resonances / start non-res background calculation   CCC
    sig_nr = 0.;
    if(sf == 1) {
        for (i = 0; i < 2; i++) {
            h_nr[i] = nr_coef[i][0]/pow(q2+nr_coef[i][1],nr_coef[i][2] + nr_coef[i][3]*q2+xval[44+i]*q2*q2);
            sig_nr = sig_nr +h_nr[i]*(pow(wdif[0],(2*(i+1)+1)/2.));
        }

        sig_nr = sig_nr*xpr[0];

    } else if (sf == 2) {
        for (i = 0; i < 1; i++) {
            sig_nr = sig_nr + nr_coef[i][0]*pow((1.-xpr[i]),(nr_coef[i][2]+nr_coef[i][1]*t))/(1.-xb)/(q2+q20);
            sig_nr = sig_nr*pow(q2/(q2+q20),nr_coef[i][3])*pow(xpr[i],(xval[40]+xval[41]*t));
        }
    }
    sig = sig_res + sig_nr;

    return sig;
}

G4double MEC2009(G4double q2,G4double w2, G4int A)//checked
{
    //! fit to low q2 dip region: purefly empirical
    //! assume contribution is pure transverse
    //returns 0 on A<2.5 and W2<=0, otherwise returns calculated value
    //propagated to F1F2IN09 into W1 as W1+=MEC2009
    G4double f1 = 0.0;
    G4double am = proton_mass_c2/GeV;
    G4double w,nu;

    G4double P[24] = {
        5.1377e-03,   9.8071e-01,   4.6379e-02,   1.6433e+00,
        6.9826e+00,  -2.2655e-01,   1.1095e-01,   2.7945e-02,
        4.0643e-01,   1.6076e+00,  -7.5460e+00,   4.4418e+00,
        -3.7464e-01,   1.0414e-01,  -2.6852e-01,   9.6653e-01,
        -1.9055e+00,   9.8965e-01,   2.0613e+02,  -4.5536e-02,
        2.4902e-01,  -1.3728e-01,   2.9201e+01,   4.9280e-03
    };

    G4double p18, x, f1corr;

    if (w2 <= 0.0) return 0;
    w  = sqrt(w2);
    nu = (w2 - am*am + q2) / 2. / am;
    x  = q2 / (2.0 * am * nu );

    if (A < 2.5) return 0;

    p18 = P[18];
    // ! special case for 3He
    if (A > 2.5 && A < 3.5) p18 = 70;
    // ! special case for 4He
    if (A > 3.5 && A < 4.5) p18 = 170;
    // ! new values for C, Al, Cu
    if(A > 4.5) p18 = 215;
    if(A > 20.) p18 = 235;
    if(A > 50.) p18 = 230;


    f1corr = P[0]*exp(-pow(w-P[1],2)/P[2])/(pow(1.0 + std::max(0.3,q2)/P[3],P[4]))*pow(nu,P[5])*(1.0 + p18 * pow(A,(1.0 + P[19] * x)));

    f1 = f1corr;

    if (f1 <= 1.0E-9 ) f1 = 0.0;

    if(std::isnan(f1)){
      G4cerr << "Error! nans "<<__FILE__ << " line " << __LINE__ <<" "<<G4endl;
      G4cerr <<" "<< f1 <<" "<<x<<" "<< nu <<" "<<w2<<" "<<p18<<" "<<q2<<" "<<G4endl;
      G4cerr <<"  "<< exp(-pow(w-P[1],2)/P[2]) << " " << (pow(1.0 + std::max(0.3,q2)/P[3],P[4])) <<" "<< pow(nu,P[5]) 
	     << " " << (1.0 + p18 * pow(A,(1.0 + P[19] * x)))<< G4endl;
    }
    return f1;
}

G4double fitemc(G4double X, G4int A)
{
    /*!---------------------------------------------------------------------
      ! Fit to EMC effect.  Steve Rock 8/3/94
      ! Funciton returns value of sigma(A)/sigma(d)
      ! with no isoscalerity correction
      ! A= atomic number
      ! x = Bjorken x.
      !
      ! Fit of sigma(A)/sigma(d) to form C*A**alpha where A is atomic number
      ! First data at each x was fit to form C*A**alpha.  The point A=2(d)
      !  was includded with a value of 1 and an error of about 2%.
      ! For x>=.125 Javier Gomez fit of 7/93 to E139 data was used.
      ! For .09 >=x>=.0085 NMC data from Amaudruz et al Z. Phys C. 51,387(91)
      !  Steve did the fit for alpha and C to the He/d. C/d and Ca/d NMC data.
      ! Alpha(x) was fit to a 9 term polynomial a0 +a1*x +a2*x**2 + a3*x**3 ..
      ! C(x) was fit to a 3 term polynomial in natural logs as
      !  Ln(C) = c0 + c1*Ln(x) + c2*[Ln(x)]**2.

      ! 6/2/98 *****  Bug (which set x= .00885 if x was out of range) fixed
      !                    also gave value at x=.0085 if x>.88
      ! 11/05 PYB PEB modified to use value at x=0.7 if x>0.7, because
      !    beyond that assume Fermi motion is giving the rise, and we
      !    already are taking that into account with the y-smearing of
      !    the inelastic
      !-----------------------------------------------------------------------
    */
  //return 0 on A<2.5 otherwise returns calculated value
  //propagated into F1F2IN09 for the calculation of F1 as a multiplicative factor

    G4double ALPHA,C,LN_C,X_U;
    G4double fitval;

    // !Chisq=         19.   for 30 points
    // !Term    Coeficient
    G4double ALPHA_COEF[9] = {
        -6.98871401E-02,
        2.18888887E+00,
        -2.46673765E+01,
        1.45290967E+02,
        -4.97236711E+02,
        1.01312929E+03,
        -1.20839250E+03,
        7.75766802E+02,
        -2.05872410E+02
    };

    // !Chisq=         22.    for 30 points
    // !Term    Coeficient
    G4double C_COEF[3] = {
        1.69029097E-02,
        1.80889367E-02,
        5.04268396E-03
    };

    fitval = 1.;
    if (A < 2.5) return 0;

    if ( (X > 0.70) || (X < 0.0085) ) {
        // !Out of range of fit
        if (X < 0.0085) X_U =.0085;
        if (X > 0.70) X_U = 0.70;
    } else {
        X_U = X;
    }

    LN_C = C_COEF[0];
    for (G4int i = 1; i <= 2; i++) {
        LN_C = LN_C + C_COEF[i] * pow(log(X_U),i);
    }

    C = exp(LN_C);

    ALPHA = ALPHA_COEF[0];
    for (G4int i = 1; i <= 8; i++) {
        ALPHA = ALPHA + ALPHA_COEF[i] * pow(X_U,i);
    }

    fitval = C*pow(A,ALPHA);
    return fitval;
}

//oooooooooooooooooooooo000ooooooooooooooooooooooooo
// Qweak Quasi Elastic part of the code
//oooooooooooooooooooooo000ooooooooooooooooooooooooo

void F1F2QE09(G4int Z, G4int IA, G4double QSQ,
	      G4double wsq, G4double &F1, G4double &F2)
{
//=======================================================================
//      SUBROUTINE F1F2QE09(Z, A, QSQ, wsq, F1, F2)
//
// Calculates quasielastic A(e,e')X structure functions F1 and F2 PER NUCLEUS
// for A>2 uses superscaling from Sick, Donnelly, Maieron, nucl-th/0109032
// for A=2 uses pre-integrated Paris wave function (see ~bosted/smear.f)
// coded by P. Bosted August to October, 2006
//
// input: Z, A  (real*8) Z and A of nucleus (shoud be 2.0D0 for deueron)
//        Qsq (real*8) is 4-vector momentum transfer squared (positive in
//                     chosen metric)
//        Wsq (real*8) is invarinat mass squared of final state calculated
//                     assuming electron scattered from a free proton
//                 
// outputs: F1, F2 (real*8) are structure functions per nucleus
//
// Note: Deuteron agrees well with Laget (see ~bosted/eg1b/laget.f) for
// a) Q2<1 gev**2 and dsig > 1% of peak: doesnt describe tail at high W
// b) Q2>1 gev**2 on wings of q.e. peak. But, this model is up
//    to 50% too big at top of q.e. peak. BUT, F2 DOES agree very
//    nicely with Osipenko et al data from CLAS, up to 5 GeV**2
  
//  G4cout << "Z, A, Q2, W2: " << Z << ", " << IA << ", " << QSQ << ", " << wsq << G4endl;

  G4double avgN, Pauli_sup1, Pauli_sup2, GEP, GEN, GMP, GMN, Q, Q3, Q4;
  G4double amp = proton_mass_c2/GeV;
  G4double RMUP = 2.792782;
  G4double RMUN = -1.913148;
  G4double Nu;
  G4double QV, TAU, FY;
  G4double kappa, lam, lamp, taup, squigglef, psi, psip, nuL, nuT;
  G4double kf, Es, GM2bar, GE2bar, W2bar, Delta, GL, GT;

  // Peter Bosted's correction params
  /*
  G4double pb[20] = {
    0.1023E+02, 0.1052E+01, 0.2485E-01, 0.1455E+01,
    0.5650E+01,-0.2889E+00, 0.4943E-01,-0.8183E-01,
    -0.7495E+00, 0.8426E+00,-0.2829E+01, 0.1607E+01,
    0.1733E+00, 0.0000E+00, 0.0000E+00, 0.0000E+00,
    0.0000E+00, 0.0000E+00, 0.0000E+00, 0.0000E+00};
  */
  G4double y,R;

  G4double P[24] = {
     5.1377e-03,   9.8071e-01,   4.6379e-02,   1.6433e+00,
     6.9826e+00,  -2.2655e-01,   1.1095e-01,   2.7945e-02,
     4.0643e-01,   1.6076e+00,  -7.5460e+00,   4.4418e+00,
     -3.7464e-01,   1.0414e-01,  -2.6852e-01,   9.6653e-01,
     -1.9055e+00,   9.8965e-01,   2.0613e+02,  -4.5536e-02,
     2.4902e-01,  -1.3728e-01,   2.9201e+01,   4.9280e-03};

  // return if proton: future change this to allow for
  // equivalent W resolution
  F1 = 0.;
  F2 = 0.;
  avgN = IA - Z;
  if (IA==1) return;

  // some kinematic factors. Return if Nu or QSQ is negative
  Nu = (wsq - amp*amp + QSQ) / 2. / amp;

  //G4cout << "In call... IA, Nu, QSQ = " << IA << ", " << Nu << ", " << QSQ << G4endl;
  if(Nu <= 0.0 || QSQ < 0.) return;
  TAU   = QSQ / 4.0 / amp / amp;                                 
  QV = sqrt(Nu*Nu + QSQ);

  // Bosted fit for nucleon form factors Phys. Rev. C 51, p. 409 (1995)
  Q = sqrt(QSQ);
  Q3 = QSQ * Q;
  Q4 = QSQ*QSQ;
  GEP = 1./  (1. + 0.14 * Q + 3.01 * QSQ + 0.02 * Q3 + 1.20 * Q4 + 0.32 * pow(Q,5));
  GMP = RMUP * GEP;
  GMN = RMUN / (1.- 1.74 * Q + 9.29 * QSQ - 7.63 * Q3 + 4.63 * Q4);
  GEN = 1.25 * RMUN * TAU / (1. + 18.3 * TAU) / pow((1. + QSQ / 0.71),2);

  //G4cout << "Form Factors: " << GEP << ", " << GMP << ", " << GEN << ", " << GMN << G4endl;

  // Get kf and Es from superscaling from Sick, Donnelly, Maieron,
    // nucl-th/0109032
    if(IA==2) kf=0.085;
    if(IA==2) Es=0.0022;
    // changed 4/09
    if(IA==3) kf=0.115;
    if(IA==3) Es=0.001 ;
    // changed 4/09
    if(IA>3) kf=0.19;
    if(IA>3) Es=0.017; 
    if(IA>7) kf=0.228;
    if(IA>7) Es=0.020;
    // changed 5/09
    if(IA>7) Es=0.0165;
    if(IA>16) kf=0.230;
    if(IA>16) Es=0.025;
    if(IA>25) kf=0.236;
    if(IA>25) Es=0.018;
    if(IA>38) kf=0.241;
    if(IA>38) Es=0.028;
    if(IA>55) kf=0.241;
    if(IA>55) Es=0.023;
    if(IA>60) kf=0.245;
    if(IA>60) Es=0.028;
    // changed 5/09 
    if(IA>55) Es=0.018;

  // Pauli suppression model from Tsai RMP 46,816(74) eq.B54
  if ((QV > 2.* kf) || (IA == 1)) {
    Pauli_sup2 =1.0;
  } else {
    Pauli_sup2 = 0.75 * (QV / kf) * (1.0 - (pow((QV / kf),2))/12.);
  }
  Pauli_sup1 = Pauli_sup2;

  //G4cout << "kf, Es, Paulisup1,2: " << kf << ", " << Es << ", " << Pauli_sup1 << ", " << Pauli_sup2 << G4endl;
  
  // structure functions with off shell factors
  kappa = QV / 2. / amp;
  lam = Nu / 2. / amp;
  lamp = lam - Es / 2. / amp;
  taup = kappa*kappa - lamp*lamp;
  squigglef = sqrt(1. + pow((kf/amp),2)) -1.;

  // Very close to treshold, could have a problem
  if(1.+lamp <= 0.) return;
  if(taup * (1. + taup) <= 0.) return;

  psi =  (lam  - TAU ) / sqrt(squigglef) / sqrt((1.+lam )* TAU + kappa * sqrt(TAU * (1. + TAU)));
  psip = (lamp - taup) / sqrt(squigglef) / sqrt((1.+lamp)*taup + kappa * sqrt(taup * (1. + taup)));
  nuL = pow((TAU / kappa / kappa),2);

  // changed definition of nuT from
  // nuT = TAU / 2. / kappa**2 + tan(thr/2.)**2
  // to this, in order to separate out F1 and F2 (F1 prop. to tan2 term)
  nuT = TAU / 2. / kappa / kappa;

  GM2bar = Pauli_sup1 * (Z * GMP*GMP + avgN * GMN*GMN);
  GE2bar = Pauli_sup2 * (Z * GEP*GEP + avgN * GEN*GEN);
  //G4double W1bar = TAU * GM2bar;
  W2bar = (GE2bar + TAU * GM2bar) / (1. + TAU);

  Delta = squigglef * (1. - psi*psi) * (sqrt(TAU * (1.+TAU)) / kappa + squigglef/3. *
     (1. - psi*psi) * TAU / kappa / kappa);
  GL = kappa*kappa / TAU * (GE2bar + Delta * W2bar) / 2. / kappa / (1. + squigglef * 
     (1. + psi*psi) / 2.);
  GT = (2. * TAU * GM2bar + Delta * W2bar) / 2. / kappa / (1. + squigglef * 
     (1. + psi*psi) / 2.);

  //G4cout << "nuL, nuT, GL, GT: " << nuL << ", " << nuT << ", " << GL << ", " << GT << G4endl;

  // added to prevent negative xsections:
  if (GT < 0) {
    GT = 0;
    //G4cout << "Reset GT to zero" << G4endl;
  }

  // from Maria Barbaro: see Amaro et al., PRC71,015501(2005).
  FY = 1.5576 / (1. + 1.7720*1.7720 * pow((psip + 0.3014),2)) / (1. + exp(-2.4291 * psip)) / kf;

  // final results
  F2 = Nu * FY * (nuL * GL + nuT * GT);
  F1 = amp * FY * GT / 2.;

  //G4cout << "nu, Fy, nuL, GL, nuT, GT, amp: " << G4endl;
  //G4cout << Nu << ", " << FY << ", " << nuL << ", " << GL << ", " << nuT << ", " << GT << ", " << amp << G4endl;

  if (F1 < 0.0) F1 = 0.;
  if (Nu > 0. && F1 > 0.) R = (F2 / Nu) / (F1 / amp) * (1. + Nu*Nu / QSQ) - 1.0;
  else R = 0.4/QSQ;


  // apply correction factors
  if ( IA > 2 ) {
    y = (wsq -amp*amp) / QV;
    //         F1 = F1 * (1. + pb(8) + pb(9) * y +
    //     >        pb(10)*y**2 + pb(11)*y**3 + pb(12)*y**4 )
    //         R = R * (1. + pb(13))
    //         F2 = Nu * F1/amp * (1. + R) / (1. + Nu**2/QSQ)

    // correction to correction Vahe
    if (wsq > 0.0) {
      F1=F1*(1.0+P[7]+P[8]*y+P[9]*y*y +P[10]*pow(y,3) +P[11]*pow(y,4));
      R = R * ( 1.0 + P[12] );
      F2 = Nu * F1/amp * (1. + R) / (1. + Nu*Nu/QSQ);
      if (F1 < 0.0) F1=0.0;
    }
  }
}


#endif//__CHRISTY_BOSTED_INELASTIC_HH








