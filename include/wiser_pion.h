#ifndef __WISER_PION_HH
#define __WISER_PION_HH

#include "TF1.h"

#define me 0.511E-3 //electron restmass (GeV)
#define alpha 0.007299

#define __WISER_EPS 1e-4
#define __WISER_N_LEG_PTS 100


Double_t wiser_all_fit(Double_t *x, Double_t *par);

Double_t wiser_sigma(Double_t Ebeam, Double_t pf, Double_t thf, Double_t rad_len, Int_t type){
    /*
        Adapted from the infamous Wiser code from Peter Bosted

	Seamus Riordan
	riordan@jlab.org
	September 4, 2012
       
     	type = 0 pi+
	       1 pi-
	       2 K+
	       3 K-
	       4 p
	       5 p-bar
      
	type defines the type of particle produced
        Ebeam and pf are the beam energy in GeV and final particle momentum 
	in GeV/c respectively thf is the lab angle relative to the beam of 
	the produced particle in radians

	rad_len is the *effective* *total* radiator. [not in percent, typically 
	called bt in the literature]  This means put in the effective radiator for 
	internal radiation BEFORE and AFTER the vertex (total 0.05 for JLab kinematics)
       	on top of real radiation lengths from ext brem times 4/3

	Returns the cross section in units of nanobarns/GeV/str
     */

    const int ntype = 6;

    Double_t A5[] = {-5.49,  -5.23, -5.91, -4.45, -6.77,  -6.53 };
    Double_t A6[] = {-1.73,  -1.82, -1.74, -3.23,  1.90,  -2.45 };

    const Double_t mass_p  = 0.9383;
    const Double_t mass_p2 = mass_p*mass_p;
    const Double_t mass_pi = 0.1396;
    const Double_t mass_K  = 0.4973;
    const Double_t mass_Lambda  = 1.116;

    Double_t mass[] = {mass_pi, mass_pi, mass_K, mass_K, mass_p, mass_p};
    Double_t *mass2 = new Double_t[ntype];

    int i;
    for( i = 0; i < 6; i++ ){
	mass2[i] = mass[i]*mass[i];
    }

    // Calculate minimum photon energy required to produce such a system
    // with the particle of transverse momentum pf*sin(thf)
    // This is the case where in the CoM frame, the particle and the
    // residual system are going back to back, transverse to the incoming particles
    // and all components of the residual system are at rest relative to one another

    double M_X;// invariant mass of the minimum residual system

    switch( type ){
	// pi+ can just be N
	case 0:
	    M_X = mass_p;
	    break;
	// pi- needs a pi+ N
	case 1:
	    M_X = mass_p + mass_pi;
	    break;
	// K+ can be created with just a Lambda, final state
	// NOTE:  This is different from the original Wiser
	// code!  They had just proton mass, but you still have
	// to have an additional strange quark lying around
	// The lowest energy configuration you can have like this
	// is the Lambda
	case 2:
	    M_X = mass_Lambda;
	    break;
	// p/p-bar production is twice the mass of the proton
	case 4:
	case 5:
	    M_X = 2.0*mass_p;
	    break;
	default:
	    fprintf(stderr, "%s: %s line %d - Forbidden type passed to Wiser parameterization\n",
		    __PRETTY_FUNCTION__, __FILE__, __LINE__ );
	    exit(1);
	    break;
    }

    double MX2  = M_X*M_X;
    double Ef = sqrt(pf*pf + mass2[type]);

    Double_t E_gamma_min = ( MX2 - mass2[type] - mass_p2 + 2.0*mass_p*Ef )/
	           ( 2.0*( mass_p - Ef + pf*cos(thf) ) );

    // Parameterization parameters
    double pT = pf*sin(thf);
    double ML = sqrt(pT*pT + mass2[type]);

    double sigma, sig_e;
    double fitres;


    if( E_gamma_min > 0.0 && E_gamma_min < Ebeam ){
	int np = __WISER_N_LEG_PTS;

	double *x=new double[np];
	double *w=new double[np];

	TF1 *wiserfit = new TF1("wiserfit", wiser_all_fit, E_gamma_min, Ebeam, 5);
	wiserfit->SetParameter(0, Ebeam);
	wiserfit->SetParameter(1, pf);
	wiserfit->SetParameter(2, thf);
	wiserfit->SetParameter(3, (Double_t) type);
	wiserfit->SetParameter(4, M_X);


	wiserfit->CalcGaussLegendreSamplingPoints(np, x, w, __WISER_EPS);
	fitres = wiserfit->IntegralFast(np, x, w, E_gamma_min, Ebeam);

	delete wiserfit;
	delete x;
	delete w;

	if( type != 4 ){
	    sig_e = fitres*exp(A5[type]*ML)*exp(A6[type]*pT*pT/Ef);
	} else {
	    sig_e = fitres*exp(A5[type]*ML);
	}

	// Factor of 1000 is required for units
	sigma = pf*pf*sig_e*rad_len*1000.0/Ef;

	delete mass2;
	return sigma;
    } else {
	// Kinematically forbidden
	delete mass2;
	return 0.0;
    }

    delete mass2;
    return 0.0;
}


Double_t wiser_all_fit(Double_t *x, Double_t *par){
    // Primary variable x[0] is photon energy in [GeV]
    
    // Parameters are:
    // par[0]    Beam energy [GeV]
    //Double_t Ebeam = par[0];
    // par[1]    Final particle momentum [GeV/c]
    Double_t pf    = par[1];
    // par[2]    Final particle angle [rad]
    Double_t thf   = par[2];
    // par[3]    Type (as defined in wiser_all_sig)
    Int_t type  = (Int_t) par[3];
    // par[4]    Minimum invariant mass of the residual system [GeV]
    Double_t M_X   = par[4];


    const Double_t mass_p  = 0.9383;
    const Double_t mass_p2 = mass_p*mass_p;
    const Double_t mass_pi = 0.1396;
    const Double_t mass_K  = 0.4973;

    Double_t mass[] = {mass_pi, mass_pi, mass_K, mass_K, mass_p, mass_p};

    Double_t E_gamma = x[0];

    double s = mass_p2 + 2.0*E_gamma*mass_p;

    /*  Wiser's fit    pi+     pi-    k+     k-     p+       p-  */
    Double_t A1[] =  {566.,  486.,   368., 18.2,  1.33E5,  1.63E3 };
    Double_t A2[] =  {829.,  115.,   1.91, 307.,  5.69E4, -4.30E3};
    Double_t A3[] =  {1.79,  1.77,   1.91, 0.98,  1.41,    1.79 };
    Double_t A4[] =  {2.10,  2.18,   1.15, 1.83,   .72,    2.24 };
    Double_t A6 =  1.90;
    Double_t A7 = -.0117;


    // Boost to CoM
    double beta_cm = E_gamma/(E_gamma+mass_p);
    double gamma_cm = 1.0/sqrt(1.0 - beta_cm*beta_cm);

    double p_cm_z = -gamma_cm*beta_cm*sqrt(pf*pf+mass[type]*mass[type])
	            +gamma_cm*pf*cos(thf);

    double pT   = pf*sin(thf);
    double p_cm = sqrt( pT*pT + p_cm_z*p_cm_z );
    double Ef = sqrt( p_cm*p_cm + mass[type]*mass[type] );

    double p_cm_max = sqrt(s +pow(M_X*M_X - mass[type]*mass[type],2.0)/s -
	   2.0*(M_X*M_X + mass[type]*mass[type]) )/2.0;

    double X_R = p_cm/p_cm_max;

    if( X_R > 1.0 ){ return 0.0; } // Kinematically forbidden


    if( type != 4 ){ // Everything but proton
	return ( A1[type] + A2[type]/sqrt(s) )*
	    pow(1.0 - X_R + A3[type]*A3[type]/s, A4[type])/E_gamma;
    } else {
	Double_t U_MAN = fabs(2.0*mass_p2 - 2.0*mass_p*Ef);

	return ( A1[type] + A2[type]/sqrt(s) )*
	    pow(1.0 - X_R + A3[type]*A3[type]/s, A4[type])/pow(1.0 + U_MAN,A6+A7*s)/E_gamma;
    }

    return 0.0;
}


#endif//__WISER_PION_HH








