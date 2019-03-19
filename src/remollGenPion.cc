#include "remollBeamTarget.hh"
#include "remollGenPion.hh"

#include "G4GenericMessenger.hh"
#include "G4String.hh"
#include "Randomize.hh"

#include "remollEvent.hh"
#include "remollVertex.hh"
#include "G4Material.hh"
#include "G4PhysicalConstants.hh"

#include "G4SystemOfUnits.hh"
#include "G4PhysicalConstants.hh"

#include "remolltypes.hh"

#include "TF1.h"


remollGenPion::remollGenPion()
: remollVEventGen("pion"),
  fPionType(kPiMinus) {
    fApplyMultScatt = false;

    fTh_min = 0.0*deg;
    fTh_max = 2.0*deg;

    // Scott Mundy 4/14/2016, attempting to solve cross section issue by defining fPh_max and fPh_min
    fPh_min = 0.0*deg;
    fPh_max = 360.0*deg;

    fE_min = 0.0*MeV;
    fE_max = -1.0*GeV; // negative to automatically pick beam energy

    // Add to generic messenger
    fThisGenMessenger->DeclareMethod("piontype",&remollGenPion::SetPionTypeByString_Deprecated,"Generate pion type");
    fThisGenMessenger->DeclareMethod("settype",&remollGenPion::SetPionTypeByString,"Generate pion type");
}

remollGenPion::~remollGenPion() { }

void remollGenPion::SamplePhysics(remollVertex *vert, remollEvent *evt)
{
    // Generate Pion event

    // Use radiated beam vertex
    double beamE   = vert->GetBeamEnergy();
    // Use unradiated beam vertex
    //double beamE = remollBeamTarget::GetBeamTarget()->fBeamEnergy;

    double rad_len = vert->GetRadiationLength();

    double th = acos(G4RandFlat::shoot(cos(fTh_max), cos(fTh_min)));
    double ph = G4RandFlat::shoot(fPh_min, fPh_max);

    // For pion generation we don't set fE_min and fE_max so for now true_emax = beamE : rakitha Wed Sep 25 10:43:57 EDT 2013
    double true_emax = 0.0;
    if (fE_max < 0.0 || fE_max > beamE) {
	true_emax = beamE;
    } else {
	true_emax = fE_max;
    }
    // If we radiated so much that beamE < fE_min, we just set true_emin to beamE and have zero phase space
    double true_emin = 0.0;
    if (fE_min > 0.0 && fE_min < true_emax) {
        true_emin = fE_min;
    } else if (fE_min > 0.0 && fE_min > true_emax) {
        true_emin = true_emax;
    } else {
        true_emin = 0.0;
    }

    // Shoot
    double pf = G4RandFlat::shoot(true_emin, true_emax);

    if (!(pf >= 0.0) || !(pf <= beamE)) {
      G4cout << "remollGenPion::SamplePhysics: ERROR"
             << " fE_min, fE_max = [" << fE_min << "," << fE_max << "] "
             << " true_emin, true_emax = [" << true_emin << "," << true_emax << "] "
             << " with beamE = " << beamE
             << " but pf = " << pf << G4endl;
      exit(-1);
    }
    //solid angle in steradians times the integral of pion energies from 0 to beamE -> int dE from 0 to beamE: rakitha Tue Sep 24 14:11:36 EDT 2013


    double V = (fPh_max - fPh_min) * (cos(fTh_min) - cos(fTh_max)) * (true_emax - true_emin);

    double intrad = 2.0*alpha*log(beamE/electron_mass_c2)/pi;

    // *effective* *total* radiator = rad_len*4/3 + internal rad length
    double sigpip = wiser_sigma(beamE/GeV, pf/GeV, th/rad, rad_len*4.0/3.0 + intrad, 0)*nanobarn/GeV;
    double sigpim = wiser_sigma(beamE/GeV, pf/GeV, th/rad, rad_len*4.0/3.0 + intrad, 1)*nanobarn/GeV;

    G4String piontypestr;

    double thisxs = 0.0;

    switch(fPionType){
	case kPiMinus:
	    piontypestr = G4String("pi-");
	    thisxs = vert->GetMaterial()->GetZ()*sigpim + (vert->GetMaterial()->GetA()*mole/g-vert->GetMaterial()->GetZ())*sigpip;
	    break;
	case kPiPlus:
	    piontypestr = G4String("pi+");
	    thisxs = vert->GetMaterial()->GetZ()*sigpip + (vert->GetMaterial()->GetA()*mole/g-vert->GetMaterial()->GetZ())*sigpim;
	    break;
	case kPi0:
	    piontypestr = G4String("pi0");
	    thisxs = vert->GetMaterial()->GetZ()*(sigpip+sigpim)/2.0 + (vert->GetMaterial()->GetA()*mole/g-vert->GetMaterial()->GetZ())*(sigpip+sigpim)/2.0;
	    break;
	default:
	    piontypestr = G4String("oops");
	    break;
    }



    //thisxs is in nanobarns per GeV per str
    //to get effective cross section in nanobarns, EffCrossSection = V*thisxs where V is in str.GeV
    //also see main.f rate calculation in original fortran code  (main.f lines 209 - 211)
    //rakitha  Tue Sep 24 11:06:41 EDT 2013
    // G4cout << "V: " << V << " thisxs: " << thisxs <<  G4endl;
    evt->SetEffCrossSection(V*thisxs);

    if( vert->GetMaterial()->GetNumberOfElements() != 1 ){
	G4cerr << __FILE__ << " line " << __LINE__ <<
	    ": Error!  Some lazy programmer didn't account for complex materials in the moller process!" << G4endl;
	exit(1);
    }

    evt->SetAsymmetry(0.0);
    evt->SetRate(0);

    evt->ProduceNewParticle( G4ThreeVector(0.0, 0.0, 0.0),
	    G4ThreeVector(pf*cos(ph)*sin(th), pf*sin(ph)*sin(th), pf*cos(th)),
	    piontypestr );
}


double remollGenPion::wiser_sigma(
	double Ebeam, // GeV
	double pf,    // GeV
	double thf,   // rad
	double rad_len, // 1
	int type)
{
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

    const double mass_p  = 0.9383;
    const double mass_p2 = mass_p*mass_p;
    const double mass_pi = 0.1396;
    const double mass_K  = 0.4973;
    const double mass_Lambda  = 1.116;

    double mass[] = {mass_pi, mass_pi, mass_K, mass_K, mass_p, mass_p};
    double *mass2 = new double[ntype];
    for (int i = 0; i < 6; i++) {
	mass2[i] = mass[i]*mass[i];
    }

    // Calculate minimum photon energy required to produce such a system
    // with the particle of transverse momentum pf*sin(thf)
    // This is the case where in the CoM frame, the particle and the
    // residual system are going back to back, transverse to the incoming particles
    // and all components of the residual system are at rest relative to one another

    double M_X; // invariant mass of the minimum residual system

    switch (type) {
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
	    G4cerr << __PRETTY_FUNCTION__ << ": " __FILE__ << " " << __LINE__ << " - "
                   << "Forbidden type passed to Wiser parameterization" << G4endl;
	    exit(1);
	    break;
    }

    double MX2 = M_X*M_X;
    double Ef = sqrt(pf*pf + mass2[type]);

    double E_gamma_min = ( MX2 - mass2[type] - mass_p2 + 2.0*mass_p*Ef )/
	           ( 2.0*( mass_p - Ef + pf*cos(thf) ) );

    // Parameterization parameters
    double pT = pf*sin(thf);
    double ML = sqrt(pT*pT + mass2[type]);

    // done with mass2
    delete[] mass2;

    if( E_gamma_min > 0.0 && E_gamma_min < Ebeam ){
	const int __WISER_N_LEG_PTS = 100;
	int np = __WISER_N_LEG_PTS;

	double *x = new double[np];
	double *w = new double[np];

        #if ROOT_VERSION_CODE >= ROOT_VERSION(6,0,0)
	TF1 *wiserfit = new TF1("wiserfit", remollGenPion::wiserfit, E_gamma_min, Ebeam, 5, 1, TF1::EAddToList::kNo);
        #else
	TF1 *wiserfit = new TF1("wiserfit", remollGenPion::wiserfit, E_gamma_min, Ebeam, 5);
        #endif
	wiserfit->SetParameter(0, Ebeam);
	wiserfit->SetParameter(1, pf);
	wiserfit->SetParameter(2, thf);
	wiserfit->SetParameter(3, (double) type);
	wiserfit->SetParameter(4, M_X);

	const double eps = 1e-4;
	wiserfit->CalcGaussLegendreSamplingPoints(np, x, w, eps);

	double fitres = wiserfit->IntegralFast(np, x, w, E_gamma_min, Ebeam);

	delete wiserfit;
	delete[] x;
	delete[] w;

        const double A5[] = {-5.49,  -5.23, -5.91, -4.45, -6.77,  -6.53 };
        const double A6[] = {-1.73,  -1.82, -1.74, -3.23,  1.90,  -2.45 };

        double sig_e = 0.0;
	if( type != 4 ){
	    sig_e = fitres*exp(A5[type]*ML)*exp(A6[type]*pT*pT/Ef);
	} else {
	    sig_e = fitres*exp(A5[type]*ML);
	}

	// Factor of 1000 is required for units
	double sigma = pf*pf*sig_e*rad_len*1000.0/Ef;

	return sigma;
    } else {
	// Kinematically forbidden
	return 0.0;
    }

    return 0.0;
}

double remollGenPion::wiserfit(double *x, double *par)
{
    // Primary variable x[0] is photon energy in [GeV]

    // Parameters are:
    // par[0]    Beam energy [GeV]
    //double Ebeam = par[0];
    // par[1]    Final particle momentum [GeV/c]
    double pf    = par[1];
    // par[2]    Final particle angle [rad]
    double thf   = par[2];
    // par[3]    Type (as defined in wiser_all_sig)
    int type     = (int) par[3];
    // par[4]    Minimum invariant mass of the residual system [GeV]
    double M_X   = par[4];


    const double mass_p  = 0.9383;
    const double mass_p2 = mass_p*mass_p;
    const double mass_pi = 0.1396;
    const double mass_K  = 0.4973;

    const double mass[] = {mass_pi, mass_pi, mass_K, mass_K, mass_p, mass_p};

    double E_gamma = x[0];

    double s = mass_p2 + 2.0*E_gamma*mass_p;

    /*  Wiser's fit       pi+    pi-     k+    k-     p+       p-  */
    const double A1[] =  {566.,  486.,   368., 18.2,  1.33E5,  1.63E3 };
    const double A2[] =  {829.,  115.,   1.91, 307.,  5.69E4, -4.30E3};
    const double A3[] =  {1.79,  1.77,   1.91, 0.98,  1.41,    1.79 };
    const double A4[] =  {2.10,  2.18,   1.15, 1.83,   .72,    2.24 };
    const double A6 =  1.90;
    const double A7 = -.0117;

    // Boost to CoM
    double beta_cm = E_gamma / (E_gamma + mass_p);
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
        double U_MAN = fabs(2.0*mass_p2 - 2.0*mass_p*Ef);

        return ( A1[type] + A2[type]/sqrt(s) )*
            pow(1.0 - X_R + A3[type]*A3[type]/s, A4[type])/pow(1.0 + U_MAN,A6+A7*s)/E_gamma;
    }

    return 0.0;
}

