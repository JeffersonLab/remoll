#include "remollBeamTarget.hh"
#include "remollGenPion.hh"

#include "G4GenericMessenger.hh"
#include "G4String.hh"
#include "Randomize.hh"

//#include "wiser_pion.h"
#include "remollEvent.hh"
#include "remollVertex.hh"
#include "G4Material.hh"
#include "G4PhysicalConstants.hh"

#include "remolltypes.hh"

#include "TF1.h"
#include "TF3.h"


remollGenPion::remollGenPion()
: remollVEventGen("pion"),
  fPionType(kPiMinus) {
    fApplyMultScatt = false;

    fTh_min = 0.0*deg;
    fTh_max = 2.0*deg;

    // Scott Mundy 4/14/2016, attempting to solve cross section issue by defining fPh_max and fPh_min
    fPh_min = 0.0*deg;
    fPh_max = 359.9*deg;

    fE_min = 0.0*MeV;
    fE_max = -1.0*GeV; // negative to automatically pick beam energy

    // Add to generic messenger
    fMessenger->DeclareMethod("piontype",&remollGenPion::SetPionTypeByString_Deprecated,"Generate pion type");
    fThisGenMessenger->DeclareMethod("settype",&remollGenPion::SetPionTypeByString,"Generate pion type");
}

remollGenPion::~remollGenPion() { }

void remollGenPion::SamplePhysics(remollVertex *vert, remollEvent *evt){
    // Generate Pion event

  double beamE   = vert->GetBeamEnergy();
    // Use unradiated beam vertex
    //double beamE = remollBeamTarget::GetBeamTarget()->fBeamE;
    double rad_len = vert->GetRadiationLength();

    double th = acos(G4RandFlat::shoot(cos(fTh_max), cos(fTh_min)));
    double ph = G4RandFlat::shoot(fPh_min, fPh_max);

    double true_emax = 0.0;
    //For pion generation we don't set fE_min and fE_max so for now true_emax = beamE : rakitha Wed Sep 25 10:43:57 EDT 2013
    if( fE_max < 0.0 || fE_max > beamE ){
	true_emax = beamE;
    } else {
	true_emax = fE_max;
    }
    
    double pf = G4RandFlat::shoot(fE_min, true_emax);

    assert( pf > 0.0 );
    assert( pf < beamE );
    //solid angle in steradians times the integral of pion energies from 0 to beamE -> int dE from 0 to beamE: rakitha Tue Sep 24 14:11:36 EDT 2013
   
   
    double V = (fPh_max-fPh_min)*(cos(fTh_min) - cos(fTh_max))*true_emax;

    double intrad = 2.0*alpha*log(beamE/electron_mass_c2)/pi;
    
    // *effective* *total* radiator = rad_len*4/3 + internal rad length
    double sigpip = wiser_sigma(beamE/GeV, pf/GeV, th, rad_len*4.0/3.0 + intrad, 0)*nanobarn/GeV;
    double sigpim = wiser_sigma(beamE/GeV, pf/GeV, th, rad_len*4.0/3.0 + intrad, 1)*nanobarn/GeV;

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
    //printf("DEBUG:  xs %f \n ",V*thisxs); 
    
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
    return;

}



Double_t remollGenPion::wiser_sigma(Double_t Ebeam, Double_t pf, Double_t thf, Double_t rad_len, Int_t type){
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

	TF1 *wiserfit = new TF1("wiserfit", remollGenPion::wiser_all_fit, E_gamma_min, Ebeam, 5);
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

Double_t remollGenPion::wiser_total_sigma(Double_t Ebeam, Double_t intrad, Double_t extrad, Int_t type){
  /*
    	TF3(const char* name, void* fcn, Double_t xmin = 0, Double_t xmax = 1, Double_t ymin = 0, Double_t ymax = 1, Double_t zmin = 0, Double_t zmax = 1, Int_t npar = 0)
   */
    TF3 *fullwiser = new TF3("fullwiser", remollGenPion::wiser_tf3, 0, Ebeam, -1.0, 1.0, 0, 1.0, 4);

    fullwiser->SetParameter(0, Ebeam);
    fullwiser->SetParameter(1, intrad);
    fullwiser->SetParameter(2, extrad);
    fullwiser->SetParameter(3, (double) type);

    return fullwiser->Integral(0, Ebeam, -1,1, 0.0, 1.0)*2.0*pi;
}
