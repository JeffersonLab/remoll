#include "remollGenpElastic.hh"

#include "CLHEP/Random/RandFlat.h"

#include "remollEvent.hh"
#include "remollVertex.hh"
#include "remollBeamTarget.hh"

#include "G4Material.hh"
#include "G4VPhysicalVolume.hh"
#include "G4LogicalVolume.hh"
#include "Randomize.hh"

#include "remolltypes.hh"

#define Euler 0.5772157
#define NINTERVAL 4

remollGenpElastic::remollGenpElastic(){
    fTh_min =    1e-3*deg;
    fTh_max =     4.0*deg;

    fApplyMultScatt = true;

    fBeamTarget = remollBeamTarget::GetBeamTarget();
}

remollGenpElastic::~remollGenpElastic(){
}

void remollGenpElastic::SamplePhysics(remollVertex *vert, remollEvent *evt){
    // Generate ep event
    
    //  Crazy weighting for brem because ep cross section blows up at low Q2

    // Get initial beam energy instead of using other sampling
    double beamE = fBeamTarget->fBeamE;
    double Ekin  = beamE - electron_mass_c2;

    std::vector <G4VPhysicalVolume *> targVols = fBeamTarget->GetTargVols();

    std::vector<G4VPhysicalVolume *>::iterator it = targVols.begin();
    while( (*it)->GetLogicalVolume()->GetMaterial()->GetName() != "LiquidHydrogen" 
	    && it != targVols.end() ){ it++; }

    if( (*it)->GetLogicalVolume()->GetMaterial()->GetName() != "LiquidHydrogen" ){
	G4cerr << __FILE__ << " line " << __LINE__ << ": ERROR could not find target" << G4endl;
	exit(1);
    }

    double bremcut = fBeamTarget->fEcut;

    double bt = (4.0/3.0)*fBeamTarget->fTravLen/(*it)->GetLogicalVolume()->GetMaterial()->GetRadlen();

    double prob, prob_sample, sample, eloss, value;
    value = 1.0;
    prob = 1.- pow(bremcut/Ekin,bt) - bt/(bt+1.)*(1.- pow(bremcut/Ekin,bt+1.))
	+ 0.75*bt/(2.+bt)*(1.- pow(bremcut/Ekin,bt+2.));
    prob = prob/(1.- bt*Euler + bt*bt/2.*(Euler*Euler+pi*pi/6.)); /* Gamma function */
    prob_sample = G4UniformRand();        /* Random sampling */

    double colEcut = 0.080*GeV;  // Expected cutoff for collimator
    double Evlo[NINTERVAL] = {
	bremcut,
	(beamE-bremcut-colEcut)*1.0*GeV/(11.0*GeV-colEcut-bremcut),
	(beamE-bremcut-colEcut)*8.0*GeV/(11.0*GeV-colEcut-bremcut),
	(beamE-bremcut-colEcut)*(beamE-colEcut)/(11.0*GeV-colEcut-bremcut),
    };

    double Evhi[NINTERVAL] = {
	(beamE-bremcut-colEcut)*1.0*GeV/(11.0*GeV-colEcut-bremcut),
	(beamE-bremcut-colEcut)*8.0*GeV/(11.0*GeV-colEcut-bremcut),
	(beamE-bremcut-colEcut)*(beamE-colEcut)/(11.0*GeV-colEcut-bremcut),
	(beamE-bremcut-colEcut)*beamE/(11.0*GeV-colEcut-bremcut)
    };

    double Eprob[NINTERVAL]  = { 0.07, 0.05, 0.875, 0.005};

    double Enorm[NINTERVAL];
    // Interval normalization
    for( int idx = 0; idx < NINTERVAL; idx++ ){
	Enorm[idx]  = ((Evhi[idx]-Evlo[idx])/(Evhi[NINTERVAL-1]-Evlo[0]))
	    /Eprob[idx];
    }

    int    Evidx;
    double evsum = 0.0;
    double vweight = 0.0;
	     
    // Averages over the intervals
    double vavg[4] = {
	log(Evhi[0]/Evlo[0])/(Evhi[0]-Evlo[0]),
	1.0,
	log((beamE-Evlo[2])/(beamE-Evhi[2]))/(Evhi[2]-Evlo[2]),
	1.0};

    if (prob_sample <= prob) {//Bremsstrahlung has taken place!
	//  We break this into 4 seperate energy loss intervals
	//  with total integrals roughly the size of
	//  what the ep product looks like with 11 GeV beam
	//   cut  -  1000 MeV, 1/x, 7%
	//   1000 -  8000 MeV, flat 5%
	//   8000 - 10920 MeV, 1/(E-x), 87.5%
	//  10920 - 11000 MeV, flat, 0.5%

	sample = G4UniformRand();

	// Identify our region
	// based on the probability distribution
	Evidx = 0;
	evsum  = Eprob[Evidx];
	while( evsum < sample ){
	    Evidx++;
	    evsum += Eprob[Evidx];
	}

	sample = G4UniformRand();

	if( Evidx == 1 || Evidx == 3 ){
	    // sample energy flat
	    eloss = (Evhi[Evidx]-Evlo[Evidx])*sample + Evlo[Evidx];
	    vweight = 1.0;
	}

	if( Evidx == 0 ){
	    eloss = Evlo[Evidx]*pow(Evhi[Evidx]/Evlo[Evidx],sample);
	    vweight = eloss;
	}

	if( Evidx == 2 ){
	    eloss = beamE - (beamE-Evhi[Evidx])*
		pow((beamE-Evlo[Evidx])/(beamE-Evhi[Evidx]),sample);
	    vweight = (beamE-eloss);
	}

	vweight *= vavg[Evidx];
	//  mult by beamE-bremcut for proper normalization
	value = RadProfile( eloss, bt)*
	    ((Evhi[NINTERVAL-1]-Evlo[0])/EnergNumInt(bt, Evlo[0], Evhi[NINTERVAL-1])) // average of RadProfile
	    *vweight // sampling weighting (flat or ) / average value for normalization
	    *Enorm[Evidx]; //  Weight given the region

	beamE = beamE - eloss;
    }

    if( beamE < electron_mass_c2 ){ 
	evt->SetEffCrossSection(0.0);
	evt->SetAsymmetry(0.0);
	return; 
    }

    // Set event information to our new sampling
    evt->fBeamE = beamE;
    evt->fBeamMomentum = evt->fBeamMomentum.unit()*sqrt(beamE*beamE - electron_mass_c2*electron_mass_c2);;

    ////////////////////////////////////////////////////////////////////////////////////////////


    double th = acos(CLHEP::RandFlat::shoot(cos(fTh_max), cos(fTh_min)));
    double ph = CLHEP::RandFlat::shoot(0.0, 2.0*pi);

    double ef    = proton_mass_c2*beamE/(proton_mass_c2 + beamE*(1.0-cos(th)));;

    double q2  = 2.0*beamE*ef*(1.0-cos(th));
    double tau = q2/(4.0*proton_mass_c2*proton_mass_c2);

    double gd = pow( 1.0 + q2/0.71, -2.0 );
    double ge = gd;
    double gm = 2.79*gd;

    double gen =  1.91*gd/(1.0+5.6*tau); // galster
    double gmn = -1.91*gd;

    double sigma_mott = hbarc*hbarc*pow(alpha*cos(th), 2.0)/pow(2.0*beamE*sin(th)*sin(th), 2.0);
    double ffpart1 = (ge*ge + tau*gm*gm)/(1.0+tau);
    double ffpart2 = 2.0*tau*gm*gm*tan(th/2.0)*tan(th/2.0);

    double sigma = sigma_mott*(ef/beamE)*(ffpart1 + ffpart2);

    double V = 2.0*pi*(cos(fTh_min) - cos(fTh_max));

    //  Multiply by Z because we have Z protons 
    //  value for uneven weighting
    evt->SetEffCrossSection(sigma*V*vert->GetMaterial()->GetZ()*value);

    if( vert->GetMaterial()->GetNumberOfElements() != 1 ){
	G4cerr << __FILE__ << " line " << __LINE__ << 
	    ": Error!  Some lazy programmer didn't account for complex materials in the moller process!" << G4endl;
	exit(1);
    }

    G4double APV_base = GF*q2/(4.0*sqrt(2.0)*pi*alpha);

    G4double rhop = 0.9878;
    G4double kp   = 1.0029;

    G4double eps = pow(1.0 + 2.0*(1.0+tau)*tan(th/2.0)*tan(th/2.0), -1.0);

    G4double apvffnum = eps*ge*gen + tau*gm*gmn;
    G4double apvffden = eps*ge*ge  + tau*gm*gm;

    G4double APV = APV_base*rhop*( (1.0 - 4*kp*sin2thW) - apvffnum/apvffden);

    evt->SetAsymmetry(APV);

    evt->SetQ2( q2 );
    evt->SetW2( proton_mass_c2*proton_mass_c2 );

    // FIXME REradiate

    evt->ProduceNewParticle( G4ThreeVector(0.0, 0.0, 0.0), 
	    G4ThreeVector(ef*cos(ph)*sin(th), ef*sin(ph)*sin(th), ef*cos(th) ), 
	    "e-" );

    return;

}

G4double remollGenpElastic::RadProfile(G4double eloss, G4double btt){
     double Ekin = fBeamTarget->fBeamE - electron_mass_c2;
     double retval = 1./eloss*(1.-eloss/Ekin+0.75*pow(eloss/Ekin,2))*pow(eloss/Ekin,btt);

     assert( !isnan(retval) && !isinf(retval) );

     return retval;
}

G4double remollGenpElastic::EnergNumInt(G4double btt, G4double a0, G4double b0){
    const int nbin = 1000;
    double sum = 0.0;
    double bremcut = fBeamTarget->fEcut;

    int j;
    double boolc[5] = {7.0, 32.0, 12.0, 32.0, 7.0};

    double a, b, thissum;

    for(int i =0;i<nbin;i++) {
	// Integrate over sample spacings that are logarithmic
	a = bremcut*pow(b0/a0, (((double) i)/nbin));
	b = bremcut*pow(b0/a0, (((double) i+1.0)/nbin));

	// Boole's rule
	thissum = 0.0;
	for( j = 0; j < 5; j++ ){
	    thissum +=  boolc[j]*RadProfile( (b-a)*j*0.25 + a, btt);
	}
	sum += thissum*(b-a)/90.0;
    }

     assert( !isnan(sum) && !isinf(sum) );

    return sum;
}















