#include "remollGenMoller.hh"

#include "CLHEP/Random/RandFlat.h"

#include "G4Material.hh"
#include "G4PhysicalConstants.hh"

#include "remollEvent.hh"
#include "remollVertex.hh"
#include "remolltypes.hh"

remollGenMoller::remollGenMoller(){
    fThCoM_min =    30.0*deg;
    fThCoM_max =   150.0*deg;

    fApplyMultScatt = true;
}

remollGenMoller::~remollGenMoller(){
}

void remollGenMoller::SamplePhysics(remollVertex *vert, remollEvent *evt){
    // Generate Moller event

    double beamE = vert->GetBeamE();
    double me    = electron_mass_c2;

    double beta_com  = sqrt( (beamE - me)/(beamE + me) );
    double gamma_com = 1.0/sqrt(1.0 - beta_com*beta_com);

    double e_com = me*gamma_com;
    double thcom = acos(CLHEP::RandFlat::shoot(cos(fThCoM_max), cos(fThCoM_min)));
    double phcom = CLHEP::RandFlat::shoot(0.0, 2.0*pi);

    double sigma = alpha*alpha*pow(3.0+cos(thcom)*cos(thcom),2.0)*hbarc*hbarc/pow(sin(thcom),4.0)/(2.0*me*beamE); // units of area

    double V = 2.0*pi*(cos(fThCoM_min) - cos(fThCoM_max));

    //  Multiply by Z because we have Z electrons
    //  here we must also divide by two because we are double covering 
    //  phasespace because of identical particles
    
    evt->SetEffCrossSection(sigma*V*vert->GetMaterial()->GetZ()/2.0);

    if( vert->GetMaterial()->GetNumberOfElements() != 1 ){
	G4cerr << __FILE__ << " line " << __LINE__ << 
	    ": Error!  Some lazy programmer didn't account for complex materials in the moller process!" << G4endl;
	exit(1);
    }

    G4double APV = electron_mass_c2*beamE*GF*4.0*sin(thcom)*sin(thcom)*(QWe+QWe_rad)/(sqrt(2.0)*pi*alpha*pow(3.0+cos(thcom)*cos(thcom),2.0));

    evt->SetAsymmetry(APV);
    evt->SetThCoM(thcom);

    //evt->SetQ2( 2.0*e_com*e_com*(1.0-cos(thcom)) );
    // Q2 is not actually well defined
    evt->SetQ2( 0.0 );

    double pperp = e_com*sin(thcom);
    double ppar  = e_com*cos(thcom);

    evt->ProduceNewParticle( G4ThreeVector(0.0, 0.0, 0.0), 
	                     G4ThreeVector(pperp*cos(phcom), pperp*sin(phcom), gamma_com*(ppar + e_com*beta_com) ), 
			     "e-" );

    evt->ProduceNewParticle( G4ThreeVector(0.0, 0.0, 0.0), 
	                     G4ThreeVector(-pperp*cos(phcom), -pperp*sin(phcom), gamma_com*(-ppar + e_com*beta_com) ), 
			     "e-" );

    return;

}
