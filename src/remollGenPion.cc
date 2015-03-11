#include "remollGenPion.hh"

#include "CLHEP/Random/RandFlat.h"

#include "G4String.hh"
#include "G4Material.hh"
#include "G4PhysicalConstants.hh"

#include "wiser_pion.h"
#include "remollEvent.hh"
#include "remollVertex.hh"
#include "remolltypes.hh"

remollGenPion::remollGenPion(){
    fApplyMultScatt = false;

    fTh_min = 0.01*deg;
    fTh_max = 5.0*deg;
    fE_min  = 80*MeV;

    fPionType = kPiMinus;
}

remollGenPion::~remollGenPion(){
}

void remollGenPion::SamplePhysics(remollVertex *vert, remollEvent *evt){
    // Generate Pion event

    double beamE   = vert->GetBeamE();
    double rad_len = vert->GetRadLen();

    double th = acos(CLHEP::RandFlat::shoot(cos(fTh_max), cos(fTh_min)));
    double ph = CLHEP::RandFlat::shoot(0.0, 2.0*pi);
    double pf = CLHEP::RandFlat::shoot(0.0, beamE);

    double V = 2.0*pi*(cos(fTh_min) - cos(fTh_max))*beamE;

    double intrad = 2.0*alpha*log(beamE/electron_mass_c2)/pi;

    double sigpip = wiser_sigma(beamE/GeV, pf/GeV, th, rad_len*4.0/3.0 + intrad, 0)*nanobarn/GeV;
    double sigpim = wiser_sigma(beamE/GeV, pf/GeV, th, rad_len*4.0/3.0 + intrad, 1)*nanobarn/GeV;

    evt->SetEffCrossSection(V*(vert->GetMaterial()->GetZ()*sigpim + 
		(vert->GetMaterial()->GetA()*mole/g-vert->GetMaterial()->GetZ())*sigpip));

    if( vert->GetMaterial()->GetNumberOfElements() != 1 ){
	G4cerr << __FILE__ << " line " << __LINE__ << 
	    ": Error!  Some lazy programmer didn't account for complex materials in the moller process!" << G4endl;
	exit(1);
    }

    evt->SetAsymmetry(0.0);

    G4String piontypestr;

    switch(fPionType){
	case kPiMinus:
	    piontypestr = G4String("pi-");
	    break;
	case kPiPlus:
	    piontypestr = G4String("pi+");
	    break;
	default:
	    piontypestr = G4String("oops");
	    break;
    }

    evt->ProduceNewParticle( G4ThreeVector(0.0, 0.0, 0.0), 
	    G4ThreeVector(pf*sin(ph)*sin(th), pf*cos(ph)*sin(th), pf*cos(th)), 
	    piontypestr );
    return;

}
