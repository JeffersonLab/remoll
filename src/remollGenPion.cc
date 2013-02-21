#include "remollGenPion.hh"

#include "G4String.hh"
#include "CLHEP/Random/RandFlat.h"

#include "wiser_pion.h"
#include "remollEvent.hh"
#include "remollVertex.hh"
#include "G4Material.hh"

#include "remolltypes.hh"

remollGenPion::remollGenPion(){
    fApplyMultScatt = false;

    fThMin = 0.01*deg;
    fThMax = 5.0*deg;
    fEmin  = 80*MeV;

    fPionType = kPiMinus;
}

remollGenPion::~remollGenPion(){
}

void remollGenPion::SamplePhysics(remollVertex *vert, remollEvent *evt){
    // Generate Pion event

    double beamE   = vert->GetBeamE();
    double rad_len = vert->GetRadLen();

    double th = acos(CLHEP::RandFlat::shoot(cos(fThMax), cos(fThMin)));
    double ph = CLHEP::RandFlat::shoot(0.0, 2.0*pi);
    double pf = CLHEP::RandFlat::shoot(0.0, beamE);

    double V = 2.0*pi*(cos(fThMin) - cos(fThMax));

    double sigpip = wiser_sigma(beamE/GeV, pf/GeV, th, rad_len + 0.05, 0)*nanobarn/GeV;
    double sigpim = wiser_sigma(beamE/GeV, pf/GeV, th, rad_len + 0.05, 1)*nanobarn/GeV;

    evt->SetEffCrossSection(V*(vert->GetMaterial()->GetZ()*sigpip + 
		(vert->GetMaterial()->GetA()*mole/g-vert->GetMaterial()->GetZ())*sigpim));

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
