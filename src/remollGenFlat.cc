#include "remollGenFlat.hh"

#include "Randomize.hh"

#include "G4Material.hh"
#include "G4PhysicalConstants.hh"

#include "remollEvent.hh"
#include "remollVertex.hh"
#include "remolltypes.hh"

remollGenFlat::remollGenFlat()
: remollVEventGen("flat") {
    fTh_min =     0.0*deg;
    fTh_max =     5.0*deg;

    fParticleName = "e-";

    fApplyMultScatt = true;
}

remollGenFlat::~remollGenFlat(){
}

void remollGenFlat::SamplePhysics(remollVertex *vert, remollEvent *evt){
    // Generate event flat in phase space

    double beamE = vert->GetBeamEnergy();

    double mp = 0.938*GeV;

    double th = acos(G4RandFlat::shoot(cos(fTh_max), cos(fTh_min)));
    double ph = G4RandFlat::shoot(fPh_min, fPh_max);
    double ef = G4RandFlat::shoot(fE_min, fE_max);

    evt->SetEffCrossSection(1);

    if( vert->GetMaterial()->GetNumberOfElements() != 1 ){
	G4cerr << __FILE__ << " line " << __LINE__ << 
	    ": Error!  Some lazy programmer didn't account for complex materials in the moller process!" << G4endl;
	exit(1);
    }


    double Q2 = 2.0*beamE*ef*(1.0-cos(th));
    evt->SetQ2( Q2 );

    G4double APV = Q2*0.8e-4/GeV/GeV; // Empirical APV value, 
                                      // stolen from mollerClass.C in mollersim

    evt->SetAsymmetry(APV);


    evt->SetW2( mp*mp + 2.0*mp*(beamE-ef) - Q2 );

    evt->ProduceNewParticle(
        G4ThreeVector(0.0, 0.0, 0.0),
        G4ThreeVector(ef*sin(th)*cos(ph), ef*sin(th)*sin(ph), ef*cos(th) ),
        fParticleName);

}
