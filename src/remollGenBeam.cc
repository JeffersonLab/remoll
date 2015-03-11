#include "remollGenBeam.hh"

#include "CLHEP/Random/RandFlat.h"

#include "remollEvent.hh"
#include "remollVertex.hh"
#include "remollBeamTarget.hh"

#include "G4Material.hh"
#include "G4VPhysicalVolume.hh"
#include "G4LogicalVolume.hh"
#include "G4PhysicalConstants.hh"
#include "Randomize.hh"

#include "remolltypes.hh"

#include <math.h>

remollGenBeam::remollGenBeam(){
    fApplyMultScatt = false;
    fBeamTarg = remollBeamTarget::GetBeamTarget();

    fZpos = -5.0*m;
}

remollGenBeam::~remollGenBeam(){
}

void remollGenBeam::SamplePhysics(remollVertex *vert, remollEvent *evt){
    // Get initial beam energy instead of using other sampling
    double beamE = fBeamTarg->fBeamE;
    evt->fBeamE = beamE;
    evt->fBeamMomentum = evt->fBeamMomentum.unit()*sqrt(beamE*beamE - electron_mass_c2*electron_mass_c2);;

    // Override target sampling z
    evt->fVertexPos.setZ( fZpos );

    evt->ProduceNewParticle( G4ThreeVector(0.0, 0.0, 0.0), 
	    evt->fBeamMomentum, 
	    "e-" );

    evt->SetEffCrossSection(0.0);
    evt->SetAsymmetry(0.0);

    evt->SetQ2(0.0);
    evt->SetW2(0.0);

    return;

}
