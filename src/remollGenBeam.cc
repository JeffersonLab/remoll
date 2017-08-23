#include "remollGenBeam.hh"

#include "remollEvent.hh"
#include "remollVertex.hh"
#include "remollBeamTarget.hh"

#include "G4Material.hh"
#include "G4VPhysicalVolume.hh"
#include "G4LogicalVolume.hh"
#include "G4PhysicalConstants.hh"

#include "remolltypes.hh"

#include <math.h>

remollGenBeam::remollGenBeam()
: remollVEventGen("beam") {
    fApplyMultScatt = true;

    fZpos = -5.0*m;
}

remollGenBeam::~remollGenBeam() { }

void remollGenBeam::SamplePhysics(remollVertex * /*vert*/, remollEvent *evt)
{
    // Get initial beam energy instead of using other sampling
    double E = fBeamTarg->fBeamEnergy;
    double m = electron_mass_c2;
    double p = sqrt(E*E - m*m);

    evt->fBeamE = E;
    evt->fBeamMomentum = evt->fBeamMomentum.unit()*p;

    // Override target sampling z
    evt->fVertexPos.setZ( fZpos );

    evt->ProduceNewParticle( G4ThreeVector(0.0, 0.0, 0.0), 
	    evt->fBeamMomentum, 
	    "e-" );

    evt->SetEffCrossSection(0.0);
    evt->SetAsymmetry(0.0);

    evt->SetQ2(0.0);
    evt->SetW2(0.0);
}
