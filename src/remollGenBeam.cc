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
: remollVEventGen("beam")
{
    fApplyMultScatt = false;

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

    G4ThreeVector pos(0.0, 0.0, 0.0);
    G4ThreeVector mom = evt->fBeamMomentum;
    static G4ParticleDefinition* particle =
        G4ParticleTable::GetParticleTable()->FindParticle("e-");
    evt->ProduceNewParticle(pos,mom,particle);

    // No cross section, asymmetry, or kinematic variables
    evt->SetEffCrossSection(0.0);
    evt->SetAsymmetry(0.0);
    evt->SetQ2(0.0);
    evt->SetW2(0.0);
}
