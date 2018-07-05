#include "remollGenBeam.hh"

#include "remollEvent.hh"
#include "remollVertex.hh"
#include "remollBeamTarget.hh"

#include "G4Material.hh"
#include "G4VPhysicalVolume.hh"
#include "G4LogicalVolume.hh"
#include "G4PhysicalConstants.hh"
#include "G4GenericMessenger.hh"
#include "remolltypes.hh"

#include <math.h>

remollGenBeam::remollGenBeam()
: remollVEventGen("beam"),
    fXpos(0.0), fYpos(0.0),
    fZpos(-5.0*m)
{
    fApplyMultScatt = true;
    
    fThisGenMessenger->DeclareMethod("x",&remollGenBeam::SetOriginX,"x coordinate of origin for the beam");
    fThisGenMessenger->DeclareMethod("y",&remollGenBeam::SetOriginY,"y coordinate of origin for the beam");
    fThisGenMessenger->DeclareMethod("z",&remollGenBeam::SetOriginZ,"z coordinate of origin for the beam");
 //   fZpos = -5.0*m;
}

remollGenBeam::~remollGenBeam() { }

void remollGenBeam::SetOriginX(double x){ fXpos = x; }
void remollGenBeam::SetOriginY(double y){ fYpos = y; }
void remollGenBeam::SetOriginZ(double z){ fZpos = z; }

void remollGenBeam::SamplePhysics(remollVertex * /*vert*/, remollEvent *evt)
{
    // Get initial beam energy instead of using other sampling
    double E = fBeamTarg->fBeamEnergy;
    double m = electron_mass_c2;
    double p = sqrt(E*E - m*m);

    evt->fBeamE = E;
    evt->fBeamMomentum = evt->fBeamMomentum.unit()*p;

    // Override target sampling 
    evt->fVertexPos.setX( fXpos );
    evt->fVertexPos.setY( fYpos );
    evt->fVertexPos.setZ( fZpos );

    evt->ProduceNewParticle( G4ThreeVector(0.0, 0.0, 0.0), 
	    evt->fBeamMomentum, 
	    "e-" );

    evt->SetEffCrossSection(0.0);
    evt->SetAsymmetry(0.0);

    evt->SetQ2(0.0);
    evt->SetW2(0.0);
}
