#include "remollGenBeam.hh"

#include "remollEvent.hh"
#include "remollVertex.hh"
#include "remollBeamTarget.hh"

#include "G4Material.hh"
#include "G4VPhysicalVolume.hh"
#include "G4LogicalVolume.hh"
#include "G4PhysicalConstants.hh"
#include "G4GenericMessenger.hh"
#include "G4ParticleTable.hh"

#include "G4SystemOfUnits.hh"
#include "G4PhysicalConstants.hh"

#include "remolltypes.hh"

#include <math.h>

remollGenBeam::remollGenBeam()
: remollVEventGen("beam"),
    fXpos(0.0), fYpos(0.0),
    fZpos(-.5*m), fXmomentum(0.0),
    fYmomentum(0.0), fZmomentum(1.0),
    fParticleName("e-")
{
    fApplyMultScatt = true;

    fThisGenMessenger->DeclareMethod("x",&remollGenBeam::SetOriginX,"x coordinate of origin for the beam");
    fThisGenMessenger->DeclareMethod("y",&remollGenBeam::SetOriginY,"y coordinate of origin for the beam");
    fThisGenMessenger->DeclareMethod("z",&remollGenBeam::SetOriginZ,"z coordinate of origin for the beam");
    fThisGenMessenger->DeclareMethod("px",&remollGenBeam::SetMomentumX,"x component of momentum");
    fThisGenMessenger->DeclareMethod("py",&remollGenBeam::SetMomentumY,"y component of momentum");
    fThisGenMessenger->DeclareMethod("pz",&remollGenBeam::SetMomentumZ,"z component of momentum");
    fThisGenMessenger->DeclareMethod("partName",&remollGenBeam::SetPartName,"name of particle to shoot");
    
    //fZpos = -5.0*m;
}

remollGenBeam::~remollGenBeam() { }
void remollGenBeam::SetOriginX(double x){ fXpos = x; }
void remollGenBeam::SetOriginY(double y){ fYpos = y; }
void remollGenBeam::SetOriginZ(double z){ fZpos = z; }

void remollGenBeam::SetMomentumX(double px){ fXmomentum = px; }
void remollGenBeam::SetMomentumY(double py){ fYmomentum = py; }
void remollGenBeam::SetMomentumZ(double pz){ fZmomentum = pz; }

void remollGenBeam::SetPartName(G4String& name){ fParticleName = name; }

void remollGenBeam::SamplePhysics(remollVertex * /*vert*/, remollEvent *evt)
{

    G4ParticleTable* particleTable = G4ParticleTable::GetParticleTable();
    G4ParticleDefinition* particle = particleTable->FindParticle(fParticleName);

    // Get initial beam energy instead of using other sampling
    double E = fBeamTarg->fBeamEnergy;
    double m = particle->GetPDGMass();
    double p = sqrt(E*E - m*m);

    evt->fBeamE = E;
    evt->fBeamMomentum = p*G4ThreeVector(fXmomentum, fYmomentum, fZmomentum);

    // Override target sampling z
    evt->fVertexPos.setX( fXpos );
    evt->fVertexPos.setY( fYpos );
    evt->fVertexPos.setZ( fZpos );

    evt->ProduceNewParticle( G4ThreeVector(0.0, 0.0, 0.0), 
	    evt->fBeamMomentum, 
	    fParticleName);

    evt->SetEffCrossSection(0.0);
    evt->SetAsymmetry(0.0);

    evt->SetQ2(0.0);
    evt->SetW2(0.0);
}
