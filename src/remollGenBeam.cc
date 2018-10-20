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
  fXpos(0.0), fYpos(0.0), fZpos(-.5*m),
  fXdir(0.0), fYdir(0.0), fZdir(1.0),
  fParticleName("e-")
{
    fSampType = kNoTargetVolume;
    fApplyMultScatt = true;

    fThisGenMessenger->DeclareMethodWithUnit("origin","mm",&remollGenBeam::SetOrigin,"set origin");
    fThisGenMessenger->DeclareMethodWithUnit("x","mm",&remollGenBeam::SetOriginX,"x coordinate of origin for the beam");
    fThisGenMessenger->DeclareMethodWithUnit("y","mm",&remollGenBeam::SetOriginY,"y coordinate of origin for the beam");
    fThisGenMessenger->DeclareMethodWithUnit("z","mm",&remollGenBeam::SetOriginZ,"z coordinate of origin for the beam");
    fThisGenMessenger->DeclareMethod("direction",&remollGenBeam::SetDirection,"set momentum direction");
    fThisGenMessenger->DeclareMethod("px",&remollGenBeam::SetDirectionX,"x component of momentum direction");
    fThisGenMessenger->DeclareMethod("py",&remollGenBeam::SetDirectionY,"y component of momentum direction");
    fThisGenMessenger->DeclareMethod("pz",&remollGenBeam::SetDirectionZ,"z component of momentum direction");
    fThisGenMessenger->DeclareMethod("polarization",&remollGenBeam::SetPolarization,"set polarization");
    fThisGenMessenger->DeclareMethod("sx",&remollGenBeam::SetPolarizationX,"x component of polarization");
    fThisGenMessenger->DeclareMethod("sy",&remollGenBeam::SetPolarizationY,"y component of polarization");
    fThisGenMessenger->DeclareMethod("sz",&remollGenBeam::SetPolarizationZ,"z component of polarization");
    fThisGenMessenger->DeclareMethod("partName",&remollGenBeam::SetPartName,"name of particle to shoot");
}

remollGenBeam::~remollGenBeam() { }

void remollGenBeam::SetOrigin(G4ThreeVector origin){ fXpos = origin.x(); fYpos = origin.y(); fZpos = origin.z(); }
void remollGenBeam::SetOriginX(double x){ fXpos = x; }
void remollGenBeam::SetOriginY(double y){ fYpos = y; }
void remollGenBeam::SetOriginZ(double z){ fZpos = z; }

void remollGenBeam::SetDirection(G4ThreeVector direction){ fXdir = direction.x(); fYdir = direction.y(); fZdir = direction.z(); }
void remollGenBeam::SetDirectionX(double px){ fXdir = px; }
void remollGenBeam::SetDirectionY(double py){ fYdir = py; }
void remollGenBeam::SetDirectionZ(double pz){ fZdir = pz; }

void remollGenBeam::SetPolarization(G4ThreeVector polarization){ fXpol = polarization.x(); fYpol = polarization.y(); fZpol = polarization.z(); }
void remollGenBeam::SetPolarizationX(double sx){ fXpol = sx; }
void remollGenBeam::SetPolarizationY(double sy){ fYpol = sy; }
void remollGenBeam::SetPolarizationZ(double sz){ fZpol = sz; }

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
    evt->fBeamMomentum = p * G4ThreeVector(fXdir,fYdir,fZdir).unit();
    evt->fBeamPolarization = G4ThreeVector(fXpol, fYpol, fZpol);

    // Override target sampling z
    evt->fVertexPos.setX( fXpos );
    evt->fVertexPos.setY( fYpos );
    evt->fVertexPos.setZ( fZpos );

    evt->ProduceNewParticle(
	    G4ThreeVector(0.0, 0.0, 0.0),
	    evt->fBeamMomentum,
	    fParticleName,
            evt->fBeamPolarization);

    evt->SetEffCrossSection(0.0);
    evt->SetAsymmetry(0.0);

    evt->SetQ2(0.0);
    evt->SetW2(0.0);
}
