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

#include "Randomize.hh"
#include "G4SystemOfUnits.hh"
#include "G4PhysicalConstants.hh"

#include "remolltypes.hh"

#include <math.h>

remollGenBeam::remollGenBeam()
: remollVEventGen("beam"),
  fOrigin(0.0*m,0.0*m,-5.0*m),
  fDirection(0.0,0.0,1.0),
  //fPolarization(0.0,0.0,0.0),
  fXras(0.0), fYras(0.0),
  fParticleName("e-")
{
    fSampType = kNoTargetVolume;
    fApplyMultScatt = true;

    fThisGenMessenger->DeclarePropertyWithUnit("origin","mm",fOrigin,"set origin: x y z unit");
    fThisGenMessenger->DeclareMethodWithUnit("x","mm",&remollGenBeam::SetOriginX,"x coordinate of origin for the beam");
    fThisGenMessenger->DeclareMethodWithUnit("y","mm",&remollGenBeam::SetOriginY,"y coordinate of origin for the beam");
    fThisGenMessenger->DeclareMethodWithUnit("z","mm",&remollGenBeam::SetOriginZ,"z coordinate of origin for the beam");
    fThisGenMessenger->DeclareMethodWithUnit("rasx","mm",&remollGenBeam::SetRasterX,"Raster x spread of origin for the beam");
    fThisGenMessenger->DeclareMethodWithUnit("rasy","mm",&remollGenBeam::SetRasterY,"Raster y spread of origin for the beam");
    fThisGenMessenger->DeclareMethod("direction",&remollGenBeam::SetDirection,"set momentum direction: (x,y,z)");
    fThisGenMessenger->DeclareMethod("px",&remollGenBeam::SetDirectionX,"x component of momentum direction");
    fThisGenMessenger->DeclareMethod("py",&remollGenBeam::SetDirectionY,"y component of momentum direction");
    fThisGenMessenger->DeclareMethod("pz",&remollGenBeam::SetDirectionZ,"z component of momentum direction");
    fThisGenMessenger->DeclareMethodWithUnit("th","deg",&remollGenBeam::SetDirectionTh,"theta angle of momentum direction");
    fThisGenMessenger->DeclareMethodWithUnit("ph","deg",&remollGenBeam::SetDirectionPh,"phi angle of momentum direction");
    fThisGenMessenger->DeclareMethod("polarization",&remollGenBeam::SetPolarization,"set polarization: (x,y,z)");
    fThisGenMessenger->DeclareMethod("sx",&remollGenBeam::SetPolarizationX,"x component of polarization");
    fThisGenMessenger->DeclareMethod("sy",&remollGenBeam::SetPolarizationY,"y component of polarization");
    fThisGenMessenger->DeclareMethod("sz",&remollGenBeam::SetPolarizationZ,"z component of polarization");
    fThisGenMessenger->DeclareMethod("partName",&remollGenBeam::SetPartName,"name of particle to shoot");
}

remollGenBeam::~remollGenBeam() { }

void remollGenBeam::SetOriginX(double x){ fOrigin.setX(x); }
void remollGenBeam::SetOriginY(double y){ fOrigin.setY(y); }
void remollGenBeam::SetOriginZ(double z){ fOrigin.setZ(z); }

void remollGenBeam::SetRasterX(double RASx){ fXras = RASx; }
void remollGenBeam::SetRasterY(double RASy){ fYras = RASy; }

void remollGenBeam::SetDirection(G4ThreeVector direction){ fDirection = direction; }
void remollGenBeam::SetDirectionX(double px){ fDirection.setX(px); }
void remollGenBeam::SetDirectionY(double py){ fDirection.setY(py); }
void remollGenBeam::SetDirectionZ(double pz){ fDirection.setZ(pz); }
void remollGenBeam::SetDirectionPh(double ph){ fDirection.setPhi(ph); }
void remollGenBeam::SetDirectionTh(double th){ fDirection.setTheta(th); }

void remollGenBeam::SetPolarization(G4ThreeVector polarization){ fPolarization = polarization; }
void remollGenBeam::SetPolarizationX(double sx){ fPolarization.setX(sx); }
void remollGenBeam::SetPolarizationY(double sy){ fPolarization.setY(sy); }
void remollGenBeam::SetPolarizationZ(double sz){ fPolarization.setZ(sz); }

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
    evt->fBeamMomentum = p * fDirection.unit();
    evt->fBeamPolarization = fPolarization;

    // Override target sampling z
    evt->fVertexPos.setX( 0.0 );
    evt->fVertexPos.setY( 0.0 );
    evt->fVertexPos.setZ( 0.0 );

    // Allow for simplistic raster/spreading in beam generator
    G4double rasx = G4RandFlat::shoot(fOrigin.x() - fXras/2.0, fOrigin.x() + fXras/2.0);
    G4double rasy = G4RandFlat::shoot(fOrigin.y() - fYras/2.0, fOrigin.y() + fYras/2.0);
    G4double rasz = fOrigin.z();

    evt->ProduceNewParticle(
	    G4ThreeVector(rasx, rasy, rasz),
	    evt->fBeamMomentum,
	    fParticleName,
        evt->fBeamPolarization);

    evt->SetEffCrossSection(0.0);
    evt->SetAsymmetry(0.0);

    evt->SetQ2(0.0);
    evt->SetW2(0.0);
}
