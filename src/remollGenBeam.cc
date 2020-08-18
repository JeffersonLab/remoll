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
  fOriginMean(0.0*m,0.0*m,-19.81*m),
  fOriginSpread(0.0,0.0,0.0),
  fOriginModelX(kOriginModelFlat),
  fOriginModelY(kOriginModelFlat),
  fOriginModelZ(kOriginModelFlat),
  fDirection(0.0,0.0,1.0),
  fCorrelation(0.0653*mrad/mm,0.0653*mrad/mm,0.0),
  fPolarization(0.0,0.0,0.0),
  fRaster(5*mm,5*mm,0.0),
  fRasterRefZ(-4.5*m),
  fParticleName("e-")
{
    fSampType = kNoTargetVolume;
    fApplyMultScatt = true;

    fThisGenMessenger->DeclarePropertyWithUnit("origin","mm",fOriginMean,"origin position mean: x y z unit");
    fThisGenMessenger->DeclareMethodWithUnit("x","mm",&remollGenBeam::SetOriginXMean,"origin x position mean");
    fThisGenMessenger->DeclareMethodWithUnit("y","mm",&remollGenBeam::SetOriginYMean,"origin y position mean");
    fThisGenMessenger->DeclareMethodWithUnit("z","mm",&remollGenBeam::SetOriginZMean,"origin z position mean");

    fThisGenMessenger->DeclarePropertyWithUnit("originspread","mm",fOriginSpread,"origin position spread: x y z unit");
    fThisGenMessenger->DeclareMethodWithUnit("xspread","mm",&remollGenBeam::SetOriginXSpread,"origin x position spread");
    fThisGenMessenger->DeclareMethodWithUnit("yspread","mm",&remollGenBeam::SetOriginYSpread,"origin y position spread");
    fThisGenMessenger->DeclareMethodWithUnit("zspread","mm",&remollGenBeam::SetOriginZSpread,"origin z position spread");
    fThisGenMessenger->DeclareMethod("xmodel",&remollGenBeam::SetOriginXModel,"origin x position model: flat, gauss");
    fThisGenMessenger->DeclareMethod("ymodel",&remollGenBeam::SetOriginYModel,"origin y position model: flat, gauss");
    fThisGenMessenger->DeclareMethod("zmodel",&remollGenBeam::SetOriginZModel,"origin z position model: flat, gauss");

    fThisGenMessenger->DeclareProperty("direction",fDirection,"direction vector (will be normalized): x y z");
    fThisGenMessenger->DeclareMethod("px",&remollGenBeam::SetDirectionX,"direction x (vector will be normalized before use)");
    fThisGenMessenger->DeclareMethod("py",&remollGenBeam::SetDirectionY,"direction y (vector will be normalized before use)");
    fThisGenMessenger->DeclareMethod("pz",&remollGenBeam::SetDirectionZ,"direction z (vector will be normalized before use)");
    fThisGenMessenger->DeclareMethodWithUnit("th","deg",&remollGenBeam::SetDirectionTh,"direction vector theta angle");
    fThisGenMessenger->DeclareMethodWithUnit("ph","deg",&remollGenBeam::SetDirectionPh,"direction vector phi angle");

    fThisGenMessenger->DeclareProperty("polarization",fPolarization,"polarization vector (will be normalized): x y z");
    fThisGenMessenger->DeclareMethod("sx",&remollGenBeam::SetPolarizationX,"x component of polarization");
    fThisGenMessenger->DeclareMethod("sy",&remollGenBeam::SetPolarizationY,"y component of polarization");
    fThisGenMessenger->DeclareMethod("sz",&remollGenBeam::SetPolarizationZ,"z component of polarization");

    fThisGenMessenger->DeclareMethodWithUnit("rasrefz","mm",&remollGenBeam::SetRasterRefZ,"reference z position where raster is defined");
    fThisGenMessenger->DeclareMethodWithUnit("rasx","mm",&remollGenBeam::SetRasterX,"raster x spread perpendicular to the beam at z = 0");
    fThisGenMessenger->DeclareMethodWithUnit("rasy","mm",&remollGenBeam::SetRasterY,"raster y spread perpendicular to the beam at z = 0");
    fThisGenMessenger->DeclareMethod("corrx",&remollGenBeam::SetCorrelationX,"sensitivity of direction to position in x (in mrad/mm)");
    fThisGenMessenger->DeclareMethod("corry",&remollGenBeam::SetCorrelationY,"sensitivity of direction to position in y (in mrad/mm)");

    fThisGenMessenger->DeclareMethod("partName",&remollGenBeam::SetPartName,"name of particle to shoot");
}

remollGenBeam::~remollGenBeam() { }

void remollGenBeam::SetOriginXMean(double x){ fOriginMean.setX(x); }
void remollGenBeam::SetOriginYMean(double y){ fOriginMean.setY(y); }
void remollGenBeam::SetOriginZMean(double z){ fOriginMean.setZ(z); }

void remollGenBeam::SetOriginXSpread(double x){ fOriginSpread.setX(x); }
void remollGenBeam::SetOriginYSpread(double y){ fOriginSpread.setY(y); }
void remollGenBeam::SetOriginZSpread(double z){ fOriginSpread.setZ(z); }

remollGenBeam::EOriginModel remollGenBeam::GetOriginModelFromString(G4String model) const {
  std::transform(model.begin(), model.end(), model.begin(), ::tolower);
  if (model == "flat")  return kOriginModelFlat;
  if (model == "gauss") return kOriginModelGauss;
  G4cerr << "remollGenBeam: did not recognize model, assuming flat." << G4endl;
  return kOriginModelFlat;
}
void remollGenBeam::SetOriginXModel(G4String x){ fOriginModelX = GetOriginModelFromString(x); }
void remollGenBeam::SetOriginYModel(G4String y){ fOriginModelY = GetOriginModelFromString(y); }
void remollGenBeam::SetOriginZModel(G4String z){ fOriginModelZ = GetOriginModelFromString(z); }

void remollGenBeam::SetRasterX(double x){ fRaster.setX(x); }
void remollGenBeam::SetRasterY(double y){ fRaster.setY(y); }
void remollGenBeam::SetRasterRefZ(double z){ fRasterRefZ = z; }

void remollGenBeam::SetDirectionX(double px){ fDirection.setX(px); }
void remollGenBeam::SetDirectionY(double py){ fDirection.setY(py); }
void remollGenBeam::SetDirectionZ(double pz){ fDirection.setZ(pz); }
void remollGenBeam::SetDirectionPh(double ph){ fDirection.setPhi(ph); }
void remollGenBeam::SetDirectionTh(double th){ fDirection.setTheta(th); }

void remollGenBeam::SetCorrelationX(double cx){ fCorrelation.setX(cx * mrad/mm); }
void remollGenBeam::SetCorrelationY(double cy){ fCorrelation.setY(cy * mrad/mm); }

void remollGenBeam::SetPolarizationX(double sx){ fPolarization.setX(sx); }
void remollGenBeam::SetPolarizationY(double sy){ fPolarization.setY(sy); }
void remollGenBeam::SetPolarizationZ(double sz){ fPolarization.setZ(sz); }

void remollGenBeam::SetPartName(G4String& name){ fParticleName = name; }

G4double remollGenBeam::GetSpread(G4double spread, EOriginModel model)
{
  if (model == kOriginModelFlat)
    return G4RandFlat::shoot(-spread/2.0, +spread/2.0);
  if (model == kOriginModelGauss)
    return G4RandGauss::shoot(0.0, spread);
  else return 0.0;
}

G4ThreeVector remollGenBeam::GetSpread(G4ThreeVector spread,
  EOriginModel x, EOriginModel y, EOriginModel z)
{
  G4ThreeVector sample(0.0,0.0,0.0);
  sample.setX(GetSpread(spread.x(), x));
  sample.setY(GetSpread(spread.y(), y));
  sample.setZ(GetSpread(spread.z(), z));
  return sample;
}

void remollGenBeam::SamplePhysics(remollVertex * /*vert*/, remollEvent *evt)
{
    G4ParticleTable* particleTable = G4ParticleTable::GetParticleTable();
    G4ParticleDefinition* particle = particleTable->FindParticle(fParticleName);

    // Get initial beam energy instead of using other sampling
    double m = particle->GetPDGMass();
    double E = fBeamTarg->fBeamEnergy + m;
    double p = sqrt(E*E - m*m);

    // Start from mean position
    G4ThreeVector origin(fOriginMean);

    // Start from mean direction
    G4ThreeVector direction(fDirection.unit());

    // Add a spread based on chosen model
    G4ThreeVector spread = GetSpread(fOriginSpread, fOriginModelX, fOriginModelY, fOriginModelZ);

    // Allow for simplistic raster/spreading in beam generator, perpendicular to direction
    G4ThreeVector raster_perpto_direction(fRaster);
    raster_perpto_direction.rotateUz(fDirection.unit());
    G4ThreeVector raster = GetSpread(raster_perpto_direction);

    // Rotate direction for position-angle correlation (maintains unit vector)
    direction.rotateY(+ fCorrelation.x() * raster.x()); // Rotate around Y by X amount
    direction.rotateX(- fCorrelation.y() * raster.y()); // Rotate around X by Y amount

    // Project raster back to origin
    raster.setX(raster.x() + direction.x() * (origin.z() - fRasterRefZ));
    raster.setY(raster.y() + direction.y() * (origin.z() - fRasterRefZ));

    // Add spreads to origin
    origin += raster;
    origin += spread;

    // Override target sampling
    evt->fBeamE = E;
    evt->fBeamMomentum = p * direction;
    evt->fBeamPolarization = fPolarization;
    evt->fVertexPos = origin; // primary vertex

    evt->ProduceNewParticle(
        G4ThreeVector(0.0,0.0,0.0), // relative position to primary vertex
        evt->fBeamMomentum,
        fParticleName,
        evt->fBeamPolarization);

    evt->SetEffCrossSection(0.0);
    evt->SetAsymmetry(0.0);

    evt->SetQ2(0.0);
    evt->SetW2(0.0);
}
