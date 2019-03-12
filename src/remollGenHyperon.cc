#include "remollGenHyperon.hh"

// Geant4 includes
#include "G4Material.hh"
#include "G4GenericMessenger.hh"

// remoll includes
#include "remollBeamTarget.hh"
#include "remollEvent.hh"
#include "remollVertex.hh"
#include "remolltypes.hh"
#include "remollRun.hh"

// System includes
#include <iostream>
#include <sstream>
#include <iterator>
#include <string>

#include "G4AutoLock.hh"
namespace { G4Mutex remollGenHyperonMutex = G4MUTEX_INITIALIZER; }
remollFileReader* remollGenHyperon::fFileReader = 0;

remollGenHyperon::remollGenHyperon()
: remollVEventGen("hyperon"),fFile(""),fParticle("pi-"),fRUnit(cm),fPUnit(GeV)
{
  // Add to generic messenger
  fThisGenMessenger->DeclareProperty("file",fFile,"Input filename");
  fThisGenMessenger->DeclareProperty("skip",fSkip,"Number of lines to skip");
  fThisGenMessenger->DeclareProperty("particle",fParticle,"Particle name");
  fThisGenMessenger->DeclarePropertyWithUnit("runit","cm",fRUnit,"Units of position");
  fThisGenMessenger->DeclarePropertyWithUnit("punit","GeV",fPUnit,"Units of momentum");
}

remollGenHyperon::~remollGenHyperon()
{
  G4AutoLock lock(&remollGenHyperonMutex);
  if (fFileReader) { delete fFileReader; fFileReader = 0; }
}

void remollGenHyperon::SamplePhysics(remollVertex* /*vert*/, remollEvent* evt)
{
  G4ThreeVector r(0,0,0), p(0,0,0);
  G4double w = 0;

  // Limit scope of mutex to read from buffered file
  if (fFileReader) {
    G4AutoLock lock(&remollGenHyperonMutex);
    remollFileEvent event = fFileReader->GetAnEvent();
    r = event.GetR();
    p = event.GetP();
    w = event.GetW();
  }

  // Create event
  evt->SetRate(w);
  evt->SetAsymmetry(0.0);
  evt->SetEffCrossSection(1.0);
  evt->ProduceNewParticle(r * fRUnit, p * fPUnit, fParticle);
}
