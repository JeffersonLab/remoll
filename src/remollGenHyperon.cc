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
: remollVEventGen("hyperon"),
  fDebugLevel(0),
  fFile("generators/aniol/hyperon_outp.dat"),fParticle("lambda"),
  fRUnit(cm),fPUnit(GeV),fWUnit(barn)
{
  // Add to generic messenger
  fThisGenMessenger->DeclareProperty("debug",fDebugLevel,"Debug level");
  fThisGenMessenger->DeclareProperty("file",fFile,"Input filename");
  fThisGenMessenger->DeclareProperty("skip",fSkip,"Number of lines to skip");
  fThisGenMessenger->DeclareProperty("particle",fParticle,"Particle name");
  fThisGenMessenger->DeclarePropertyWithUnit("runit","cm",fRUnit,"Units of position");
  fThisGenMessenger->DeclarePropertyWithUnit("punit","GeV",fPUnit,"Units of momentum");
  fThisGenMessenger->DeclarePropertyWithUnit("wunit","barn",fWUnit,"Units of weight");
}

remollGenHyperon::~remollGenHyperon()
{
  G4AutoLock lock(&remollGenHyperonMutex);
  if (fFileReader) { delete fFileReader; fFileReader = 0; }
}

remollFileReader* remollGenHyperon::GetFileReader() const
{
  G4AutoLock lock(&remollGenHyperonMutex);
  if (! fFileReader) {
    fFileReader = new remollFileReader(fFile,fSkip,fDebugLevel);
  }
  return fFileReader;
}

void remollGenHyperon::SamplePhysics(remollVertex* /*vert*/, remollEvent* evt)
{
  remollFileEvent event;

  // Limit scope of mutex to read from buffered file
  if (GetFileReader()) {
    G4AutoLock lock(&remollGenHyperonMutex);
    event = fFileReader->GetAnEvent(); // don't use GetFileReader, race condition
  }

  // Create event
  evt->SetRate(event.w() / fWUnit);
  evt->SetAsymmetry(0.0);
  evt->SetEffCrossSection(1.0);
  evt->ProduceNewParticle(event.r() / fRUnit, event.p() / fPUnit, fParticle);
}
