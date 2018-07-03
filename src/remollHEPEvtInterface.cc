#include "remollHEPEvtInterface.hh"
#include "remollHEPEvtMessenger.hh"

#include "G4Event.hh"
#include "G4HEPEvtInterface.hh"
#include "G4AutoLock.hh"

namespace {
 G4Mutex remollHEPEvtInterfaceMutex = G4MUTEX_INITIALIZER;
}

G4VPrimaryGenerator* remollHEPEvtInterface::fHEPEvtInterface = 0;

remollHEPEvtInterface::remollHEPEvtInterface()
: fVerbose(0), fFilename("hepevt.dat")
{
  Initialize();
  fMessenger = new remollHEPEvtMessenger(this);
}

remollHEPEvtInterface::~remollHEPEvtInterface()
{
  G4AutoLock lock(&remollHEPEvtInterfaceMutex);
  if(fHEPEvtInterface) { delete fHEPEvtInterface; fHEPEvtInterface = 0; }
  delete fMessenger;
}

void remollHEPEvtInterface::Initialize()
{
  G4AutoLock lock(&remollHEPEvtInterfaceMutex);
  if (fHEPEvtInterface) { delete fHEPEvtInterface; fHEPEvtInterface = 0; }
  fHEPEvtInterface = new G4HEPEvtInterface(fFilename,1);
}

void remollHEPEvtInterface::GeneratePrimaryVertex(G4Event* anEvent)
{
  G4AutoLock lock(&remollHEPEvtInterfaceMutex);
  fHEPEvtInterface->GeneratePrimaryVertex(anEvent);
}
