
// Make this appear first!
#include "G4Timer.hh"

#include "remollRunAction.hh"
#include "remollBeamTarget.hh"
#include "G4Run.hh"
#include "G4UImanager.hh"
#include "G4ios.hh"

#include "G4RunManager.hh"

#include "remollIO.hh"
#include "remollRun.hh"

#include "G4AutoLock.hh"
namespace { G4Mutex remollIOMutex = G4MUTEX_INITIALIZER; }

remollRunAction::remollRunAction() { }

remollRunAction::~remollRunAction() { }
{
  delete timer;
}

void remollRunAction::BeginOfRunAction(const G4Run* aRun)
{
  G4cout << "### Run " << aRun->GetRunID() << " start." << G4endl;

  G4AutoLock lock(&remollIOMutex);
  remollIO* io = remollIO::GetInstance();
  io->InitializeTree();
  // Print progress
  G4int evts_to_process = aRun->GetNumberOfEventToBeProcessed();
  G4RunManager::GetRunManager()->SetPrintProgress((evts_to_process > 100)
                                                  ? evts_to_process/100
                                                  : 1);

  remollRunData *rmrundata = remollRun::GetInstance()->GetData();

  rmrundata->SetBeamE( remollBeamTarget::GetBeamTarget()->fBeamE/GeV );
  rmrundata->SetNthrown( aRun->GetNumberOfEventToBeProcessed() );

  rmrundata->Print();
}

void remollRunAction::EndOfRunAction(const G4Run* aRun)
{

  G4AutoLock lock(&remollIOMutex);
  remollIO* io = remollIO::GetInstance();
  io->WriteTree();
}

