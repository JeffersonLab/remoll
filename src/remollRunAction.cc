#include "remollRunAction.hh"

#include "G4GenericMessenger.hh"
#include "G4RunManager.hh"
#include "G4Timer.hh"

#include "remollIO.hh"
#include "remollRun.hh"
#include "remollRunData.hh"
#include "remollBeamTarget.hh"

#include "G4Threading.hh"
#include "G4AutoLock.hh"
namespace { G4Mutex remollRunActionMutex = G4MUTEX_INITIALIZER; }

remollRunAction::remollRunAction()
: fInterval(10)
{
  // Create messenger to set the seed with single long int
  fMessenger = new G4GenericMessenger(this,"/remoll/","Remoll properties");
  fMessenger->DeclareMethod(
      "seed",
      &remollRunAction::UpdateSeed,
      "Set random engine seed")
      .SetParameterName("seed", false)
      .SetStates(G4State_PreInit,G4State_Idle);
  fMessenger->DeclareMethod(
      "interval",
      &remollRunAction::SetUpdateInterval,
      "Print this many progress points (i.e. 100 -> every 1%)")
      .SetParameterName("interval", false)
      .SetStates(G4State_PreInit,G4State_Idle);

  // Create timer
  fTimer = new G4Timer();
}

remollRunAction::~remollRunAction()
{
  delete fMessenger;
  delete fTimer;
}

void remollRunAction::UpdateSeed(const G4long seed)
{
  G4Random::setTheSeed(seed);
  remollRun::GetRunData()->SetSeed(seed);
  G4cout << "Random seed set to " << seed << G4endl;
}

G4Run* remollRunAction::GenerateRun()
{
  return new remollRun();
}

void remollRunAction::BeginOfRunAction(const G4Run* run)
{
  // Cast into remollRun
  const remollRun* aRun = static_cast<const remollRun*>(run);

  // Print progress
  G4int evts_to_process = aRun->GetNumberOfEventToBeProcessed();
  G4RunManager::GetRunManager()->SetPrintProgress((evts_to_process > fInterval)
                                                  ? evts_to_process/ fInterval
                                                  : 1);

  if (IsMaster())
  {
    G4cout << "### Run " << aRun->GetRunID() << " start." << G4endl;

    fTimer->Start();

    G4AutoLock lock(&remollRunActionMutex);
    remollIO* io = remollIO::GetInstance();
    io->InitializeTree();

    remollRunData *rundata = remollRun::GetRunData();
    rundata->SetNthrown( aRun->GetNumberOfEventToBeProcessed() );
  }
}

void remollRunAction::EndOfRunAction(const G4Run* run)
{
  // Cast into remollRun
  const remollRun* aRun = static_cast<const remollRun*>(run);

  if (IsMaster())
  {
      fTimer->Stop();

      G4cout << "### Run " << aRun->GetRunID() << " ended "
             << "(" << fTimer->GetUserElapsed() << "s)." << G4endl;

      G4AutoLock lock(&remollRunActionMutex);
      remollIO* io = remollIO::GetInstance();
      io->WriteTree();
  }
}

