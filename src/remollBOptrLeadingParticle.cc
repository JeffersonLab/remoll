#include "remollBOptrLeadingParticle.hh"
#include "G4BiasingProcessInterface.hh"

#include "G4BOptnLeadingParticle.hh"
#include "G4ParticleDefinition.hh"
#include "G4Gamma.hh"
#include "G4Electron.hh"
#include "G4Positron.hh"
#include "G4PionZero.hh"
#include "G4ProcessManager.hh"


//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

remollBOptrLeadingParticle::remollBOptrLeadingParticle( G4String operatorName )
  : G4VBiasingOperator        ( operatorName ),
    fAnnihilation             ( nullptr ),
    fConversion               ( nullptr ),
    fDecay                    ( nullptr ),
    fTwoParticleProcess       ( nullptr )
{
  fLeadingParticleBiasingOperation =
    new G4BOptnLeadingParticle("LeadingParticleBiasingOperation");
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

remollBOptrLeadingParticle::~remollBOptrLeadingParticle()
{
  delete fLeadingParticleBiasingOperation;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

G4VBiasingOperation*
remollBOptrLeadingParticle::
ProposeFinalStateBiasingOperation(const G4Track*,
                                  const G4BiasingProcessInterface* callingProcess)
{
  // --   When the present method is called, we are at the process final state
  // -- generation level. The process is given by the callingProcess argument,
  // -- which, in our case,  wrappes a physics process, to control it.
  // --   To bias the final state generation, we return a biasing operation
  // -- which is fLeadingParticleBiasingOperation here. Before returning it, we
  // -- configure it depending on if the process is a two-particle final state
  // -- or if it is a many-particle final state process. For the two-particle
  // -- final state, one track is the leading and the other is alone in its category,
  // -- so always surviving by default. We play a Russian roulette on it to
  // -- trim also these two-particles final states.

  if ( callingProcess == fTwoParticleProcess )
    {
      // -- secondary particle accompagnying the leading one will be
      // -- killed with 2./3. probability (Russian roulette):
      fLeadingParticleBiasingOperation->SetFurtherKillingProbability( 2./3.);
    }
  else
    {
      // -- -1.0 means no effect : no further killing is applied to secondary
      // -- particles accompanying the leading one.
      fLeadingParticleBiasingOperation->SetFurtherKillingProbability( -1.0 );
    }

  return fLeadingParticleBiasingOperation;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

void
remollBOptrLeadingParticle::
StartRun()
{
  // -- collect the two-particle final state processes:
  fAnnihilation = nullptr;
  fConversion   = nullptr;
  fDecay        = nullptr;

  // ---- collect e+ annihilation process:
  auto positronProcesses = G4Positron::Definition()->GetProcessManager()->GetProcessList();
  for ( size_t i = 0; i < positronProcesses->size(); ++i )
    {
      if ( (*positronProcesses)[i]->GetProcessName() == "biasWrapper(annihil)")
        {
          fAnnihilation = (*positronProcesses)[i];
          break;
        }
    }

  // ---- collect gamma conversion process:
  auto gammaProcesses = G4Gamma::Definition()->GetProcessManager()->GetProcessList();
  for ( size_t i = 0; i < gammaProcesses->size(); ++i )
    {
      if ( (*gammaProcesses)[i]->GetProcessName() == "biasWrapper(conv)")
        {
          fConversion = (*gammaProcesses)[i];
          break;
        }
    }

  // ---- collect pi0 decay process:
  auto pi0Processes = G4PionZero::Definition()->GetProcessManager()->GetProcessList();
  for ( size_t i = 0; i < pi0Processes->size(); ++i )
    {
      if ( (*pi0Processes)[i]->GetProcessName() == "biasWrapper(Decay)")
        {
          fDecay = (*pi0Processes)[i];
          break;
        }
    }
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

void
remollBOptrLeadingParticle::
StartTracking( const G4Track* track )
{
  // -- remember what is the two-particle final state process -if any- for this starting
  // -- track:
  fTwoParticleProcess = nullptr;
  if ( track->GetDefinition() == G4Gamma   ::Definition() ) fTwoParticleProcess =   fConversion;
  if ( track->GetDefinition() == G4Positron::Definition() ) fTwoParticleProcess = fAnnihilation;
  if ( track->GetDefinition() == G4PionZero::Definition() ) fTwoParticleProcess =        fDecay;
}
