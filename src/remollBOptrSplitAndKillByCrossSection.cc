#include "remollBOptrSplitAndKillByCrossSection.hh"
#include "G4BiasingProcessInterface.hh"
#include "remollBOptnSplitAndKillByCrossSection.hh"

#include "G4ParticleDefinition.hh"
#include "G4ParticleTable.hh"
#include "G4VProcess.hh"

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

remollBOptrSplitAndKillByCrossSection::
remollBOptrSplitAndKillByCrossSection(G4String particleName,
                                      G4bool   forwardSplit,
                                      G4String         name)
  : G4VBiasingOperator(name),
    fForwardSplit(forwardSplit),
    fSetup(true)
{
  fParticleToBias = G4ParticleTable::GetParticleTable()->FindParticle(particleName);

  if ( fParticleToBias == 0 )
    {
      G4ExceptionDescription ed;
      ed << "Particle `" << particleName << "' not found !" << G4endl;
      G4Exception("remollBOptrSplitAndKillByCrossSection(...)",
                  "exremoll.01",
                  JustWarning,
                  ed);
    }

  fSplitAndKillByCrossSection =
    new remollBOptnSplitAndKillByCrossSection("splitterFor_"+particleName, forwardSplit);

}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

remollBOptrSplitAndKillByCrossSection::
~remollBOptrSplitAndKillByCrossSection()
{
  delete fSplitAndKillByCrossSection;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

void
remollBOptrSplitAndKillByCrossSection::
StartRun()
{
  // ---------------
  // -- Setup stage:
  // ---------------
  // -- Start by collecting the pointer of the physics processes
  // -- considered for the splitting by cross-sections. Doing so,
  // -- this also verifies that these physics processes are each
  // -- under control of a G4BiasingProcessInterface wrapper.
  if ( fSetup )
    {
      const G4ProcessManager* processManager = fParticleToBias->GetProcessManager();
      const G4BiasingProcessSharedData* sharedData =
        G4BiasingProcessInterface::GetSharedData( processManager );
      if ( sharedData )
        {
          for ( size_t i = 0 ; i < fProcessesToEquipoise.size() ; i++ )
            {
              G4bool processFound(false);
              for ( size_t j = 0 ;
                    j < (sharedData->GetPhysicsBiasingProcessInterfaces()).size();
                    j++ )
                {
                  const G4BiasingProcessInterface* wrapperProcess =
                    (sharedData->GetPhysicsBiasingProcessInterfaces())[j];
                  if ( fProcessesToEquipoise[i] ==
                       wrapperProcess->GetWrappedProcess()->GetProcessName() )
                    {
                      fProcesses.push_back( wrapperProcess->GetWrappedProcess() );
                      processFound = true;
                      break;
                    }
                }
              if ( !processFound )
                {
                  G4String particleName = "(unknown)";
                  if ( fParticleToBias != nullptr )
                    {
                      particleName = fParticleToBias->GetParticleName();
                    }
                  G4ExceptionDescription ed;
                  ed << "Process `" << fProcessesToEquipoise[i]
                     << "' not found for particle `" << particleName << "'"
                     << G4endl;
                  G4Exception("remollBOptrSplitAndKillByCrossSection::StartRun(...)",
                              "exremoll.02",
                              JustWarning,
                              ed);
                }
            }
        }
      fSetup = false;
    }

  if ( fProcessesToEquipoise.size() == 0 || fProcesses.size() == 0 )
    {
      G4ExceptionDescription ed;
      ed << "No processes to counterbalance for defined or found ! "
         << "Biasing will do nothing."
         << G4endl;
      G4Exception("remollBOptrSplitAndKillByCrossSection::StartRun(...)",
                  "exremoll.03",
                  JustWarning,
                  ed);
    }

}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

G4VBiasingOperation*
remollBOptrSplitAndKillByCrossSection::
ProposeNonPhysicsBiasingOperation(const G4Track*                               track,
                                  const G4BiasingProcessInterface* /*callingProcess*/)
{

  // -----------------------------------------------------
  // -- Check if current particle type is the one to bias:
  // -----------------------------------------------------
  if ( track->GetDefinition() != fParticleToBias ) return 0;

  // --------------------------------------------------------------------
  // -- Compute the total cross-section for the physics processes
  // -- considered.
  // -- These physics processes have been updated to the current track
  // -- state by their related wrapper G4BiasingProcessInterface objects,
  // -- the cross-sections/mean free pathes are hence usable.
  // --------------------------------------------------------------------
  G4double totalCrossSection(0.0);
  for ( size_t i = 0 ; i < fProcesses.size() ; i++ )
    {
      G4double interactionLength = fProcesses[i]->GetCurrentInteractionLength();
      if ( interactionLength < DBL_MAX/10. )
        totalCrossSection += 1./interactionLength;
    }
  if ( totalCrossSection < DBL_MIN ) return 0;

  G4double totalInteractionLength = 1./totalCrossSection;

  // ---------------------------------------------------------------------
  // -- Passes the updated "absorption" cross-section (interaction length)
  // -- to the biasing operation, and returns this operation:
  // ---------------------------------------------------------------------
  fSplitAndKillByCrossSection->SetInteractionLength( totalInteractionLength );

  return fSplitAndKillByCrossSection;

}

void
remollBOptrSplitAndKillByCrossSection::
AddProcessToEquipoise(G4String processName)
{
  fProcessesToEquipoise.push_back( processName );
}
