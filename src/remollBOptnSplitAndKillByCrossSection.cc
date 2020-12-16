#include "Randomize.hh"
#include "remollBOptnSplitAndKillByCrossSection.hh"

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

remollBOptnSplitAndKillByCrossSection::
remollBOptnSplitAndKillByCrossSection(G4String       name,
                                      G4bool forwardSplit)
: G4VBiasingOperation(name),
  fParticleChange(),
  fInteractionLength(-1.0),
  fForwardSplit(forwardSplit? +1.0: -1.0)
{}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

remollBOptnSplitAndKillByCrossSection::
~remollBOptnSplitAndKillByCrossSection()
{}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

G4double
remollBOptnSplitAndKillByCrossSection::
DistanceToApplyOperation( const G4Track*,
                          G4double,
                          G4ForceCondition* condition)
{
  *condition = NotForced;

  // -- Sample the exponential law using the total interaction length of processes
  // -- to couterbalance for:
  G4double proposedStepLength =  -std::log( G4UniformRand() ) * fInteractionLength;

  return proposedStepLength;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

G4VParticleChange*
remollBOptnSplitAndKillByCrossSection::
GenerateBiasingFinalState( const G4Track* track,
                           const G4Step*       )
{

  // -- This method is called if we have limited the step.
  // -- We hence make the splitting or killing.

  // Get track weight:
  G4double initialWeight = track->GetWeight();

  // The "particle change" is the object to be used to communicate to
  // the tracking the update of the primary state and/or creation
  // secondary tracks.
  fParticleChange.Initialize(*track);

  // -- Splitting and killing factors.
  // -- They are taken the same, but the killing factor can be make bigger.
  G4double splittingFactor =  2.0;
  G4double   killingFactor =  2.0;


  if ( fForwardSplit * track->GetMomentumDirection().z() > 0 )
    {
      // -- We split if the track is moving forward:

      // Define the tracks weight:
      G4double weightOfTrack = initialWeight/splittingFactor;

      // Ask currect track weight to be changed to new value:
      fParticleChange.ProposeParentWeight( weightOfTrack );
      // Now make clones of this track (this is the actual splitting):
      // we will then have the primary and clone of it, hence the
      // splitting by a factor 2:
      G4Track* clone = new G4Track( *track );
      clone->SetWeight( weightOfTrack );
      fParticleChange.AddSecondary( clone );
      // -- Below's call added for safety & illustration : inform particle change to not
      // -- modify the clone (ie : daughter) weight to male it that of the
      // -- primary. Here call not mandatory and both tracks have same weights.
      fParticleChange.SetSecondaryWeightByProcess(true);
    }
  else
    {
      // -- We apply Russian roulette if the track is moving backward:

      // Shoot a random number (in ]0,1[ segment):
      G4double random = G4UniformRand();
      G4double killingProbability = 1.0 - 1.0/killingFactor;
      if ( random < killingProbability )
        {
          // We ask for the the track to be killed:
          fParticleChange.ProposeTrackStatus(fStopAndKill);
        }
      else
        {
          // In this case, the track survives. We change its weight
          // to conserve weight among killed and survival tracks:
          fParticleChange.ProposeParentWeight( initialWeight*killingFactor );
        }
    }

  return &fParticleChange;

}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
