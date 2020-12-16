#ifndef remollBOptnSplitAndKillByCrossSection_hh
#define remollBOptnSplitAndKillByCrossSection_hh 1

#include "G4VBiasingOperation.hh"
#include "G4ParticleChange.hh"


class remollBOptnSplitAndKillByCrossSection : public G4VBiasingOperation {
public:
  // -- Constructor :
  remollBOptnSplitAndKillByCrossSection(G4String name, G4bool forwardSplit);
  // -- destructor:
  virtual ~remollBOptnSplitAndKillByCrossSection();

public:
  // ----------------------------------------------
  // -- Methods from G4VBiasingOperation interface:
  // ----------------------------------------------
  // -- Unused:
  virtual const G4VBiasingInteractionLaw*
  ProvideOccurenceBiasingInteractionLaw( const G4BiasingProcessInterface*,
                                         G4ForceCondition&                 ) final
  {return 0;}
  virtual G4VParticleChange*
  ApplyFinalStateBiasing               ( const G4BiasingProcessInterface*,
                                         const G4Track*,
                                         const G4Step*,
                                         G4bool&                           ) final
  {return 0;}

  // -- Used methods ("non-physics biasing methods"):
  // ------------------------------------------------
  // -- Method to return the distance or the condition under which
  // -- requesting the biasing.
  // -- Here this distance will be sampled according the exponential
  // -- interaction law, using the interaction length passed to the
  // -- method SetInteractionLength(G4double)  below.
  virtual G4double
  DistanceToApplyOperation              ( const G4Track*,
                                          G4double,
                                          G4ForceCondition* condition      ) final;
  // -- Method the generate the final state, which is made of the primary
  // -- with half of its original weight, and a clone of it.
  virtual G4VParticleChange*
  GenerateBiasingFinalState             ( const G4Track*,
                                          const G4Step*                    ) final;

  // -- Specific to this example:
  // ----------------------------
  void SetInteractionLength(G4double interactionLength )
  {
    fInteractionLength = interactionLength;
  }


private:
  G4ParticleChange    fParticleChange;
  G4double         fInteractionLength;
  G4double              fForwardSplit;

};

#endif
