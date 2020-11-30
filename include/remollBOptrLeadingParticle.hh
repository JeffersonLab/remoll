#ifndef remollBOptrLeadingParticle_hh
#define remollBOptrLeadingParticle_hh 1

#include "G4VBiasingOperator.hh"
class G4BOptnLeadingParticle;
class G4VProcess;

#include <map>

class remollBOptrLeadingParticle : public G4VBiasingOperator {
public:
  remollBOptrLeadingParticle( G4String operatorName = "LeadingParticleBiasingOperator");
  virtual ~remollBOptrLeadingParticle();

private:
  // -----------------------------
  // -- Mandatory from base class:
  // -----------------------------
  // -- Unsused:
  virtual G4VBiasingOperation*
  ProposeNonPhysicsBiasingOperation(const G4Track*,
                                    const G4BiasingProcessInterface*) final
  { return nullptr; }
  // -- Unused:
  virtual G4VBiasingOperation*
  ProposeOccurenceBiasingOperation (const G4Track*,
                                    const G4BiasingProcessInterface*) final
  { return nullptr; }
  // -- Used:
  // -- Will return the biasing operation at the final state generation stage
  virtual G4VBiasingOperation*
  ProposeFinalStateBiasingOperation(const G4Track* track,
                                    const G4BiasingProcessInterface* callingProcess) final;

public:
  // ------------------------------------
  // -- Optional methods from base class:
  // ------------------------------------
  virtual void      StartRun()                       final;
  virtual void StartTracking( const G4Track* track ) final;

private:
  // -- The leading particle biasing operation that will actually
  // -- trim the final state generation:
  G4BOptnLeadingParticle* fLeadingParticleBiasingOperation;
  // -- Two-particle final states, they will be biased with a
  // -- "further killing probability" for particles accompanying
  // -- the leading particle, we remember what processes are concerned:
  const G4VProcess*       fAnnihilation;
  const G4VProcess*         fConversion;
  const G4VProcess*              fDecay;
  const G4VProcess* fTwoParticleProcess;

};

#endif
