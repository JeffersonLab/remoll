//---------------------------------------------------------------
//
// remollBOptrSplitAndKillByCrossSection
//
// Class Description:
//        A G4VBiasingOperator concrete implementation example to
//    illustrate how to bias physics processes cross-section for
//    one particle type.
//        The G4VBiasingOperation G4BOptnChangeCrossSection is
//    selected by this operator, and is sent to each process
//    calling the operator.
//        A simple constant bias to the cross-section is applied,
//    but more sophisticated changes can be applied.
//
//---------------------------------------------------------------
//

#ifndef remollBOptrSplitAndKillByCrossSection_hh
#define remollBOptrSplitAndKillByCrossSection_hh 1

#include "G4VBiasingOperator.hh"
class G4BOptnChangeCrossSection;
class G4ParticleDefinition;
class G4VProcess;
class remollBOptnSplitAndKillByCrossSection;
#include <map>


class remollBOptrSplitAndKillByCrossSection : public G4VBiasingOperator {
public:
  // ------------------------------------------------------------
  // -- Constructor: takes the name of the particle type to bias:
  // ------------------------------------------------------------
  remollBOptrSplitAndKillByCrossSection(G4String particleToBias,
                                        G4bool forwardSplit = true,
                                        G4String name = "SplitAndKillByXS");
  virtual ~remollBOptrSplitAndKillByCrossSection();

  // -- method called at beginning of run:
  virtual void StartRun();

private:
  // -----------------------------
  // -- Mandatory from base class:
  // -----------------------------
  // -- Not used:
  virtual G4VBiasingOperation*
  ProposeOccurenceBiasingOperation(const G4Track*,
                                   const G4BiasingProcessInterface*) final
  { return 0; }

  // -- Not used:
  virtual G4VBiasingOperation*
  ProposeFinalStateBiasingOperation(const G4Track*,
                                    const G4BiasingProcessInterface*) final
  { return 0; }

  // -- Used method : it will return the biasing operation that will split particles
  // -- with a probabilty depending on the total absorption cross-section.
  virtual G4VBiasingOperation*
  ProposeNonPhysicsBiasingOperation(const G4Track*,
                                    const G4BiasingProcessInterface*) final;


  // ---------------------------------------
  // -- Method specific to this application:
  // ---------------------------------------
  // -- Each "absorbing" process that the biasing has to counterbalance
  // -- its action for has to be passed to the biasing operator
public:
  void AddProcessToEquipoise(G4String processName);


private:
  remollBOptnSplitAndKillByCrossSection* fSplitAndKillByCrossSection;
  const G4ParticleDefinition*                      fParticleToBias;
  const G4bool                                       fForwardSplit;
  std::vector< G4String >                    fProcessesToEquipoise;
  G4bool                                                    fSetup;
  std::vector< const G4VProcess* >                      fProcesses;

};

#endif
