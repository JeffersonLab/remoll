#ifndef remollUserTrackInformation_h_
#define remollUserTrackInformation_h_

#include "G4StepStatus.hh"
#include "G4VUserTrackInformation.hh"

class remollUserTrackInformation : public G4VUserTrackInformation
{
  public:
    remollUserTrackInformation() { fStepStatus = fUndefined; };
    virtual ~remollUserTrackInformation() { };
    G4StepStatus GetStepStatus() const { return fStepStatus; };
    void SetStepStatus(G4StepStatus stepstatus) { fStepStatus = stepstatus; };
  private:
    G4StepStatus fStepStatus;
};

#endif /* remollUserTrackInformation_h_ */
