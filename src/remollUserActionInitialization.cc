/*
 * remollUserActionInitialization.cc
 *
 *  Created on: May 1, 2017
 *      Author: wdconinc
 */

#include "remollUserActionInitialization.hh"

#include "remollIO.hh"
#include "remollMessenger.hh"

#include "remollRunAction.hh"
#include "remollEventAction.hh"
#include "remollSteppingAction.hh"
#include "remollPrimaryGeneratorAction.hh"

remollUserActionInitialization::remollUserActionInitialization() {
  fMess = new remollMessenger();
  fIO   = new remollIO();
  fMess->SetIO(fIO);

}

remollUserActionInitialization::~remollUserActionInitialization() { }

void remollUserActionInitialization::Build() const
{
  remollPrimaryGeneratorAction* gen_action = new remollPrimaryGeneratorAction;
  gen_action->SetIO(fIO);
  fMess->SetPriGen(gen_action);
  SetUserAction(gen_action);

  remollEventAction* event_action = new remollEventAction;
  event_action->SetIO(fIO);
  SetUserAction(event_action);

  remollSteppingAction* stepping_action = new remollSteppingAction;
  fMess->SetStepAct(stepping_action);
  SetUserAction(stepping_action);
}

void remollUserActionInitialization::BuildForMaster() const
{
  remollRunAction* run_action = new remollRunAction;
  run_action->SetIO(fIO);
  SetUserAction(run_action);
}
