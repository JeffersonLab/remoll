/*
 * remollUserActionInitialization.cc
 *
 *  Created on: May 1, 2017
 *      Author: wdconinc
 */

#include "remollActionInitialization.hh"

#include "remollMessenger.hh"

#include "remollRunAction.hh"
#include "remollEventAction.hh"
#include "remollSteppingAction.hh"
#include "remollPrimaryGeneratorAction.hh"

void remollActionInitialization::Build() const
{
  // Get messenger
  remollMessenger* mess = remollMessenger::GetInstance();

  // Run action
  remollRunAction* run_action = new remollRunAction();
  SetUserAction(run_action);

  // Primary generator action
  remollPrimaryGeneratorAction* gen_action = new remollPrimaryGeneratorAction();
  mess->SetPriGen(gen_action); // TODO change to add instance of pri gen
  SetUserAction(gen_action);

  // Event action
  remollEventAction* event_action = new remollEventAction();
  //mess->SetEventAction(gen_action); // TODO change to add instance of event action
  SetUserAction(event_action);

  // Stepping action
  remollSteppingAction* stepping_action = new remollSteppingAction();
  mess->SetStepAct(stepping_action); // TODO change to add instance of stepping action
  SetUserAction(stepping_action);
}

void remollActionInitialization::BuildForMaster() const
{
  // Run action
  remollRunAction* run_action = new remollRunAction();
  SetUserAction(run_action);
}
