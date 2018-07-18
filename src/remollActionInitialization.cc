/*
 * remollUserActionInitialization.cc
 *
 *  Created on: May 1, 2017
 *      Author: wdconinc
 */

#include "remollActionInitialization.hh"

#include "remollRunAction.hh"
#include "remollEventAction.hh"
#include "remollSteppingAction.hh"
#include "remollTrackingAction.hh"
#include "remollPrimaryGeneratorAction.hh"

void remollActionInitialization::Build() const
{
  // Run action
  remollRunAction* run_action = new remollRunAction();
  SetUserAction(run_action);

  // Event action
  remollEventAction* event_action = new remollEventAction();
  SetUserAction(event_action);

  // Stepping action
  remollSteppingAction* stepping_action = new remollSteppingAction();
  SetUserAction(stepping_action);

  // Tracking action
  remollTrackingAction* tracking_action = new remollTrackingAction();
  SetUserAction(tracking_action);

  // Primary generator action
  remollPrimaryGeneratorAction* gen_action = new remollPrimaryGeneratorAction();
  SetUserAction(gen_action);
  event_action->SetPrimaryGeneratorAction(gen_action);
}

void remollActionInitialization::BuildForMaster() const
{
  // Run action
  remollRunAction* run_action = new remollRunAction();
  SetUserAction(run_action);
}
