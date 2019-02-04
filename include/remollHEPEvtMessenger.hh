#ifndef REMOLLHEPEVTMESSENGER_H
#define REMOLLHEPEVTMESSENGER_H

#include "G4UImessenger.hh"

class remollHEPEvtInterface;

class G4UIdirectory;
class G4UIcmdWithoutParameter;
class G4UIcmdWithAString;
class G4UIcmdWithAnInteger;

class remollHEPEvtMessenger : public G4UImessenger {
public:
  remollHEPEvtMessenger(remollHEPEvtInterface* iface);
  virtual ~remollHEPEvtMessenger();

  void SetNewValue(G4UIcommand* command, G4String newValues);
  G4String GetCurrentValue(G4UIcommand* command);

private:
  remollHEPEvtInterface* fHEPEvtInterface;

  G4UIdirectory* fDir;
  G4UIcmdWithAnInteger* fVerbose;
  G4UIcmdWithAString* fOpen;

};

#endif
