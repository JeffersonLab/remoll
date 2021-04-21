//
// ********************************************************************
// * License and Disclaimer                                           *
// *                                                                  *
// * The  Geant4 software  is  copyright of the Copyright Holders  of *
// * the Geant4 Collaboration.  It is provided  under  the terms  and *
// * conditions of the Geant4 Software License,  included in the file *
// * LICENSE and available at  http://cern.ch/geant4/license .  These *
// * include a list of copyright holders.                             *
// *                                                                  *
// * Neither the authors of this software system, nor their employing *
// * institutes,nor the agencies providing financial support for this *
// * work  make  any representation or  warranty, express or implied, *
// * regarding  this  software system or assume any liability for its *
// * use.  Please see the license in the file  LICENSE  and URL above *
// * for the full disclaimer and the limitation of liability.         *
// *                                                                  *
// * This  code  implementation is the result of  the  scientific and *
// * technical work of the GEANT4 collaboration.                      *
// * By using,  copying,  modifying or  distributing the software (or *
// * any work based  on the software)  you  agree  to acknowledge its *
// * use  in  resulting  scientific  publications,  and indicate your *
// * acceptance of all terms of the Geant4 Software license.          *
// ********************************************************************
//
/// \file eventgenerator/HepMC/HepMCEx01/include/HepMCG4AsciiMessenger.hh
/// \brief Definition of the HepMCG4AsciiMessenger class
//
// $Id: HepMCG4AsciiMessenger.hh 77801 2013-11-28 13:33:20Z gcosmo $
//

#ifndef HEPMC_G4_ASCII_READER_MESSENGER_H
#define HEPMC_G4_ASCII_READER_MESSENGER_H

#include "G4UImessenger.hh"

class HepMCG4AsciiInterface;
class G4UIdirectory;
class G4UIcmdWithoutParameter;
class G4UIcmdWithAString;
class G4UIcmdWithAnInteger;

class HepMCG4AsciiMessenger : public G4UImessenger {
public:
  HepMCG4AsciiMessenger(HepMCG4AsciiInterface* iface);
  virtual ~HepMCG4AsciiMessenger();

  void SetNewValue(G4UIcommand* command, G4String newValues);
  G4String GetCurrentValue(G4UIcommand* command);

private:
  HepMCG4AsciiInterface* gen;

  G4UIdirectory* fDir;
  G4UIcmdWithAnInteger* fVerbose;
  G4UIcmdWithAString* fOpen;

};

#endif
