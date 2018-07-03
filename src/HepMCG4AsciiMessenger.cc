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
/// \file eventgenerator/HepMC/HepMCEx01/src/HepMCG4AsciiMessenger.cc
/// \brief Implementation of the HepMCG4AsciiMessenger class
//
// $Id: HepMCG4AsciiMessenger.cc 77801 2013-11-28 13:33:20Z gcosmo $
//
#include "G4UIdirectory.hh"
#include "G4UIcmdWithoutParameter.hh"
#include "G4UIcmdWithAString.hh"
#include "G4UIcmdWithAnInteger.hh"
#include "HepMCG4AsciiMessenger.hh"
#include "HepMCG4AsciiInterface.hh"


//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
HepMCG4AsciiMessenger::HepMCG4AsciiMessenger
                      (HepMCG4AsciiInterface* agen)
  : gen(agen)
{
  fDir= new G4UIdirectory("/generator/hepmcAscii/");
  fDir-> SetGuidance("Reading HepMC event from an Ascii file");

  fVerbose=
    new G4UIcmdWithAnInteger("/generator/hepmcAscii/verbose", this);
  fVerbose-> SetGuidance("Set verbose level");
  fVerbose-> SetParameterName("verboseLevel", false, false);
  fVerbose-> SetRange("verboseLevel>=0 && verboseLevel<=1");

  fOpen= new G4UIcmdWithAString("/generator/hepmcAscii/open", this);
  fOpen-> SetGuidance("(re)open data file (HepMC Ascii format)");
  fOpen-> SetParameterName("input ascii file", true, true);
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
HepMCG4AsciiMessenger::~HepMCG4AsciiMessenger()
{
  delete fVerbose;
  delete fOpen;
  delete fDir;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
void HepMCG4AsciiMessenger::SetNewValue(G4UIcommand* command,
                                              G4String newValues)
{
  if (command==fVerbose) {
    int level= fVerbose-> GetNewIntValue(newValues);
    gen-> SetVerboseLevel(level);
  } else if (command==fOpen) {
    gen-> SetFileName(newValues);
    G4cout << "HepMC Ascii inputfile: "
           << gen-> GetFileName() << G4endl;
    gen-> Initialize();
  }
}


//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
G4String HepMCG4AsciiMessenger::GetCurrentValue(G4UIcommand* command)
{
  G4String cv;

  if (command == fVerbose) {
    cv= fVerbose-> ConvertToString(gen-> GetVerboseLevel());
  } else  if (command == fOpen) {
    cv= gen-> GetFileName();
  }
  return cv;
}
