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
/// \file eventgenerator/HepMC/HepMCEx01/include/HepMCG4AsciiInterface.hh
/// \brief Definition of the HepMCG4AsciiInterface class
//
// $Id: HepMCG4AsciiInterface.hh 77801 2013-11-28 13:33:20Z gcosmo $
//

#ifndef HEPMC_G4_ASCII_READER_H
#define HEPMC_G4_ASCII_READER_H

#include "HepMCG4Interface.hh"
#include "HepMC/IO_GenEvent.h"

class HepMCG4AsciiMessenger;

class HepMCG4AsciiInterface : public HepMCG4Interface {
protected:
  G4String filename;
  HepMC::IO_GenEvent* asciiInput;

  G4int verbose;
  HepMCG4AsciiMessenger* messenger;

  virtual HepMC::GenEvent* GenerateHepMCEvent();

public:
  HepMCG4AsciiInterface();
  ~HepMCG4AsciiInterface();

  // set/get methods
  void SetFileName(G4String name);
  G4String GetFileName() const;

  void SetVerboseLevel(G4int i);
  G4int GetVerboseLevel() const; 

  // methods...
  void Initialize();
};

// ====================================================================
// inline functions
// ====================================================================

inline void HepMCG4AsciiInterface::SetFileName(G4String name)
{
  filename= name;
}

inline G4String HepMCG4AsciiInterface::GetFileName() const
{
  return filename;
}

inline void HepMCG4AsciiInterface::SetVerboseLevel(G4int i)
{
  verbose= i;
}

inline G4int HepMCG4AsciiInterface::GetVerboseLevel() const
{
  return verbose;
}

#endif
