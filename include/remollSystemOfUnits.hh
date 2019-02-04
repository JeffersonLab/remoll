#ifndef __REMOLLSYSTEMOFUNITS_HH
#define __REMOLLSYSTEMOFUNITS_HH

#include "G4Types.hh"
#include "G4SystemOfUnits.hh"

// New units in the remoll namespace
namespace remoll {
  static const double ppm = 1E-6;
  static const double ppb = 1E-9;
}

// Allow for easy use without specifying namespace
using remoll::ppm;
using remoll::ppb;

#endif // __REMOLLSYSTEMOFUNITS_HH
