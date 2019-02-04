#ifndef __REMOLLPHYSICALCONSTANTS_HH
#define __REMOLLPHYSICALCONSTANTS_HH

#include <G4PhysicalConstants.hh>

#include "remollSystemOfUnits.hh"

/**
 * Physical constants, defined using the Geant4 units and
 * the additional units in remollSystemOfUnits.hh.
 */

const double GF = 1.1663787e-5/GeV/GeV; // CODATA https://physics.nist.gov/cgi-bin/cuu/Value?gf
const double alpha = 0.007299;

//const double sin2thW_ms = 0.23116; // PDG 2012
const double sin2thW_ms = 0.23125; // arxiv 1302.6263 - KK's low Q2 EW review

const double QWe =  -0.0435;  // arxiv 1302.6263 - KK's low Q2 EW review
const double QWe_rad = -0.002787;  //  arxiv 1302.6263 - KK's low Q2 EW review with radiative (eq 31)
                             //  See HAPLOG 284
const double QWp = 0.0707;

const double QWn = -0.988;

#endif // __REMOLLPHYSICALCONSTANTS_HH
