#ifndef __REMOLLGLOBS_HH
#define __REMOLLGLOBS_HH

#include "G4SystemOfUnits.hh"

/*
   remollglobs.hh

   Global type stuff
*/


const double GF = 1.16637e-5/GeV/GeV; 
const double alpha = 0.007299; 
const double sin2thW = 0.2312;
const double QeW = 1.0 - 4.0*sin2thW; 

const double gDefaultBeamE   = 11.0*GeV;
const double gDefaultBeamPol = 0.85;
const double gDefaultBeamCur = 85e-6*ampere;


#endif//__REMOLLGLOBS_HH
