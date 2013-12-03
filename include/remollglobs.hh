#ifndef __REMOLLGLOBS_HH
#define __REMOLLGLOBS_HH

#include "G4SystemOfUnits.hh"

/*
   remollglobs.hh

   Global type stuff
*/


const double GF = 1.16637e-5/GeV/GeV; 
const double alpha = 0.007299; 
//const double sin2thW_ms = 0.23116; // PDG 2012
const double sin2thW_ms = 0.23125; // arxiv 1302.6263 - KK's low Q2 EW review

const double QWe =  0.0435;  // arxiv 1302.6263 - KK's low Q2 EW review
const double QWe_rad = 0.002787;  //  arxiv 1302.6263 - KK's low Q2 EW review with radiative (eq 31)
	                     //  See HAPLOG 284
const double QWp = -0.0707;

const double gDefaultBeamE   = 11.0*GeV;
const double gDefaultBeamPol = 0.85;
const double gDefaultBeamCur = 75e-6*ampere;


#endif//__REMOLLGLOBS_HH
