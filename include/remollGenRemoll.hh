#ifndef __REMOLLGENREMOLL_HH 
#define __REMOLLGENREMOLL_HH 
/*!
 * Event generator matching MOLLER output at detector plane
 *
 * Cameron Clarke
 * April 25, 2018
 *
*/

#include "G4Event.hh"

#include "remollVEventGen.hh"
#include "CLHEP/Random/RandFlat.h"
#include "CLHEP/Random/RandGauss.h"
#include <math.h>
// For the TH1Fs
#include <TFile.h>
#include <TMath.h>
#include <TH1.h>
#include <TH2.h>
#include <TTree.h>
#include <TLeaf.h>


class remollGenRemoll : public remollVEventGen {
  public:
    remollGenRemoll();
    virtual ~remollGenRemoll();
    void GeneratePrimaries(G4Event* anEvent);
    double RadSpectrum();

  private:
    void SamplePhysics(remollVertex *, remollEvent *);
    TH1F * elasticDist;
    TH1F * inelDist;

    double fZpos;

  public:
    G4double fXmin, fXmax, fYmin, fYmax;
    G4double fZ; 
//    G4double fEmin, fEmax;
//    G4double fthetaMin, fthetaMax;
//    G4double fPhiMin, fPhiMax;
//    G4double fDeltaPhiMin, fDeltaPhiMax;
//    G4int fSector, fRing;
//    G4bool fBoffsetR; 

};

#endif//__REMOLLGENREMOLL_HH 
