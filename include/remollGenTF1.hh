#ifndef __REMOLLGENTF1_HH 
#define __REMOLLGENTF1_HH 
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


class remollGenTF1 : public remollVEventGen {
  public:
    remollGenTF1();
    virtual ~remollGenTF1();
    void GeneratePrimaries(G4Event* anEvent);
    double RadSpectrum();

  private:
    void SamplePhysics(remollVertex *, remollEvent *);
    void SetGenFunctionFile(G4String& filename);
    double fZpos;
    
    TFile* fFile;
    TF1* fFunc;
    G4double r_t;
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

#endif//__REMOLLGENTF1_HH 
