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
#include "Randomize.hh"
#include "G4AutoLock.hh"

#include <math.h>
// For the TH1Fs
#include <TFile.h>
#include <TMath.h>
#include <TH1.h>
#include <TH2.h>
#include <TTree.h>
#include <TLeaf.h>

#include <vector>

class TFile;
class TTree;
struct remollEvent_t;
struct remollGenericDetectorHit_t;


class remollGenTF1 : public remollVEventGen {
  public:
    remollGenTF1();
    virtual ~remollGenTF1();
    void GeneratePrimaries(G4Event* anEvent);
    double RadSpectrum();
    void PrintEventGen();

  private:
    void SamplePhysics(remollVertex *, remollEvent *);
    
    void SetRing(G4int num);
    void SetRadOffset(G4bool offset);
    void SetGenFunctionFile(G4String filename);
    void SetSector(int secnum);
    void SetScatteringType(G4String input);

    void distAnalysis();
    void getHist(G4String fname);
    void fitHist(G4String type);
    static double elasticFit(double *x, double *par);
    static double inelasticFit(double *x, double *par);
    static double lorentzFit(double *x, double *par);

    double fZpos;
    

    TFile* fFile;
    TF1* fFunc;
    TF1* fMollerFunc;
    TF1* fElasticFunc;
    TF1* fInelasticFunc;
    G4String fType;
    G4int fRing;
    G4int fSector;
    TH1F* fMollerHist;
    TH1F* fElasticHist;
    TH1F* fInelasticHist;
    G4double r_t;
  public:
    G4double fXmin, fXmax, fYmin, fYmax;
    G4double fZ;
    G4double fDeltaPh_min, fDeltaPh_max;
    G4double fR_min, fR_max;
    G4bool fBoffsetR;

};

#endif//__REMOLLGENTF1_HH 
