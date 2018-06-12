/* remollGenRemoll.cc
 *
 * Updated June 2018
 * Authors: Audrey Farrell, Cameron Clarke
 *
 * Reads functions of radius from external root file and generates particles according to those functions.
 * Default functions are inside remollGenFunctions.root, and include moller, elastic, and inelastic scattering
 * for the whole array as well as for sectors 1, 2, and 3.
 *
 */
#include "remollGenRemoll.hh"

#include "remollEvent.hh"
#include "remollVertex.hh"
#include "remollBeamTarget.hh"

#include "G4GenericMessenger.hh"
#include "G4Material.hh"
#include "G4VPhysicalVolume.hh"
#include "G4LogicalVolume.hh"
#include "G4PhysicalConstants.hh"

#include "remolltypes.hh"


#include "CLHEP/Random/RandFlat.h"
#include "CLHEP/Random/RandGauss.h"
#include <math.h>
#include <iostream>
#include <string>
// For the TF1s
#include <TFile.h>
#include <TF1.h>

G4Mutex inFileMutex2 = G4MUTEX_INITIALIZER; //files are being read so mutex is needed

remollGenRemoll::remollGenRemoll()
: remollVEventGen("Remoll"),
    fFile(0),
    fFunc(0)
{
    fApplyMultScatt = true;
    r_t = CLHEP::RandFlat::shoot(0.6,1.2);
    fZpos = (28.5*m - 0.52*m);
    fThisGenMessenger->DeclareMethod("input",&remollGenRemoll::SetGenFunctionFile,"ROOT filename:function name");
    SetGenFunctionFile(*new G4String("../analysis/externalGenerator/remollGenFunctions.root:elastic_0"));
}

remollGenRemoll::~remollGenRemoll() {
    if (fFile){
        fFile->Close();
        fFunc = 0;
    }
}

void remollGenRemoll::SetGenFunctionFile(G4String& input) {
    if (!input.contains(":")){
        G4cerr << "Improper formatting for user input. Please ensure your macro commands include '/remoll/remollinput <file name>:<function name>'" << G4endl;
        return;
    }

    std::stringstream ss;
    ss << input;

    G4String filename;
    G4String funcname;

    std::getline(ss, filename, ':');
    std::getline(ss, funcname, ':');

    G4cout << "File name read as " << filename << G4endl;
    G4cout << "Function name read as " << funcname << G4endl;

    G4AutoLock inFileLock(&inFileMutex2);

    G4cout << "Setting the external file to " << filename << G4endl;

    if (fFile) {
        fFile->Close();
        fFunc  = 0;
    }

    fFile = new TFile(filename);
    if (! fFile){
        G4cerr << "could not open function file " << filename << G4endl;
        return;
    }

    fFile->GetObject(funcname,fFunc);
    if (! fFunc){
        G4cerr << "could not find function in file " << filename << G4endl;
        return;
    }
    inFileLock.unlock();
}

void remollGenRemoll::SamplePhysics(remollVertex * /*vert*/, remollEvent *evt)
{

  double xPos, yPos, zPos, zOffset;
  fE_min = 2.0*GeV;
  fE_max = 11.0*GeV;

  fTh_min = -0*deg;//FIXME Needs justification, what about the angle about the z axis that this points as well, is that phi, or is phi the start position about z?
  fTh_max = 2.5*deg;

  fR_min = 0.0*mm;
  fR_max = 0.0*mm;

  fPh_min = 0.0*deg;
  fPh_max = 360.0*deg;

  fDeltaPh_min = -2.0*deg;
  fDeltaPh_max = 2.0*deg;

//  fRing = 5;
//  fSector = 0; //FIXME what are the sector options? What do they refer to?
//  fBoffsetR = false;

//  zPos = fZ;

  // Get initial Remoll energy instead of using other sampling
  double E = CLHEP::RandFlat::shoot( fE_min, fE_max );
  double mass = electron_mass_c2;
  double p = sqrt(E*E - mass*mass);

  double pX, pY, pZ;
  double randTheta, randDeltaPhi, randPhi;
  double tanth, tanph;

  randTheta = CLHEP::RandFlat::shoot( fTh_min, fTh_max );
  randPhi = CLHEP::RandFlat::shoot(fPh_min,fPh_max);
  randDeltaPhi = CLHEP::RandFlat::shoot(fDeltaPh_min,fDeltaPh_max); // FIXME define min/max angle spread in phi direction
  pX = cos(randPhi)*sin(randTheta)*p + sin(randPhi)*sin(randDeltaPhi)*p;
  pY = sin(randPhi)*sin(randTheta)*p - cos(randPhi)*sin(randDeltaPhi)*p;
  pZ = cos(randDeltaPhi)*cos(randTheta)*p;
//get radial distribution from remoll, radius is along x-axis
  double radialOffset[6][3] = {{710,710,710},
    {755,755,755},
    {817.5,817.5,817.5},
    {892.5,892.5,892.5},
    {1017.5,987.5,1030},
    {1150,1150,1150}};
  double rad = RadSpectrum();
  zOffset = -1*500; //FIXME arbitrary z offset for Moller distribution propagation - affects air showering noise
  zPos = (28500 + zOffset); //FIXME arbitrary z offset for Moller distribution propagation - affects air showering noise
  double xHitPos = (rad*cos(randPhi) - 1*((fBoffsetR)?radialOffset[fRing][fSector] : 0)); // Putting the offset here means that the detector and distribution will still make circles, just where the edge of the circle now passes the origin
  double yHitPos = rad*sin(randPhi);
  xPos = xHitPos - (-1*zOffset)*sin(randTheta)*cos(randPhi) - (-1*zOffset)*sin(randPhi)*sin(randDeltaPhi);
  yPos = yHitPos - (-1*zOffset)*sin(randTheta)*sin(randPhi) + (-1*zOffset)*cos(randPhi)*sin(randDeltaPhi);


  assert( E > 0.0 );
  assert( E > mass );

  evt->ProduceNewParticle( G4ThreeVector(xPos, yPos, zPos), 
    G4ThreeVector(pX, pY, pZ ), 
	  "e-" );
  evt->fBeamE = E;
  evt->fBeamMomentum = evt->fBeamMomentum.unit()*p;
  // Override Beam Target creating a spread in z vertex, by overwriting the remollVEventGen.cc functionality
  // where it asks the remollBeamTarget to treat the vertex as beam target spread. Adds (+=) the vector fVertexPos
  // to the particle position vector defined when particle is defined, happening in PolishEvent() method
  evt->fVertexPos.setZ( 0.0 );
  
  evt->SetEffCrossSection(0.0);
  evt->SetAsymmetry(0.0);
  evt->SetQ2(0.0);
  evt->SetW2(0.0);

}

double remollGenRemoll::RadSpectrum(){
  //determine whether to use moller, elastic, or inelastic distribution

  if (fR_max>0.0 && fBoffsetR==true) {
    double flatRad = CLHEP::RandFlat::shoot(fR_min,fR_max);
    return flatRad;
  }
  

  //Randomly generate the distribution using the Metropolis algorithm
  double r, a, u; 
  //r_t is the previous hit, r is the proposed new hit, a is their relative
  //probabilities, and u is the deciding probability.
  r = CLHEP::RandGauss::shoot(r_t,0.1); //generate proposed r with gaussian around previous
  a = fFunc->Eval(r) / fFunc->Eval(r_t);

  u = CLHEP::RandFlat::shoot(0.0,1.0);
  if (u <= a)
      r_t = r;

  double finRad = (r_t)*1000.0;
  return  finRad;
}

