#include "remollGenRemoll.hh"

#include "remollEvent.hh"
#include "remollVertex.hh"
#include "remollBeamTarget.hh"

#include "G4Material.hh"
#include "G4VPhysicalVolume.hh"
#include "G4LogicalVolume.hh"
#include "G4PhysicalConstants.hh"

#include "remolltypes.hh"


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


remollGenRemoll::remollGenRemoll()
: remollVEventGen("Remoll") {
  fApplyMultScatt = true;

//  fZpos = (28.5*m - 0.52*m);
//FIXME declare in header
  elasticDist = new TH1F("el","elastic dist",200,0.6,1.2);
  inelDist = new TH1F("inel","inelastic dist",200,0.6,1.2);
}

remollGenRemoll::~remollGenRemoll() {
  delete(elasticDist);
  delete(inelDist);
}

void remollGenRemoll::SamplePhysics(remollVertex * /*vert*/, remollEvent *evt)
{

  double xPos, yPos, zPos;
  fE_min = 2.0*GeV;
  fE_max = 11.0*GeV;

  fTh_min = -0*deg;//FIXME Needs justification, what about the angle about the z axis that this points as well, is that phi, or is phi the start position about z?
  fTh_max = 2.5*deg;

  fR_min = 0.0*mm;
  fR_max = 0.0*mm;

  fPh_min = 0.0*deg;
  fPh_max = 360.0*deg;//360.0*deg;

  fDeltaPh_min = -2.0*deg;
  fDeltaPh_max = 2.0*deg;

//  fRing = 5;
//  fSector = 0; //FIXME what are the sector options? What do they refer to?
//  fBoffsetR = false;

  zPos = fZ;

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
  zPos = 28500 - 500; //FIXME arbitrary z offset for Moller distribution propagation - affects air showering noise
  double xHitPos = (rad*cos(randPhi) - 1*((fBoffsetR)?radialOffset[fRing][fSector] : 0)); // Putting the offset here means that the detector and distribution will still make circles, just where the edge of the circle now passes the origin
  double yHitPos = rad*sin(randPhi);
  xPos = xHitPos - (-1*zPos)*sin(randTheta)*cos(randPhi) - (-1*zPos)*sin(randPhi)*sin(randDeltaPhi);
  yPos = yHitPos - (-1*zPos)*sin(randTheta)*sin(randPhi) + (-1*zPos)*cos(randPhi)*sin(randDeltaPhi);


  assert( E > 0.0 );
  assert( E > mass );

  evt->ProduceNewParticle( G4ThreeVector(xPos, yPos, zPos), 
    G4ThreeVector(pX, pY, pZ ), 
	  "e-" );
  evt->fBeamE = E;
  evt->fBeamMomentum = evt->fBeamMomentum.unit()*p;
  // Override target sampling z
//  evt->fVertexPos.setZ( fZpos );
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

  double radius;
  double test = CLHEP::RandFlat::shoot(0.0,1.0);
  if (test < 24163.0/1265326.0){//inelastic
    double gArea[] = {2.60384e6,36188.3,8.65955e5,1.64544e6};
    double eArea[] = {2.76828e6,264188,7.15987e5,1.85492e6};
    double tH[] = {30e6,5e6,12e6,20e6};
    double tN[] = {2.81725e6,7.2377e4,9.07299e5,1.84075e6};
    double mu[] = {7.73e-1,7.5e-1,7.7e-1,7.73e-1};
    double sig[] = {1.88253e-2,1.12121e-2,1.77574e-2,2.16431e-2};
    double n[] = {5e6,3e5,1e6,3.5e6};
    double exp[] = {6.5,7.0,9.0,6.0};
    double v[] = {0.75,0.3,0.75,0.75};


    double div = (fSector == 1)? 0.75 :(fSector == 2)? 0.79 : 0.8;
    double u =  CLHEP::RandFlat::shoot(0.0,1.0);
    bool filled = false;

    if (u < gArea[fSector]/(gArea[fSector] + eArea[fSector])){
      while (!filled){
        double u1 = 2* CLHEP::RandFlat::shoot(0.0,1.0)-1;
        double u2 = 2* CLHEP::RandFlat::shoot(0.0,1.0)-1;
    
        double r = u1*u1 + u2*u2;

        if (r <= 1 && r > 1e-6){

          double z1 = u1*sqrt(-2*log(r)/r);
          radius = mu[fSector]+sig[fSector]*z1; 
  
          if (radius < div){
            inelDist->Fill(radius);
            filled = true;
          }
        }
      }
    }
    else{
      do{
        radius = CLHEP::RandFlat::shoot(div,1.2);
        double u2 = CLHEP::RandFlat::shoot(0.0,1.0);

        if ( tH[fSector]*u2 < n[fSector]*TMath::Power(radius,(-1*exp[fSector]))+v[fSector]){
          inelDist->Fill(radius);
          filled = true;
        }
      }while (!filled);
    }
  }
  else if (test < 61237.0/1265326.0){//elastic
    double gArea[] = {7.21689e7,782945,1.3112e7,5.9954e7};
    double qArea[] = {1.70795e8,9.09159e6,5.50713e7,1.25122e8};
    double n[] = {800e6,60e6,550e6,600e6};
    double tN[] = {8.08859e7,8.19378e5,1.75234e7,6.79431e7};
    double tMu[] = {0.748941,0.746941,0.759441,0.749441};
    double tSig[] = {1.70039e-2,1.35555e-2,1.5783e-2,1.7318e-2};
    double tQ[] = {9.77635e9,3.3381e8,5.65955e9,7.1164e9};
    double tY[] = {2.45583e8,7.98e6,5.62236e7,1.80618e8};
    double tV[] = {9.75e-1,0.83,0.92,0.975};
    
    bool filled = false;
    
    double u1,u2;
    double u =  CLHEP::RandFlat::shoot(0.0,1.0);

    if (u >= gArea[fSector]/(gArea[fSector]+qArea[fSector])){
      do{
        u1 =  CLHEP::RandFlat::shoot(0.77,1.2);
        u2 =  CLHEP::RandFlat::shoot(0.0,1.0);

        if (u1 > 0.77 && u2*n[fSector] < tQ[fSector]*(u1-tV[fSector])*(u1-tV[fSector]) + tY[fSector]){
          radius = u1;
          elasticDist->Fill(radius);
          filled = true;
        }
      }while (!filled);
    }
    else{
      do{
        u1 = 2.0* CLHEP::RandFlat::shoot(0.0,1.0) - 1;
        u2 = 2.0* CLHEP::RandFlat::shoot(0.0,1.0) - 1;
        double r2 = (u1)*(u1) + (u2)*(u2);
        if (r2 <= 1){   
          radius = (tMu[fSector] + tSig[fSector]*(u1*sqrt(-2*log(r2)/r2)));

          if (elasticDist->GetBinContent(elasticDist->GetXaxis()->FindBin(tMu[fSector])) < tN[fSector]){
            if (radius < 0.77){
              elasticDist->Fill(radius);
              filled = true;
            }
          }
        }
      }while (!filled);
    }
  }
  else{//moller
    bool filled = false;

    double r,u1,u2;
    double n[4] = {3.28542e8,4.98888e7,1.43470e8,1.70085e8};
    double mu[4] = {9.97921e-1,1.01235,1.00520,0.984920};
    double sig[4] = {0.04,3.07008e-2,3.187e-2,0.04};
    do{
      u1 = 2.0*CLHEP::RandFlat::shoot(0.0,1.0) - 1;
      u2 = 2.0*CLHEP::RandFlat::shoot(0.0,1.0) - 1;
      r = u1*u1 + u2*u2;
      if (r <= 1) {
        radius = mu[fSector] + sig[fSector]*(u1*sqrt(-2*log(r)/r));
        filled = true;
      }
    }while (!filled);
  }
  double LGstart[] = {0.730,0.730,0.855,0.930,1.04,1.2};
  double finRad = (radius /*- LGstart[fRing] - 0.075*/)*1000.0; //0.075*m is the offset for the start of the light guide, since the center of the quartz is at (0,0,0).
  //if (-1.0*finRad > 75) std::cout <<"finRad" << std::endl; else std::cout <<"recur" << std::endl;
  //std::cout << finRad << std::endl << -1.0*finRad << std::endl;
  return  finRad;
  //    return (-1.0*finRad > 75)? finRad : RadSpectrum();
}

