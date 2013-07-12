// http://mathworld.wolfram.com/LeastSquaresFitting.html
//
// This program evaluates a track defined as
//   f = a + b z
// a and b in the equation above is determined
// by minimizing the residual R^2 of the straight
// line fit of the data points
//
//  R^2 = sum[(xi - f(zi,a,b))^2/sigi^2]
//
// Adopted the use of det resolution sigi^2
// from Seamus's thesis, pg.51
//
//   -- rupesh, 28 Nov, 2012
//
//  -- copied from track.C in moller/tracking
//
//  -- copied from mollersim/backup_Mar20_2013/include/MollerSimGEM_TrackReconstruct.hh
// 

#ifndef __REMOLLTRACKRECONSTRUCT_HH
#define __REMOLLTRACKRECONSTRUCT_HH

#include "remollGenericDetectorHit.hh"

#include "globals.hh"

class remollTrackReconstruct{

public:
  remollTrackReconstruct();
  ~remollTrackReconstruct();

  void AddHit(remollGenericDetectorHit* aHit);

  G4int GetTrackHitSize(){return rTrackHitSize;};
  void  PrintTrackInfo(){
    PrintHitInfo(aTrackHit);
  };
  void  PrintHitInfo(std::vector<remollGenericDetectorHit*> aHitVec);
  G4int ReconstructTrack();

  std::vector <remollGenericDetectorHit*> GetTrack(){return aTrackHit;};

private:

  void ClearTrack();

  G4int EvaluateTrack(std::vector <G4ThreeVector> Pos, std::vector <G4ThreeVector> res);
  G4ThreeVector EvaluateTrack(std::vector <G4double> rPosX,std::vector <G4double> rPosZ, 
		      std::vector <G4double> rGEMRes);

  void FillRecTrackHit();

  void EvalTheta();

  G4float EvalTrackPos(G4float z,G4ThreeVector &ab);
  G4float EvalTrackAng(G4float z,G4ThreeVector &ab);

  // GEM wire plane tracks
  std::vector<remollGenericDetectorHit*> aTrackHit;
  G4int rTrackHitSize; // size of the aTrackHit vector

  std:: vector <std::vector <G4ThreeVector> > hitPos;
  std:: vector <std::vector <G4ThreeVector> > GEMRes;

  // recTrack has (a,b,0) of 
  //    x = a + bz & y = a + bz
  // recTrack[0] is (a,b) along XZ plane
  // recTrack[1] is (a,b) along YZ plane
  std::vector <G4ThreeVector>  recTrackXZ;
  std::vector <G4ThreeVector>  recTrackYZ;

};

#endif
