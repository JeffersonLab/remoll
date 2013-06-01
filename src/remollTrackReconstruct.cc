// http://mathworld.wolfram.com/LeastSquaresFitting.html
//
// This program evaluates a track defined as
//   f = a + b z
// a and b in the equation above is determined
// by minimizing the residual R^2 of the straight
// line fit of the data points
//
//  R^2 = sum[(x_i - f(z_i,a,b))^2/sig_i^2]
//
// Adopted the use of det resolution sig_i^2
// from Seamus's thesis, pg.51
//
//   -- rupesh, 28 Nov, 2012
//
//  -- adopted from  track.C in moller/tracking
//  -- rupesh, Jan4, 2012

#include "remollTrackReconstruct.hh"
#include "remollGenericDetectorHit.hh"

#define TrackingVerbose 0
#define EvalTrackVerbose 0

//#include "RootAnalysis.hh"

remollTrackReconstruct::remollTrackReconstruct(){
  ClearTrack();
}

remollTrackReconstruct::~remollTrackReconstruct(){;}

void remollTrackReconstruct::AddHit(remollGenericDetectorHit* aHit){

  // label the number of hits for each plane by fCopyID
  for(int i=0;i<rTrackHitSize;i++){
    if(aTrackHit[i]->fDetID == aHit->fDetID) 
      aHit->fCopyID += 1;
  }

  // original track vector
  aTrackHit.push_back(aHit);

  rTrackHitSize = aTrackHit.size();

  // reconstructed track vector
  aRecTrackHit.resize(rTrackHitSize);
};

void remollTrackReconstruct::ClearTrack(){

  aTrackHit.clear(); // GEM wire plane track hits
  rTrackHitSize = 0;

  hitPos.clear();
  GEMRes.clear();

  recTrackXZ.clear(); // reconstructed track vector, holds (a,b) of chisq minimization
  recTrackYZ.clear(); // reconstructed track vector, holds (a,b) of chisq minimization

  aRecTrackHit.clear(); // reconstructed GEM wire plane tracks
};


void remollTrackReconstruct::PrintHitInfo(std::vector<remollGenericDetectorHit*> aHitVec){

  G4int aHitVecSize = aHitVec.size();

  for(size_t i =0; i<aHitVecSize;i++){
    G4cout << "Info for Hit# " << i+1 << G4endl;
    aHitVec[i]->Print();
  }
}

G4int remollTrackReconstruct::ReconstructTrack(){

  if(TrackingVerbose)
    G4cout << "\n***** Calling remollTrackReconstruct::ReconstructTrack() *****\n" << G4endl;

  // return if no tracks
  if(rTrackHitSize==0) return 0;

  //  G4float dataXZ[5][2] = {{2,1},{1,2},{2,3},{1,4},{3,5}};
  G4int copyID=0;
  G4int maxCopyID=0;

  for(size_t i =0; i<rTrackHitSize;i++){

    hitPos.push_back(std::vector <G4ThreeVector>());
    GEMRes.push_back(std::vector <G4ThreeVector>());
    
    copyID=aTrackHit[i]->fCopyID;
    if(copyID>maxCopyID) maxCopyID = copyID;

    hitPos[copyID].push_back(aTrackHit[i]->f3X);
    GEMRes[copyID].push_back(G4ThreeVector(0.5*mm,0.5*mm,0));
  }

  if(TrackingVerbose)
    G4cout << "--** Number of Hits per Plane :: " << maxCopyID+1 << " ** --"<< G4endl;
  
  for(size_t i=0;i<=maxCopyID;i++){
    EvaluateTrack(hitPos[i],GEMRes[i]); // fills recTrackXZ/YZ for each copyID
  }

  if(TrackingVerbose){
    G4cout << "\tXpos(mm)\tYpos(mm)\tZpos(mm)\tGEMXZres(mm)\tGEMYZres(mm)" << G4endl;
    for(size_t i=0;i<=maxCopyID;i++){
      for(size_t j=0;j<hitPos[i].size();j++){
	G4cout << hitPos[i][j].x()/mm <<"\t" << hitPos[i][j].y()/mm <<"\t" << hitPos[i][j].z()/mm << "\t" << GEMRes[i][j].x()/mm << "\t" << GEMRes[i][j].y()/mm << G4endl;
      }
    }
  }
  
  FillRecTrackHit();
  
  if(TrackingVerbose)
    G4cout << "\n***** Leaving remollTrackReconstruct::ReconstructTrack() *****\n" << G4endl;
  
  return 1;
}


// calls EvaluateTrack(XVec,ZVec,XResVec,ZResVec)
//     EvaluateTrack(YVec,ZVec,YResVec,ZResVec)
G4int remollTrackReconstruct::EvaluateTrack(std::vector <G4ThreeVector> Pos, 
					    std::vector <G4ThreeVector> Res){
  std::vector <G4double> rPosX;
  std::vector <G4double> rPosY;
  std::vector <G4double> rPosZ;
  std::vector <G4double> rGEMRes;
  
  for(size_t i=0;i<Pos.size();i++){
    
    rPosX.push_back(Pos[i].x()/mm);
    rPosY.push_back(Pos[i].y()/mm);
    rPosZ.push_back(Pos[i].z()/mm);
    
    rGEMRes.push_back(Res[i].x()/mm);
    
    //    G4cout << rPosX[i] <<"\t" << rPosY[i] <<"\t" << rPosZ[i] << "\t" << rGEMRes[i] << G4endl;
  }
  
  // theta is defined along the XZ plane, see remollWirePlaneSD.cc
  // phi is defined along the YZ plane
  
  recTrackXZ.push_back((G4ThreeVector)EvaluateTrack(rPosX,rPosZ,rGEMRes));// evaluate the track variables (a,b) along XZ plane
  recTrackYZ.push_back((G4ThreeVector)EvaluateTrack(rPosY,rPosZ,rGEMRes));// evaluate the track variables (a,b) along YZ plane
  
  return 1;
}

//////////////////////////////////////
  //
  //    // // http://mathworld.wolfram.com/LeastSquaresFitting.html
  // 
  // for a linear fit
  //    f(a,b) = a + bx
  //
  //   R^2(a,b) = sum_i[y_i -(a+b x_i)]^2
  // R is the residual
  // minimize R^2, and extract a,b to get the best fit line
  //   f(a,b)
  // 
  // Minimization yields
  // 
  //   a = | n          sum_i x_i   |^-1 | y_i     |
  //   b   | sum_i x_i  sum_i x_i^2 |    | x_i y_i |
  //
  //   n => # of GEM planes
  //
  // x,y,res are all in mm, see above
G4ThreeVector remollTrackReconstruct::EvaluateTrack(std::vector <G4double> rPosX,
						    std::vector <G4double> rPosZ, 
						    std::vector <G4double> rGEMRes){
  if(EvalTrackVerbose)
    G4cout << "Entering remollTrackReconstruct::EvaluateTrack(...)" << G4endl;

  const G4int dim=2;  
  G4double matXZ[dim][dim] = {{0}};

  // fill the matrix as
  for(G4int iPts=0;iPts<rPosX.size();iPts++){
    matXZ[0][0] += 1/pow(rGEMRes[iPts],2);
    matXZ[0][1] += rPosZ[iPts]/pow(rGEMRes[iPts],2);
    matXZ[1][0] += rPosZ[iPts]/pow(rGEMRes[iPts],2);
    matXZ[1][1] += pow(rPosZ[iPts],2)/pow(rGEMRes[iPts],2);
  }

  if(TrackingVerbose>0 && EvalTrackVerbose>0){
    printf("Matrix:\n");
    for(G4int imat=0;imat<dim;imat++){
      printf("\t%.2f\t%.2f\n",matXZ[imat][0],matXZ[imat][1]);
    }
  }

  // create and fill a vector as
  G4double vecXZ[dim] = {0};
  for(G4int iPts=0;iPts<rPosX.size();iPts++){
    vecXZ[0] += rPosX[iPts]/pow(rGEMRes[iPts],2);
    vecXZ[1] += rPosX[iPts]*rPosZ[iPts]/pow(rGEMRes[iPts],2);
  }

  if(TrackingVerbose>0 && EvalTrackVerbose>0){
    printf("Data vector:");
    for(G4int idim=0;idim<dim;idim++)
      printf("\n\t%.2f",vecXZ[idim]);
    printf("\n");
  }

  // now need to evaluate vecAB = matXZ^-1 * vecXZ
  // invert matXZ
  G4double detXZ = matXZ[0][0]*matXZ[1][1] - matXZ[0][1]*matXZ[1][0];

  if(!detXZ){
    G4cerr << "** Can't invert the matrix because determinant is ZERO **" << G4endl;
    exit (EXIT_FAILURE);
  }
  
  G4double matXZI[dim][dim] ={{0}};
  // now the inverse of matXZ
  matXZI[0][0] = 1/detXZ * matXZ[1][1];
  matXZI[0][1] = -1/detXZ * matXZ[0][1];
  matXZI[1][0] = -1/detXZ * matXZ[1][0];
  matXZI[1][1] = 1/detXZ * matXZ[0][0];

  if(TrackingVerbose>0 && EvalTrackVerbose>0){
    printf("Inverted Matrix:\n");
    for(G4int imat=0;imat<dim;imat++){
      printf("\t%.2f\t%.2f\n",matXZI[imat][0],matXZI[imat][1]);
    }
  }

  // now evaluate vecAB
  // a (vecAB[0]) is in units of mm
  // b (vecAB[1]) is in units of 1/mm
  G4float vecAB[dim] = {0};
  for(G4int imat=0;imat<dim;imat++){
    vecAB[imat] = (G4float)(matXZI[imat][0]*vecXZ[0] + matXZI[imat][1]*vecXZ[1]);
  }

  if(TrackingVerbose>0 && EvalTrackVerbose>0){
    printf("Track:");
    printf("\n\tx = %.2f + %.2f z \n",vecAB[0],vecAB[1]);
  }

  if(EvalTrackVerbose)
    G4cout << "Leaving remollTrackReconstruct::EvaluateTrack(...)" << G4endl;

  return G4ThreeVector(vecAB[0],vecAB[1],0); // in mm
}


void remollTrackReconstruct::FillRecTrackHit(){

  // first copy aTrackHit into aRecTrackHit
  for(size_t i =0; i<rTrackHitSize;i++){

    aRecTrackHit[i] = aTrackHit[i];

    // assign unique detID for reconstructed tracks
    aRecTrackHit[i]->fDetID = aRecTrackHit[i]->fDetID + 100; 
  }

  // now rewrite relevant variables in aRecTrackHit
  // if(TrackingVerbose){
  //   G4cout << "\tXpos(mm)\tYpos(mm)\tZpos(mm)" << G4endl;
  // }


  for(size_t i=0; i<rTrackHitSize;i++){

    G4int copyID=aTrackHit[i]->fCopyID;

    G4float tmpz = aTrackHit[i]->f3X.z()/mm; // tmpz does not have to be chained to fCopyID because FillRecTrackHit only fills the tracks for 1 GEM box at a time.
      
    // reconstuct positions, recTrackXZ[j] hold (a,b)
    G4float tmpx = EvalTrackPos(tmpz,recTrackXZ[copyID]);
    G4float tmpy = EvalTrackPos(tmpz,recTrackYZ[copyID]);
      
    if(TrackingVerbose)
      G4cout << "Reconstructed pos (x,y,z): (" <<tmpx << ", " << tmpy << ", "<< tmpz << ") mm" << G4endl;

    // all the tmp vars are in mm
    G4ThreeVector tmp3vec = G4ThreeVector(tmpx,tmpy,tmpz);
    
    // store reconstructed positions
    //    aRecTrackHit[i]->StorePreStepLocalPos(tmp3vec);
    aRecTrackHit[i]->f3X = tmp3vec;
    
    // // reconstuct angles
    // G4float tmpTh = EvalTrackAng(tmpz,recTrack[0]);
    // G4float tmpPh = EvalTrackAng(tmpz,recTrack[1]);
    
    // G4cout << "Reconstructed angles (th,ph): (" << tmpTh << ", " << tmpPh << ") rad" << G4endl;
    
    // // store reconstructed angles
    // aRecTrackHit[i]->StoreLocalTheta(tmpTh);
    // aRecTrackHit[i]->StoreLocalPhi(tmpPh);
  }
  
  // store reconstructed angles
  //     aRecTrackHit[i]->StoreWorldTheta(G4double th);
  
  if(TrackingVerbose)
    PrintHitInfo(aRecTrackHit);
}

// return y = a + bx
G4float remollTrackReconstruct::EvalTrackPos(G4float z,G4ThreeVector &ab){

  G4float aVal = ab.x()/mm; 
  G4float bVal = ab.y()/mm;

  // z in mm
  return aVal + bVal*z;
}

// evaluate the angles, which are just slopes
G4float remollTrackReconstruct::EvalTrackAng(G4float z,G4ThreeVector &ab){

  //  G4float aVal = ab.x(); 
  G4float bVal = ab.y()/mm; // in 1/mm

  return bVal;
}
