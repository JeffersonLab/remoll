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

#include "CLHEP/Random/RandFlat.h"
#include "CLHEP/Random/RandGauss.h"
#include "TMath.h"

#include "remollTrackReconstruct.hh"
#include "remollGenericDetectorHit.hh"

#define TrackingVerbose 0
#define EvalTrackVerbose 0

remollTrackReconstruct::remollTrackReconstruct(){
  ClearTrack();
}

remollTrackReconstruct::~remollTrackReconstruct(){;}

void remollTrackReconstruct::AddHit(remollGenericDetectorHit* aHit){

  aTrackHitOrg.push_back(aHit); // need this to keep track of fCopyID

  // if there is more than one hit per plane, then sort them out by trackID
  // fix me:: need to do something to sort out the case when there is more than 1 hit per plane, but of the same trackID.
  for(int i=0;i<rTrackHitSize;i++){
    if(aTrackHit[i]->fDetID==aHit->fDetID) // look if this planeID has been recorded already
      if(aTrackHit[i]->fTrID!=aHit->fTrID) // if the trackID is different, temporarily flag it using fCopyID
	aHit->fCopyID += 1; // fCopyID==0 by default, for now..
  }

  aTrackHit.push_back(aHit);
  rTrackHitSize = aTrackHit.size();
};

void remollTrackReconstruct::ClearTrack(){

  aTrackHit.clear(); // GEM wire plane track hits
  rTrackHitSize=0;

  hitPos.clear();
  GEMRes.clear();

  recTrackXZ.clear(); // reconstructed track vector, holds (a,b) of chisq minimization
  recTrackYZ.clear(); // reconstructed track vector, holds (a,b) of chisq minimization
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

  G4int copyID=0;
  G4int maxCopyID=1;

  //  G4cout << "aTrackHit.size():: " << rTrackHitSize << G4endl;

  for(size_t i =0; i<rTrackHitSize;i++){

    hitPos.push_back(std::vector <G4ThreeVector>());
    GEMRes.push_back(std::vector <G4ThreeVector>());
    
    copyID=aTrackHit[i]->fCopyID; // usually 0
    if(copyID>maxCopyID) maxCopyID = copyID;

    hitPos[copyID].push_back(aTrackHit[i]->f3X*mm);  // in mm already -- default unit
    
    G4double GEMRES=0.075*mm; // 75 um GEM res
    G4double GEMRES_x = GEMRES;
    G4double GEMRES_y = GEMRES;

    //    G4cout << "GEMRES:: " << GEMRES_x << "\t" << GEMRES_y << G4endl;

    GEMRes[copyID].push_back(G4ThreeVector(GEMRES_x,GEMRES_y,0));

    // G4cout << "copyID:: " << copyID << G4endl;
    // G4cout << "hitPos.size():: " << hitPos[copyID].size() << G4endl;
    // G4cout << "GEMRes.size():: " << GEMRes[copyID].size() << G4endl;
  }

  if(TrackingVerbose)
    G4cout << "--** Number of Hits per Plane :: " << maxCopyID << " ** --"<< G4endl;
  
  for(size_t i=0;i<maxCopyID;i++){
    // G4cout << "hitPos["<<i<< "].size():: " << hitPos[copyID].size() << G4endl;
    EvaluateTrack(hitPos[i],GEMRes[i]); // fills recTrackXZ/YZ for each copyID
  }

  if(TrackingVerbose){
    G4cout << "\nXpos(mm)\tYpos(mm)\tZpos(mm)\tGEMXZres(mm)\tGEMYZres(mm)" << G4endl;
    for(size_t i=0;i<maxCopyID;i++){
      for(size_t j=0;j<hitPos[i].size();j++){
	G4cout << hitPos[i][j].x()/mm <<"\t" << hitPos[i][j].y()/mm <<"\t" << hitPos[i][j].z()/mm << "\t" << GEMRes[i][j].x()/mm << "\t" << GEMRes[i][j].y()/mm << G4endl;
      }
    }
    G4cout << " " <<G4endl;
  }
  
  FillRecTrackHit(); // fills x,y,x
  
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
  std::vector <G4double> rGEMResX;
  std::vector <G4double> rGEMResY;
  
  //  G4cout << "Pos.size():: " << Pos.size() << G4endl;

  for(size_t i=0;i<Pos.size();i++){
    
    // need to smear rPosX & rPosY -> have finite width
    //  TRandom *r1 = new TRandom(123456);
    //r2->Gaus(rPosY,rGEMResY)
    
    // Pos & Res in mm
    rPosX.push_back(CLHEP::RandGauss::shoot(Pos[i].x(),Res[i].x()));
    rPosY.push_back(CLHEP::RandGauss::shoot(Pos[i].y(),Res[i].y()));
    rPosZ.push_back(Pos[i].z());
    
    rGEMResX.push_back(Res[i].x());
    rGEMResY.push_back(Res[i].y());
    
    //    G4cout << rPosX[i] <<"\t" << rPosY[i] <<"\t" << rPosZ[i] << "\t" << rGEMResX[i] << "\t" << rGEMResY[i] << G4endl;
    //    G4cout << TMath::Sqrt(TMath::Power(rPosX[i],2) + TMath::Power(rPosY[i],2)) << G4endl;
  }
 
  //  G4cout << "rPosX.size():: " << rPosX.size() << G4endl;

  // theta is defined along the XZ plane
  // phi is defined along the YZ plane
  
  recTrackXZ.push_back((G4ThreeVector)EvaluateTrack(rPosX,rPosZ,rGEMResX));// evaluate the track variables (a,b) along XZ plane
  recTrackYZ.push_back((G4ThreeVector)EvaluateTrack(rPosY,rPosZ,rGEMResY));// evaluate the track variables (a,b) along YZ plane
  
  return 1;
}

//////////////////////////////////////
  //
  //    // // http://mathworld.wolfram.com/LeastSquaresFitting.html
  // 
  // for a linear fit
  //    f(a,b) = a + bx
  //
  //   R^2(a,b) = sum_i[y_i - (a+b x_i)]^2
  // R is the residual
  // minimize R^2, and extract a,b to get the best fit line
  //   f(a,b)
  // 
  //  matXZ * vecab = vec??
  //
  // Minimization yields
  // 
  //   a = | n          sum_i x_i   |^-1 | y_i     |
  //   b   | sum_i x_i  sum_i x_i^2 |    | x_i y_i |
  //
  //   n => # of GEM planes
  //
  // rPosX,rPosZ,rGEMResX are all in mm, see above
G4ThreeVector remollTrackReconstruct::EvaluateTrack(std::vector <G4double> rPosX,
						    std::vector <G4double> rPosZ, 
						    std::vector <G4double> rGEMResX){
  if(EvalTrackVerbose){
    G4cout << "Entering remollTrackReconstruct::EvaluateTrack(...)" << G4endl;

    for(G4int iPts=0;iPts<rPosX.size();iPts++)
      G4cout << rPosX[iPts] <<"\t" << rPosZ[iPts] << "\t" << rGEMResX[iPts] << G4endl;
  }

  //  G4cout << "rPosX.size():: " << rPosX.size() << G4endl;

  const G4int dim=2;  // xz or yz plane
  G4double matXZ[dim][dim] = {{0}};

  // fill the matrix as
  for(G4int iPts=0;iPts<rPosX.size();iPts++){
    matXZ[0][0] += 1/pow(rGEMResX[iPts],2);
    matXZ[0][1] += rPosZ[iPts]/pow(rGEMResX[iPts],2);
    matXZ[1][0] += rPosZ[iPts]/pow(rGEMResX[iPts],2);
    matXZ[1][1] += pow(rPosZ[iPts],2)/pow(rGEMResX[iPts],2);
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
    vecXZ[0] += rPosX[iPts]/pow(rGEMResX[iPts],2);
    vecXZ[1] += rPosX[iPts]*rPosZ[iPts]/pow(rGEMResX[iPts],2);
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
    //    G4cerr << "** Can't invert the matrix because determinant is ZERO **" << G4endl;
    return G4ThreeVector(-1000/m,-1000/m,0); // in m
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
  // b (vecAB[1]) is in units of rad
  G4double vecAB[dim] = {0};
  for(G4int imat=0;imat<dim;imat++){
    vecAB[imat] = (G4double)(matXZI[imat][0]*vecXZ[0] + matXZI[imat][1]*vecXZ[1]);
  }

  if(TrackingVerbose>0 && EvalTrackVerbose>0){
    printf("Track:");
    printf("\n\tx = %.2f + %.2f z \n",vecAB[0],vecAB[1]);
  }

  if(EvalTrackVerbose)
    G4cout << "Leaving remollTrackReconstruct::EvaluateTrack(...)" << G4endl;

  // vecAB[0] in mm by default
  // vecAB[1] in rad by default
  return G4ThreeVector(vecAB[0],vecAB[1],0);
}


void remollTrackReconstruct::FillRecTrackHit(){

  for(size_t i=0; i<rTrackHitSize;i++){

    G4int copyID=aTrackHit[i]->fCopyID;

    // in mm by default
    //    G4double tmpz = aTrackHit[i]->f3X.z(); // tmpz does not have to be chained to fCopyID because FillRecTrackHit only fills the tracks for 1 GEM box at a time.

    G4double tmpz = 28695.0*mm; // maindet z ** COMMENT ME ** 
      
    // reconstuct positions, recTrackXZ[j] hold (a,b)
    // tmpx/y in mm
    G4double tmpx = EvalTrackPos(tmpz,recTrackXZ[copyID]);
    G4double tmpy = EvalTrackPos(tmpz,recTrackYZ[copyID]);

    // tmpxp/yp in rad
    G4double tmpxp = EvalTrackAng(tmpz,recTrackXZ[copyID]);
    G4double tmpyp = EvalTrackAng(tmpz,recTrackYZ[copyID]);

    // if(TrackingVerbose)
    //   G4cout << "Reconstructed pos (x,y,z): (" <<tmpx << ", " << tmpy << ", "<< tmpz << ") mm" << G4endl;

    // all the tmp vars are in mm
    G4ThreeVector tmp3vec  = G4ThreeVector(tmpx,tmpy,tmpz);
    G4ThreeVector tmp3vecp = G4ThreeVector(tmpxp,tmpyp,0);
    
    // store reconstructed position/angle
    aTrackHit[i]->f3XRec  = tmp3vec;
    aTrackHit[i]->f3dPRec = tmp3vecp;

    aTrackHit[i]->fCopyID = aTrackHitOrg[i]->fCopyID; // reset the copyID to what it is supposed to be
  }

  EvalTheta(); // eval & record theta
  
  if(TrackingVerbose)
    PrintHitInfo(aTrackHit);
}

// evaluate th from each GEM rec vars,
// and average them
void remollTrackReconstruct::EvalTheta(){

  if(EvalTrackVerbose) 
    G4cout << "Entering remollTrackReconstruct::EvalTheta() ..." << G4endl;

  std:: vector <G4double> theta;
  std:: vector <G4double> rec_r;
  std:: vector <G4double> rec_dr;
  std:: vector <G4double> rec_ph;

  //  Double_t det_peak_cntr = 0.78;
  //  Double_t open_cntr = atan(0.084/2/det_peak_cntr)*180/3.14159;
  Double_t open_cntr = 360./7/4;

  theta.resize(rTrackHitSize);
  
  // evaluate th from individual GEM rec vars
  for(G4int i=0;i<rTrackHitSize;i++){
    // // use the original det vars for comparision // -- needs to be changed back
    // rec_r.push_back(aTrackHit[i]->f3X.perp()/m);
    // rec_ph.push_back(aTrackHit[i]->f3X.phi()/deg);
    // rec_dr.push_back(aTrackHit[i]->f3dP.perp());

    rec_r.push_back(aTrackHit[i]->f3XRec.perp()/m);
    rec_ph.push_back(aTrackHit[i]->f3XRec.phi()/deg);
    rec_dr.push_back(aTrackHit[i]->f3dPRec.perp());

    // G4cout << rec_r[i] <<"\t" << rec_ph[i] <<"\t" << rec_dr[i] << G4endl;
  }

  // det quartz is 16.0x8.4x1.5cm
  // ep distribution is centered at r~0.8m in the quartz
  // center: phi at quartz center ~ atan(0.084/2/0.8)*180/3.14159 = 3.00 deg
  // wings: center phi~ atan((0.084+0.042)/0.8)*180/3.14159 = 8.95 deg
  // the fit eqs below are only good for the center & wings of ep
  // use ep to get reconstructed ee th distribution -- how well does this work??
  for(G4int i=0;i<rTrackHitSize;i++){
    // need GEM closest to det because phi changes with z
    // if(aTrackHit[i]->fDetID == 503){ 
    // }
    G4bool cut[] = {
      //Cuts for Open sec1 R1:
      rec_dr[i]>=0.0395 && rec_r[i]<0.77&& TMath::Abs(rec_ph[i]-(180/7.*1-open_cntr/4*-1))<open_cntr/4*1,
      rec_dr[i]>=0.0395 && rec_r[i]<0.77&& TMath::Abs(rec_ph[i]-(180/7.*3-open_cntr/4*-1))<open_cntr/4*1,
      rec_dr[i]>=0.0395 && rec_r[i]<0.77&& TMath::Abs(rec_ph[i]-(180/7.*5-open_cntr/4*-1))<open_cntr/4*1,
      rec_dr[i]>=0.0395 && rec_r[i]<0.77&& TMath::Abs(rec_ph[i]-(180-open_cntr*0)*-1)<open_cntr/2,
      rec_dr[i]>=0.0395 && rec_r[i]<0.77&& TMath::Abs(rec_ph[i]+(180/7.*1+open_cntr/4*-1))<open_cntr/4*1,
      rec_dr[i]>=0.0395 && rec_r[i]<0.77&& TMath::Abs(rec_ph[i]+(180/7.*3+open_cntr/4*-1))<open_cntr/4*1,
      rec_dr[i]>=0.0395 && rec_r[i]<0.77&& TMath::Abs(rec_ph[i]+(180/7.*5+open_cntr/4*-1))<open_cntr/4*1,

      //Cuts for Open sec1 R2:
      rec_dr[i]<0.0395&& TMath::Abs(rec_ph[i]-(180/7.*1-open_cntr/4*-1))<open_cntr/4*1,
      rec_dr[i]<0.0395&& TMath::Abs(rec_ph[i]-(180/7.*3-open_cntr/4*-1))<open_cntr/4*1,
      rec_dr[i]<0.0395&& TMath::Abs(rec_ph[i]-(180/7.*5-open_cntr/4*-1))<open_cntr/4*1,
      rec_dr[i]<0.0395&& TMath::Abs(rec_ph[i]-(180-open_cntr*0)*-1)<open_cntr/2,
      rec_dr[i]<0.0395&& TMath::Abs(rec_ph[i]+(180/7.*1+open_cntr/4*-1))<open_cntr/4*1,
      rec_dr[i]<0.0395&& TMath::Abs(rec_ph[i]+(180/7.*3+open_cntr/4*-1))<open_cntr/4*1,
      rec_dr[i]<0.0395&& TMath::Abs(rec_ph[i]+(180/7.*5+open_cntr/4*-1))<open_cntr/4*1,

      //Cuts for Open sec2 R1:
      rec_dr[i]>=0.0395 && rec_r[i]<0.77&& TMath::Abs(rec_ph[i]-(180/7.*1-open_cntr/4*1))<open_cntr/4*1,
      rec_dr[i]>=0.0395 && rec_r[i]<0.77&& TMath::Abs(rec_ph[i]-(180/7.*3-open_cntr/4*1))<open_cntr/4*1,
      rec_dr[i]>=0.0395 && rec_r[i]<0.77&& TMath::Abs(rec_ph[i]-(180/7.*5-open_cntr/4*1))<open_cntr/4*1,
      rec_dr[i]>=0.0395 && rec_r[i]<0.77&& TMath::Abs(rec_ph[i]-(180-open_cntr*0)*1)<open_cntr/2,
      rec_dr[i]>=0.0395 && rec_r[i]<0.77&& TMath::Abs(rec_ph[i]+(180/7.*1+open_cntr/4*1))<open_cntr/4*1,
      rec_dr[i]>=0.0395 && rec_r[i]<0.77&& TMath::Abs(rec_ph[i]+(180/7.*3+open_cntr/4*1))<open_cntr/4*1,
      rec_dr[i]>=0.0395 && rec_r[i]<0.77&& TMath::Abs(rec_ph[i]+(180/7.*5+open_cntr/4*1))<open_cntr/4*1,

      //Cuts for Open sec2 R2:
      rec_dr[i]<0.0395&& TMath::Abs(rec_ph[i]-(180/7.*1-open_cntr/4*1))<open_cntr/4*1,
      rec_dr[i]<0.0395&& TMath::Abs(rec_ph[i]-(180/7.*3-open_cntr/4*1))<open_cntr/4*1,
      rec_dr[i]<0.0395&& TMath::Abs(rec_ph[i]-(180/7.*5-open_cntr/4*1))<open_cntr/4*1,
      rec_dr[i]<0.0395&& TMath::Abs(rec_ph[i]-(180-open_cntr*0)*1)<open_cntr/2,
      rec_dr[i]<0.0395&& TMath::Abs(rec_ph[i]+(180/7.*1+open_cntr/4*1))<open_cntr/4*1,
      rec_dr[i]<0.0395&& TMath::Abs(rec_ph[i]+(180/7.*3+open_cntr/4*1))<open_cntr/4*1,
      rec_dr[i]<0.0395&& TMath::Abs(rec_ph[i]+(180/7.*5+open_cntr/4*1))<open_cntr/4*1,

      //Cuts for Tran sec 1:
      TMath::Abs(rec_ph[i]-(180/7.*1-open_cntr/4*-4))<open_cntr/4*2,
      TMath::Abs(rec_ph[i]-(180/7.*3-open_cntr/4*-4))<open_cntr/4*2,
      TMath::Abs(rec_ph[i]-(180/7.*5-open_cntr/4*-4))<open_cntr/4*2,
      TMath::Abs(rec_ph[i]-(180-open_cntr*1)*-1)<open_cntr/2,
      TMath::Abs(rec_ph[i]+(180/7.*1+open_cntr/4*-4))<open_cntr/4*2,
      TMath::Abs(rec_ph[i]+(180/7.*3+open_cntr/4*-4))<open_cntr/4*2,
      TMath::Abs(rec_ph[i]+(180/7.*5+open_cntr/4*-4))<open_cntr/4*2,

      //Cuts for Tran sec 2:
      TMath::Abs(rec_ph[i]-(180/7.*1-open_cntr/4*4))<open_cntr/4*2,
      TMath::Abs(rec_ph[i]-(180/7.*3-open_cntr/4*4))<open_cntr/4*2,
      TMath::Abs(rec_ph[i]-(180/7.*5-open_cntr/4*4))<open_cntr/4*2,
      TMath::Abs(rec_ph[i]-(180-open_cntr*1)*1)<open_cntr/2,
      TMath::Abs(rec_ph[i]+(180/7.*1+open_cntr/4*4))<open_cntr/4*2,
      TMath::Abs(rec_ph[i]+(180/7.*3+open_cntr/4*4))<open_cntr/4*2,
      TMath::Abs(rec_ph[i]+(180/7.*5+open_cntr/4*4))<open_cntr/4*2
    };

    // theta in rad by default
    G4double eqn[] = {

      //Eqs for Open sec1 R1:
      (1*0.332284+1*-0.924642*rec_r[i]+1*0.655239*rec_r[i]*rec_r[i])+(1*0.000094+1*-0.000122*(rec_ph[i]-(180/7.*1-12.857143*0))+1*0.000023*TMath::Power((rec_ph[i]-(180/7.*1-12.857143*0)),2)),
      (1*0.332284+1*-0.924642*rec_r[i]+1*0.655239*rec_r[i]*rec_r[i])+(1*0.000094+1*-0.000115*(rec_ph[i]-(180/7.*3-12.857143*0))+1*0.000021*TMath::Power((rec_ph[i]-(180/7.*3-12.857143*0)),2)),
      (1*0.332284+1*-0.924642*rec_r[i]+1*0.655239*rec_r[i]*rec_r[i])+(1*0.000084+1*-0.000120*(rec_ph[i]-(180/7.*5-12.857143*0))+1*0.000020*TMath::Power((rec_ph[i]-(180/7.*5-12.857143*0)),2)),
      (1*0.332284+1*-0.924642*rec_r[i]+1*0.655239*rec_r[i]*rec_r[i])+(1*0.000088+1*-0.000078*(rec_ph[i]>1?(rec_ph[i]-180):(rec_ph[i]+180))+1*0.000010*TMath::Power((rec_ph[i]>1?(rec_ph[i]-180):(rec_ph[i]+180)),2)),
      (1*0.332284+1*-0.924642*rec_r[i]+1*0.655239*rec_r[i]*rec_r[i])+(1*0.000101+1*-0.000133*(rec_ph[i]+(180/7.*1+12.857143*0))+1*0.000022*TMath::Power((rec_ph[i]+(180/7.*1+12.857143*0)),2)),
      (1*0.332284+1*-0.924642*rec_r[i]+1*0.655239*rec_r[i]*rec_r[i])+(1*0.000058+1*-0.000103*(rec_ph[i]+(180/7.*3+12.857143*0))+1*0.000016*TMath::Power((rec_ph[i]+(180/7.*3+12.857143*0)),2)),
      (1*0.332284+1*-0.924642*rec_r[i]+1*0.655239*rec_r[i]*rec_r[i])+(1*0.000069+1*-0.000104*(rec_ph[i]+(180/7.*5+12.857143*0))+1*0.000020*TMath::Power((rec_ph[i]+(180/7.*5+12.857143*0)),2)),

      //Eqs for Open sec1 R2:
      (1*0.068098+1*-2.268090*rec_dr[i]+1*19.882790*rec_dr[i]*rec_dr[i])+(1*-0.000084+1*-0.000007*(rec_ph[i]-(180/7.*1-12.857143*0))+1*0.000008*TMath::Power((rec_ph[i]-(180/7.*1-12.857143*0)),2)),
      (1*0.068098+1*-2.268090*rec_dr[i]+1*19.882790*rec_dr[i]*rec_dr[i])+(1*-0.000089+1*0.000009*(rec_ph[i]-(180/7.*3-12.857143*0))+1*0.000004*TMath::Power((rec_ph[i]-(180/7.*3-12.857143*0)),2)),
      (1*0.068098+1*-2.268090*rec_dr[i]+1*19.882790*rec_dr[i]*rec_dr[i])+(1*-0.000070+1*-0.000007*(rec_ph[i]-(180/7.*5-12.857143*0))+1*0.000008*TMath::Power((rec_ph[i]-(180/7.*5-12.857143*0)),2)),
      (1*0.068098+1*-2.268090*rec_dr[i]+1*19.882790*rec_dr[i]*rec_dr[i])+(1*-0.000121+1*0.000022*(rec_ph[i]>1?(rec_ph[i]-180):(rec_ph[i]+180))+1*0.000004*TMath::Power((rec_ph[i]>1?(rec_ph[i]-180):(rec_ph[i]+180)),2)),
      (1*0.068098+1*-2.268090*rec_dr[i]+1*19.882790*rec_dr[i]*rec_dr[i])+(1*-0.000075+1*-0.000012*(rec_ph[i]+(180/7.*1+12.857143*0))+1*0.000009*TMath::Power((rec_ph[i]+(180/7.*1+12.857143*0)),2)),
      (1*0.068098+1*-2.268090*rec_dr[i]+1*19.882790*rec_dr[i]*rec_dr[i])+(1*-0.000124+1*0.000015*(rec_ph[i]+(180/7.*3+12.857143*0))+1*0.000006*TMath::Power((rec_ph[i]+(180/7.*3+12.857143*0)),2)),
      (1*0.068098+1*-2.268090*rec_dr[i]+1*19.882790*rec_dr[i]*rec_dr[i])+(1*-0.000097+1*0.000008*(rec_ph[i]+(180/7.*5+12.857143*0))+1*0.000006*TMath::Power((rec_ph[i]+(180/7.*5+12.857143*0)),2)),

      //Eqs for Open sec2 R1:
      (1*0.358687+1*-0.996040*rec_r[i]+1*0.703479*rec_r[i]*rec_r[i])+(1*0.000097+1*0.000116*(rec_ph[i]-(180/7.*1-12.857143*0))+1*0.000017*TMath::Power((rec_ph[i]-(180/7.*1-12.857143*0)),2)),
      (1*0.358687+1*-0.996040*rec_r[i]+1*0.703479*rec_r[i]*rec_r[i])+(1*0.000087+1*0.000108*(rec_ph[i]-(180/7.*3-12.857143*0))+1*0.000019*TMath::Power((rec_ph[i]-(180/7.*3-12.857143*0)),2)),
      (1*0.358687+1*-0.996040*rec_r[i]+1*0.703479*rec_r[i]*rec_r[i])+(1*0.000098+1*0.000095*(rec_ph[i]-(180/7.*5-12.857143*0))+1*0.000015*TMath::Power((rec_ph[i]-(180/7.*5-12.857143*0)),2)),
      (1*0.358687+1*-0.996040*rec_r[i]+1*0.703479*rec_r[i]*rec_r[i])+(1*0.000117+1*0.000148*(rec_ph[i]>1?(rec_ph[i]-180):(rec_ph[i]+180))+1*0.000026*TMath::Power((rec_ph[i]>1?(rec_ph[i]-180):(rec_ph[i]+180)),2)),
      (1*0.358687+1*-0.996040*rec_r[i]+1*0.703479*rec_r[i]*rec_r[i])+(1*0.000103+1*0.000085*(rec_ph[i]+(180/7.*1+12.857143*0))+1*0.000011*TMath::Power((rec_ph[i]+(180/7.*1+12.857143*0)),2)),
      (1*0.358687+1*-0.996040*rec_r[i]+1*0.703479*rec_r[i]*rec_r[i])+(1*0.000111+1*0.000142*(rec_ph[i]+(180/7.*3+12.857143*0))+1*0.000022*TMath::Power((rec_ph[i]+(180/7.*3+12.857143*0)),2)),
      (1*0.358687+1*-0.996040*rec_r[i]+1*0.703479*rec_r[i]*rec_r[i])+(1*0.000072+1*0.000075*(rec_ph[i]+(180/7.*5+12.857143*0))+1*0.000012*TMath::Power((rec_ph[i]+(180/7.*5+12.857143*0)),2)),

      //Eqs for Open sec2 R2:
      (1*0.068087+1*-2.266499*rec_dr[i]+1*19.846172*rec_dr[i]*rec_dr[i])+(1*-0.000103+1*-0.000022*(rec_ph[i]-(180/7.*1-12.857143*0))+1*0.000003*TMath::Power((rec_ph[i]-(180/7.*1-12.857143*0)),2)),
      (1*0.068087+1*-2.266499*rec_dr[i]+1*19.846172*rec_dr[i]*rec_dr[i])+(1*-0.000082+1*0.000005*(rec_ph[i]-(180/7.*3-12.857143*0))+1*0.000008*TMath::Power((rec_ph[i]-(180/7.*3-12.857143*0)),2)),
      (1*0.068087+1*-2.266499*rec_dr[i]+1*19.846172*rec_dr[i]*rec_dr[i])+(1*-0.000115+1*-0.000031*(rec_ph[i]-(180/7.*5-12.857143*0))+1*0.000002*TMath::Power((rec_ph[i]-(180/7.*5-12.857143*0)),2)),
      (1*0.068087+1*-2.266499*rec_dr[i]+1*19.846172*rec_dr[i]*rec_dr[i])+(1*-0.000078+1*0.000004*(rec_ph[i]>1?(rec_ph[i]-180):(rec_ph[i]+180))+1*0.000007*TMath::Power((rec_ph[i]>1?(rec_ph[i]-180):(rec_ph[i]+180)),2)),
      (1*0.068087+1*-2.266499*rec_dr[i]+1*19.846172*rec_dr[i]*rec_dr[i])+(1*-0.000105+1*-0.000021*(rec_ph[i]+(180/7.*1+12.857143*0))+1*0.000003*TMath::Power((rec_ph[i]+(180/7.*1+12.857143*0)),2)),
      (1*0.068087+1*-2.266499*rec_dr[i]+1*19.846172*rec_dr[i]*rec_dr[i])+(1*-0.000088+1*-0.000002*(rec_ph[i]+(180/7.*3+12.857143*0))+1*0.000006*TMath::Power((rec_ph[i]+(180/7.*3+12.857143*0)),2)),
      (1*0.068087+1*-2.266499*rec_dr[i]+1*19.846172*rec_dr[i]*rec_dr[i])+(1*-0.000081+1*0.000002*(rec_ph[i]+(180/7.*5+12.857143*0))+1*0.000007*TMath::Power((rec_ph[i]+(180/7.*5+12.857143*0)),2)),

      //Eqs for Tran sec 1:
      (1*0.044040+1*-0.915601*rec_dr[i]+1*1.468913*rec_dr[i]*rec_dr[i])+(1*0.000202+1*0.000077*(rec_ph[i]-(180/7.*1-12.857143*-1))+1*-0.000001*TMath::Power((rec_ph[i]-(180/7.*1-12.857143*-1)),2)),
      (1*0.044040+1*-0.915601*rec_dr[i]+1*1.468913*rec_dr[i]*rec_dr[i])+(1*0.000202+1*0.000068*(rec_ph[i]-(180/7.*3-12.857143*-1))+1*-0.000003*TMath::Power((rec_ph[i]-(180/7.*3-12.857143*-1)),2)),
      (1*0.044040+1*-0.915601*rec_dr[i]+1*1.468913*rec_dr[i]*rec_dr[i])+(1*0.000208+1*0.000078*(rec_ph[i]-(180/7.*5-12.857143*-1))+1*-0.000002*TMath::Power((rec_ph[i]-(180/7.*5-12.857143*-1)),2)),
      (1*0.044040+1*-0.915601*rec_dr[i]+1*1.468913*rec_dr[i]*rec_dr[i])+(1*0.000188+1*0.000079*(rec_ph[i]-(180-12.857143)*-1)+1*-0.000000*TMath::Power((rec_ph[i]-(180-12.857143)*-1),2)),
      (1*0.044040+1*-0.915601*rec_dr[i]+1*1.468913*rec_dr[i]*rec_dr[i])+(1*0.000194+1*0.000080*(rec_ph[i]+(180/7.*1+12.857143*-1))+1*-0.000001*TMath::Power((rec_ph[i]+(180/7.*1+12.857143*-1)),2)),
      (1*0.044040+1*-0.915601*rec_dr[i]+1*1.468913*rec_dr[i]*rec_dr[i])+(1*0.000231+1*0.000071*(rec_ph[i]+(180/7.*3+12.857143*-1))+1*-0.000003*TMath::Power((rec_ph[i]+(180/7.*3+12.857143*-1)),2)),
      (1*0.044040+1*-0.915601*rec_dr[i]+1*1.468913*rec_dr[i]*rec_dr[i])+(1*0.000181+1*0.000080*(rec_ph[i]+(180/7.*5+12.857143*-1))+1*-0.000000*TMath::Power((rec_ph[i]+(180/7.*5+12.857143*-1)),2)),

      //Eqs for Tran sec 2:
      (1*0.044763+1*-0.957440*rec_dr[i]+1*2.066556*rec_dr[i]*rec_dr[i])+(1*0.000196+1*-0.000075*(rec_ph[i]-(180/7.*1-12.857143*1))+1*-0.000001*TMath::Power((rec_ph[i]-(180/7.*1-12.857143*1)),2)),
      (1*0.044763+1*-0.957440*rec_dr[i]+1*2.066556*rec_dr[i]*rec_dr[i])+(1*0.000226+1*-0.000076*(rec_ph[i]-(180/7.*3-12.857143*1))+1*-0.000003*TMath::Power((rec_ph[i]-(180/7.*3-12.857143*1)),2)),
      (1*0.044763+1*-0.957440*rec_dr[i]+1*2.066556*rec_dr[i]*rec_dr[i])+(1*0.000211+1*-0.000082*(rec_ph[i]-(180/7.*5-12.857143*1))+1*-0.000000*TMath::Power((rec_ph[i]-(180/7.*5-12.857143*1)),2)),
      (1*0.044763+1*-0.957440*rec_dr[i]+1*2.066556*rec_dr[i]*rec_dr[i])+(1*0.000178+1*-0.000075*(rec_ph[i]-(180-12.857143)*1)+1*-0.000001*TMath::Power((rec_ph[i]-(180-12.857143)*1),2)),
      (1*0.044763+1*-0.957440*rec_dr[i]+1*2.066556*rec_dr[i]*rec_dr[i])+(1*0.000231+1*-0.000075*(rec_ph[i]+(180/7.*1+12.857143*1))+1*-0.000002*TMath::Power((rec_ph[i]+(180/7.*1+12.857143*1)),2)),
      (1*0.044763+1*-0.957440*rec_dr[i]+1*2.066556*rec_dr[i]*rec_dr[i])+(1*0.000179+1*-0.000076*(rec_ph[i]+(180/7.*3+12.857143*1))+1*-0.000001*TMath::Power((rec_ph[i]+(180/7.*3+12.857143*1)),2)),
      (1*0.044763+1*-0.957440*rec_dr[i]+1*2.066556*rec_dr[i]*rec_dr[i])+(1*0.000195+1*-0.000073*(rec_ph[i]+(180/7.*5+12.857143*1))+1*-0.000001*TMath::Power((rec_ph[i]+(180/7.*5+12.857143*1)),2))
    };
    
    for(int ii=0;ii<42;ii++){
      if(cut[ii]){
	theta[i] = eqn[ii];  // in rad by default

	// G4cout <<"cut[ii]:: " << cut[ii] << G4endl;
	// G4cout <<"eqn[ii]:: " << eqn[ii] << G4endl;
	// G4cout <<"rec_ph[i]:: " << rec_ph[i] << G4endl;
	break;
      }
      else 
	theta[i] = -1.0;
    }

    if(EvalTrackVerbose){
      G4cout << "Org:: " << aTrackHit[i]->f3X.perp()/m << "\t" << aTrackHit[i]->f3X.phi()/deg << "\t" << aTrackHit[i]->f3dP.perp() << "\t" << G4endl;
      G4cout << "Rec:: " << rec_r[i] << "\t" << rec_ph[i] << "\t" << rec_dr[i] << "\t" << theta[i] << G4endl;
    }
  }
  for(G4int i=0;i<rTrackHitSize;i++)
    aTrackHit[i]->fThRec = theta[i];  // in rad by default
  
  if(EvalTrackVerbose)
    G4cout << "Leaving remollTrackReconstruct::EvalTheta() ..." << G4endl;
}

// return y = a + bx
G4double remollTrackReconstruct::EvalTrackPos(G4double z,G4ThreeVector &ab){

  G4double aVal = ab.x();  // in mm
  G4double bVal = ab.y();  // in rad

  G4double yVal = aVal + bVal*z;   // z is in mm

  return yVal;  // in mm
}

// return the direction angle, which is just the slope
G4double remollTrackReconstruct::EvalTrackAng(G4double z,G4ThreeVector &ab){

  G4double bVal = ab.y();  // in rad

  return bVal;
}
