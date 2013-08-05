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

  // label the number of hits for each plane by fCopyID
  for(int i=0;i<rTrackHitSize;i++){
    if(aTrackHit[i]->fDetID == aHit->fDetID) 
      aHit->fCopyID += 1;
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
  G4int maxCopyID=0;

  for(size_t i =0; i<rTrackHitSize;i++){

    hitPos.push_back(std::vector <G4ThreeVector>());
    GEMRes.push_back(std::vector <G4ThreeVector>());
    
    copyID=aTrackHit[i]->fCopyID;
    if(copyID>maxCopyID) maxCopyID = copyID;

    hitPos[copyID].push_back(aTrackHit[i]->f3X);
    
    G4double GEMRES=0.5; // 500 um GEM res
    G4double GEMRES_x = CLHEP::RandFlat::shoot(GEMRES*0.8,GEMRES); 
    G4double GEMRES_y = CLHEP::RandFlat::shoot(GEMRES*0.8,GEMRES); 

    //    G4cout << "GEMRES:: " << GEMRES_x << "\t" << GEMRES_y << G4endl;

    GEMRes[copyID].push_back(G4ThreeVector(GEMRES_x*mm,GEMRES_y*mm,0));
  }

  if(TrackingVerbose)
    G4cout << "--** Number of Hits per Plane :: " << maxCopyID+1 << " ** --"<< G4endl;
  
  for(size_t i=0;i<=maxCopyID;i++){
    EvaluateTrack(hitPos[i],GEMRes[i]); // fills recTrackXZ/YZ for each copyID
  }

  if(TrackingVerbose){
    G4cout << "\nXpos(mm)\tYpos(mm)\tZpos(mm)\tGEMXZres(mm)\tGEMYZres(mm)" << G4endl;
    for(size_t i=0;i<=maxCopyID;i++){
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
  
  for(size_t i=0;i<Pos.size();i++){
    
    rPosX.push_back(Pos[i].x()/mm);
    rPosY.push_back(Pos[i].y()/mm);
    rPosZ.push_back(Pos[i].z()/mm);
    
    rGEMResX.push_back(Res[i].x()/mm);
    rGEMResY.push_back(Res[i].y()/mm);
    
    //    G4cout << rPosX[i] <<"\t" << rPosY[i] <<"\t" << rPosZ[i] << "\t" << rGEMResX[i] << "\t" << rGEMResY[i] << G4endl;
  }
 
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
						    std::vector <G4double> rGEMResX){
  if(EvalTrackVerbose){
    G4cout << "Entering remollTrackReconstruct::EvaluateTrack(...)" << G4endl;

    for(G4int iPts=0;iPts<rPosX.size();iPts++)
      G4cout << rPosX[iPts] <<"\t" << rPosZ[iPts] << "\t" << rGEMResX[iPts] << G4endl;
  }

  const G4int dim=2;  
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

  return G4ThreeVector(vecAB[0]/mm,vecAB[1]/rad,0);
}


void remollTrackReconstruct::FillRecTrackHit(){

  for(size_t i=0; i<rTrackHitSize;i++){

    G4int copyID=aTrackHit[i]->fCopyID;

    G4double tmpz = aTrackHit[i]->f3X.z()/mm; // tmpz does not have to be chained to fCopyID because FillRecTrackHit only fills the tracks for 1 GEM box at a time.
      
    // reconstuct positions, recTrackXZ[j] hold (a,b)
    G4double tmpx = EvalTrackPos(tmpz,recTrackXZ[copyID])/mm;
    G4double tmpy = EvalTrackPos(tmpz,recTrackYZ[copyID])/mm;

    G4double tmpxp = EvalTrackAng(tmpz,recTrackXZ[copyID])/rad;
    G4double tmpyp = EvalTrackAng(tmpz,recTrackYZ[copyID])/rad;

    // if(TrackingVerbose)
    //   G4cout << "Reconstructed pos (x,y,z): (" <<tmpx << ", " << tmpy << ", "<< tmpz << ") mm" << G4endl;

    // all the tmp vars are in mm
    G4ThreeVector tmp3vec = G4ThreeVector(tmpx,tmpy,tmpz);
    G4ThreeVector tmp3vecp = G4ThreeVector(tmpxp,tmpyp,0);
    
    // store reconstructed position/angle
    aTrackHit[i]->f3XRec = tmp3vec;    
    aTrackHit[i]->f3dPRec = tmp3vecp;
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

  Double_t det_peak_cntr = 0.78;
  Double_t open_cntr = atan(0.084/2/det_peak_cntr)*180/3.14159;
  // Double_t GEM_peak_cntr = 0.78;
  // Double_t GEM_open_cntr = open_cntr*GEM_peak_cntr/open_cntr;

  //  G4bool region[]={0,0,0};

  theta.resize(rTrackHitSize);
  
  // evaluate th from individual GEM rec vars
  for(G4int i=0;i<rTrackHitSize;i++){
    rec_r.push_back(aTrackHit[i]->f3XRec.perp()/m);
    rec_ph.push_back(aTrackHit[i]->f3XRec.phi()/deg);
    rec_dr.push_back(aTrackHit[i]->f3dPRec.perp());
    
    // det quartz is 16.0x8.4x1.5cm
    // ep distribution is centered at r~0.8m in the quartz
    // center: phi at quartz center ~ atan(0.084/2/0.8)*180/3.14159 = 3.00 deg
    // wings: center phi~ atan((0.084+0.042)/0.8)*180/3.14159 = 8.95 deg
    // the fit eqs below are only good for the center & wings of ep
    // use ep to get reconstructed ee th distribution -- how well does this work??

    G4bool cut[] = {
      //Cuts for Open sec (dr>=0.0395 && r<0.765):
      (rec_dr[i]>=0.0395 && rec_r[i]<0.765 && TMath::Abs(rec_ph[i]-(180/7.*1-2*open_cntr*0))<open_cntr),
      (rec_dr[i]>=0.0395 && rec_r[i]<0.765 && TMath::Abs(rec_ph[i]-(180/7.*3-2*open_cntr*0))<open_cntr),
      (rec_dr[i]>=0.0395 && rec_r[i]<0.765 && TMath::Abs(rec_ph[i]-(180/7.*5-2*open_cntr*0))<open_cntr),
      (rec_dr[i]>=0.0395 && rec_r[i]<0.765 && TMath::Abs(TMath::Abs(rec_ph[i])-(180-2*open_cntr*0))<open_cntr),
      (rec_dr[i]>=0.0395 && rec_r[i]<0.765 && TMath::Abs(rec_ph[i]+(180/7.*1+2*open_cntr*0))<open_cntr),
      (rec_dr[i]>=0.0395 && rec_r[i]<0.765 && TMath::Abs(rec_ph[i]+(180/7.*3+2*open_cntr*0))<open_cntr),
      (rec_dr[i]>=0.0395 && rec_r[i]<0.765 && TMath::Abs(rec_ph[i]+(180/7.*5+2*open_cntr*0))<open_cntr),

      //Cuts for Open sec (dr>=0.0395 && r>=0.765):
      (rec_dr[i]>=0.0395 && rec_r[i]>=0.765 && TMath::Abs(rec_ph[i]-(180/7.*1-2*open_cntr*0))<open_cntr),
      (rec_dr[i]>=0.0395 && rec_r[i]>=0.765 && TMath::Abs(rec_ph[i]-(180/7.*3-2*open_cntr*0))<open_cntr),
      (rec_dr[i]>=0.0395 && rec_r[i]>=0.765 && TMath::Abs(rec_ph[i]-(180/7.*5-2*open_cntr*0))<open_cntr),
      (rec_dr[i]>=0.0395 && rec_r[i]>=0.765 && TMath::Abs(TMath::Abs(rec_ph[i])-(180-2*open_cntr*0))<open_cntr),
      (rec_dr[i]>=0.0395 && rec_r[i]>=0.765 && TMath::Abs(rec_ph[i]+(180/7.*1+2*open_cntr*0))<open_cntr),
      (rec_dr[i]>=0.0395 && rec_r[i]>=0.765 && TMath::Abs(rec_ph[i]+(180/7.*3+2*open_cntr*0))<open_cntr),
      (rec_dr[i]>=0.0395 && rec_r[i]>=0.765 && TMath::Abs(rec_ph[i]+(180/7.*5+2*open_cntr*0))<open_cntr),

      //Cuts for Open sec (dr<0.0395):
      (rec_dr[i]<0.0395 && TMath::Abs(rec_ph[i]-(180/7.*1-2*open_cntr*0))<open_cntr),
      (rec_dr[i]<0.0395 && TMath::Abs(rec_ph[i]-(180/7.*3-2*open_cntr*0))<open_cntr),
      (rec_dr[i]<0.0395 && TMath::Abs(rec_ph[i]-(180/7.*5-2*open_cntr*0))<open_cntr),
      (rec_dr[i]<0.0395 && TMath::Abs(TMath::Abs(rec_ph[i])-(180-2*open_cntr*0))<open_cntr),
      (rec_dr[i]<0.0395 && TMath::Abs(rec_ph[i]+(180/7.*1+2*open_cntr*0))<open_cntr),
      (rec_dr[i]<0.0395 && TMath::Abs(rec_ph[i]+(180/7.*3+2*open_cntr*0))<open_cntr),
      (rec_dr[i]<0.0395 && TMath::Abs(rec_ph[i]+(180/7.*5+2*open_cntr*0))<open_cntr),

      //Cuts for Closed sec 1:
      (TMath::Abs(rec_ph[i]-(180/7.*1-2*open_cntr*-1))<open_cntr),
      (TMath::Abs(rec_ph[i]-(180/7.*3-2*open_cntr*-1))<open_cntr),
      (TMath::Abs(rec_ph[i]-(180/7.*5-2*open_cntr*-1))<open_cntr),
      (TMath::Abs(rec_ph[i]-(180-2*open_cntr)*-1)<open_cntr),
      (TMath::Abs(rec_ph[i]+(180/7.*1+2*open_cntr*-1))<open_cntr),
      (TMath::Abs(rec_ph[i]+(180/7.*3+2*open_cntr*-1))<open_cntr),
      (TMath::Abs(rec_ph[i]+(180/7.*5+2*open_cntr*-1))<open_cntr),

      //Cuts for Closed sec 2:
      (TMath::Abs(rec_ph[i]-(180/7.*1-2*open_cntr*1))<open_cntr),
      (TMath::Abs(rec_ph[i]-(180/7.*3-2*open_cntr*1))<open_cntr),
      (TMath::Abs(rec_ph[i]-(180/7.*5-2*open_cntr*1))<open_cntr),
      (TMath::Abs(rec_ph[i]-(180-2*open_cntr)*1)<open_cntr),
      (TMath::Abs(rec_ph[i]+(180/7.*1+2*open_cntr*1))<open_cntr),
      (TMath::Abs(rec_ph[i]+(180/7.*3+2*open_cntr*1))<open_cntr),
      (TMath::Abs(rec_ph[i]+(180/7.*5+2*open_cntr*1))<open_cntr)};


    G4double eqn[] = {
    //Eqs for Open sec (dr>=0.0395 && r<0.765):
      (1*0.398820+1*-1.099818*rec_r[i]+1*0.769735*rec_r[i]*rec_r[i])+(1*0.000019+1*0.000014*(rec_ph[i]-(180/7.*1-2*open_cntr*0))),
      (1*0.398820+1*-1.099818*rec_r[i]+1*0.769735*rec_r[i]*rec_r[i])+(1*0.000034+1*0.000045*(rec_ph[i]-(180/7.*3-2*open_cntr*0))),
      (1*0.398820+1*-1.099818*rec_r[i]+1*0.769735*rec_r[i]*rec_r[i])+(1*-0.000051+1*-0.000060*(rec_ph[i]-(180/7.*5-2*open_cntr*0))),
      (1*0.398820+1*-1.099818*rec_r[i]+1*0.769735*rec_r[i]*rec_r[i])+(1*0.000018+1*-0.000030*(rec_ph[i]>1?(rec_ph[i]-180):(rec_ph[i]+180))),
      (1*0.398820+1*-1.099818*rec_r[i]+1*0.769735*rec_r[i]*rec_r[i])+(1*-0.000004+1*0.000005*(rec_ph[i]+(180/7.*1+2*open_cntr*0))),
      (1*0.398820+1*-1.099818*rec_r[i]+1*0.769735*rec_r[i]*rec_r[i])+(1*-0.000049+1*-0.000018*(rec_ph[i]+(180/7.*3+2*open_cntr*0))),
      (1*0.398820+1*-1.099818*rec_r[i]+1*0.769735*rec_r[i]*rec_r[i])+(1*0.000046+1*0.000026*(rec_ph[i]+(180/7.*5+2*open_cntr*0))),

      //Eqs for Open sec (dr>=0.0395 && r>=0.765):
      (1*0.127163+1*-4.989350*rec_dr[i]+1*51.500748*rec_dr[i]*rec_dr[i])+(1*-0.000218+1*0.000016*(rec_ph[i]-(180/7.*1-2*open_cntr*0))),
      (1*0.127163+1*-4.989350*rec_dr[i]+1*51.500748*rec_dr[i]*rec_dr[i])+(1*0.000057+1*-0.000018*(rec_ph[i]-(180/7.*3-2*open_cntr*0))),
      (1*0.127163+1*-4.989350*rec_dr[i]+1*51.500748*rec_dr[i]*rec_dr[i])+(1*0.000230+1*0.000038*(rec_ph[i]-(180/7.*5-2*open_cntr*0))),
      (1*0.127163+1*-4.989350*rec_dr[i]+1*51.500748*rec_dr[i]*rec_dr[i])+(1*-0.000121+1*-0.000047*(rec_ph[i]>1?(rec_ph[i]-180):(rec_ph[i]+180))),
      (1*0.127163+1*-4.989350*rec_dr[i]+1*51.500748*rec_dr[i]*rec_dr[i])+(1*-0.000011+1*-0.000006*(rec_ph[i]+(180/7.*1+2*open_cntr*0))),
      (1*0.127163+1*-4.989350*rec_dr[i]+1*51.500748*rec_dr[i]*rec_dr[i])+(1*0.000078+1*-0.000020*(rec_ph[i]+(180/7.*3+2*open_cntr*0))),
      (1*0.127163+1*-4.989350*rec_dr[i]+1*51.500748*rec_dr[i]*rec_dr[i])+(1*-0.000070+1*-0.000060*(rec_ph[i]+(180/7.*5+2*open_cntr*0))),

      //Eqs for Open sec (dr<0.0395):
      (1*0.042981+1*-0.832476*rec_dr[i]+1*0.000000*rec_dr[i]*rec_dr[i])+(1*-0.000041+1*0.000069*(rec_ph[i]-(180/7.*1-2*open_cntr*0))),
      (1*0.042981+1*-0.832476*rec_dr[i]+1*0.000000*rec_dr[i]*rec_dr[i])+(1*0.000089+1*0.000119*(rec_ph[i]-(180/7.*3-2*open_cntr*0))),
      (1*0.042981+1*-0.832476*rec_dr[i]+1*0.000000*rec_dr[i]*rec_dr[i])+(1*0.000467+1*0.000010*(rec_ph[i]-(180/7.*5-2*open_cntr*0))),
      (1*0.042981+1*-0.832476*rec_dr[i]+1*0.000000*rec_dr[i]*rec_dr[i])+(1*-0.000039+1*0.000144*(rec_ph[i]>1?(rec_ph[i]-180):(rec_ph[i]+180))),
      (1*0.042981+1*-0.832476*rec_dr[i]+1*0.000000*rec_dr[i]*rec_dr[i])+(1*-0.000348+1*0.000059*(rec_ph[i]+(180/7.*1+2*open_cntr*0))),
      (1*0.042981+1*-0.832476*rec_dr[i]+1*0.000000*rec_dr[i]*rec_dr[i])+(1*-0.000065+1*-0.000174*(rec_ph[i]+(180/7.*3+2*open_cntr*0))),
      (1*0.042981+1*-0.832476*rec_dr[i]+1*0.000000*rec_dr[i]*rec_dr[i])+(1*-0.000195+1*0.000017*(rec_ph[i]+(180/7.*5+2*open_cntr*0))),

      //Eqs for Closed sec 1:
      (1*0.094772+1*-3.658849*rec_dr[i]+1*38.356179*rec_dr[i]*rec_dr[i])+(1*0.000185+1*0.000185*(rec_ph[i]-(180/7.*1-2*open_cntr*-1))),
      (1*0.094772+1*-3.658849*rec_dr[i]+1*38.356179*rec_dr[i]*rec_dr[i])+(1*-0.000010+1*0.000115*(rec_ph[i]-(180/7.*3-2*open_cntr*-1))),
      (1*0.094772+1*-3.658849*rec_dr[i]+1*38.356179*rec_dr[i]*rec_dr[i])+(1*0.000025+1*0.000195*(rec_ph[i]-(180/7.*5-2*open_cntr*-1))),
      (1*0.094772+1*-3.658849*rec_dr[i]+1*38.356179*rec_dr[i]*rec_dr[i])+(1*0.000119+1*0.000231*(rec_ph[i]-(180-2*open_cntr)*-1)),
      (1*0.094772+1*-3.658849*rec_dr[i]+1*38.356179*rec_dr[i]*rec_dr[i])+(1*0.000134+1*0.000276*(rec_ph[i]+(180/7.*1+2*open_cntr*-1))),
      (1*0.094772+1*-3.658849*rec_dr[i]+1*38.356179*rec_dr[i]*rec_dr[i])+(1*0.000195+1*0.000285*(rec_ph[i]+(180/7.*3+2*open_cntr*-1))),
      (1*0.094772+1*-3.658849*rec_dr[i]+1*38.356179*rec_dr[i]*rec_dr[i])+(1*0.000178+1*0.000144*(rec_ph[i]+(180/7.*5+2*open_cntr*-1))),

      //Eqs for Closed sec 2:
      (1*0.090725+1*-3.435000*rec_dr[i]+1*35.353838*rec_dr[i]*rec_dr[i])+(1*0.000081+1*-0.000045*(rec_ph[i]-(180/7.*1-2*open_cntr*1))),
      (1*0.090725+1*-3.435000*rec_dr[i]+1*35.353838*rec_dr[i]*rec_dr[i])+(1*0.000207+1*-0.000315*(rec_ph[i]-(180/7.*3-2*open_cntr*1))),
      (1*0.090725+1*-3.435000*rec_dr[i]+1*35.353838*rec_dr[i]*rec_dr[i])+(1*0.000036+1*-0.000185*(rec_ph[i]-(180/7.*5-2*open_cntr*1))),
      (1*0.090725+1*-3.435000*rec_dr[i]+1*35.353838*rec_dr[i]*rec_dr[i])+(1*0.000181+1*-0.000207*(rec_ph[i]-(180-2*open_cntr)*1)),
      (1*0.090725+1*-3.435000*rec_dr[i]+1*35.353838*rec_dr[i]*rec_dr[i])+(1*0.000088+1*-0.000123*(rec_ph[i]+(180/7.*1+2*open_cntr*1))),
      (1*0.090725+1*-3.435000*rec_dr[i]+1*35.353838*rec_dr[i]*rec_dr[i])+(1*0.000260+1*-0.000329*(rec_ph[i]+(180/7.*3+2*open_cntr*1))),
      (1*0.090725+1*-3.435000*rec_dr[i]+1*35.353838*rec_dr[i]*rec_dr[i])+(1*-0.000087+1*-0.000190*(rec_ph[i]+(180/7.*5+2*open_cntr*1)))};
    
    for(int ii=0;ii<35;ii++){
      if(cut[ii]){
	theta[i] = eqn[ii];

	// G4cout <<"cut[ii]:: " << cut[ii] << G4endl;
	// G4cout <<"eqn[ii]:: " << eqn[ii] << G4endl;
	// G4cout <<"rec_ph[i]:: " << rec_ph[i] << G4endl;
	break;
      }
      else 
	theta[i] = -1.0;
    }

    if(EvalTrackVerbose){
      G4cout << "Org:: " << aTrackHit[i]->f3X.perp()/m << "\t" << aTrackHit[i]->f3X.phi()/deg << "\t" << aTrackHit[i]->f3dP.perp() << "\t" << aTrackHit[i]->fTh << G4endl;
      G4cout << "Rec:: " << rec_r[i] << "\t" << rec_ph[i] << "\t" << rec_dr[i] << "\t" << theta[i] << G4endl;
    }    
  }
  for(G4int i=0;i<rTrackHitSize;i++)
    aTrackHit[i]->fThRec = theta[i];
  
  if(EvalTrackVerbose)
    G4cout << "Leaving remollTrackReconstruct::EvalTheta() ..." << G4endl;
}

// return y = a + bx
G4double remollTrackReconstruct::EvalTrackPos(G4double z,G4ThreeVector &ab){

  G4double aVal = ab.x()/mm; 
  G4double bVal = ab.y()/rad;

  G4double yVal = aVal + bVal*z;   // z is in mm

  return yVal/mm;
}

// return the direction angle, which is just the slope
G4double remollTrackReconstruct::EvalTrackAng(G4double z,G4ThreeVector &ab){

  G4double bVal = ab.y()/rad; 

  return bVal/rad;
}
