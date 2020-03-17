#ifndef __MAINEDETUTIL_H
#define __MAINEDETUTIL_H

#include "anaConst.h"

std::vector<std::vector<double>> rMin, rMax;
void initRadialConfiguration(int k);

int findDetector(int &sector, double phi, double r, int rConfig){
  initRadialConfiguration(rConfig);
  const double secPhi = fmod(phi, 2*pi/7);
  
  //0,1,2 == closed, transition, open
  if( secPhi < pi/28 )
    sector = 0;
  else if( secPhi < 3*pi/28 )
    sector = 1;
  else if( secPhi < 5*pi/28 )
    sector = 2;
  else if( secPhi < 7*pi/28 )
    sector = 1;
  else if( secPhi < 8*pi/28 )
    sector = 0;

  // this will jsut pick up the first hit and ignore the rest

  const int nRings = 7;
  for(int i=0;i<nRings;i++)
    if(r >= rMin[i][sector] && r <= rMax[i][sector])
      return i;

  return -1;
}

void initRadialConfiguration(int k=0){

  if(k==1){
    // segmented hybrid configuration from Sakib 200131
    rMin={
      { 640.0,  640.0,  640.0},
      { 680.0,  680.0,  680.0},
      { 750.0,  750.0,  750.0},
      { 855.0,  847.5,  825.0},
      { 935.0,  920.0,  875.0},
      {1075.0, 1080.0, 1090.0},
      {1200.0, 1200.0, 1200.0}
    };
    rMax={
      { 680.0,  680.0,  680.0},
      { 750.0,  750.0,  750.0},
      { 855.0,  847.5,  825.0},
      { 935.0,  920.0,  875.0},
      {1075.0, 1060.0, 1055.0},
      {1190.0, 1190.0, 1190.0},
      {1500.0, 1500.0, 1500.0}
    };
  }else if(k==0){
    // hybrid magnet configuration as defined by Sakib on 200202
    rMin={
      { 640.0,  640.0,  640.0},
      { 680.0,  680.0,  680.0},
      { 730.0,  730.0,  730.0},
      { 805.0,  827.5,  835.0},
      { 855.0,  900.0,  915.0},
      {1070.0, 1060.0, 1055.0},
      {1200.0, 1200.0, 1200.0}
    };
    rMax={
      { 680.0,  680.0,  680.0},
      { 730.0,  730.0,  730.0},
      { 805.0,  827.5,  835.0},
      { 855.0,  900.0,  915.0},
      {1070.0, 1060.0, 1055.0},
      {1170.0, 1170.0, 1170.0},
      {1500.0, 1500.0, 1500.0}
    };
  }
}
#endif // __MAINDETUTIL_H
