#ifndef __REMOLL_TYPES_HH
#define __REMOLL_TYPES_HH

/*
   remolltypes.hh

   Generic data types which are classes.  Used for passing IO
   data around and useful enumerations
*/

#if !defined(__CINT__) && !defined(__CLING__)
#include <G4Types.hh>
#endif

#define __RUNSTR_LEN 255
#define __MAXFILE_LEN 1048576 // MB file size limit

#include "TTimeStamp.h"

enum SampType_t { kCryogen, kWalls, kFullTarget };

struct filedata_t {
    char filename[__RUNSTR_LEN];
    char hashsum[__RUNSTR_LEN];
    TTimeStamp timestamp;
};

struct remollBeamTarget_t {
  double x, y, z;
  double dx, dy, dz;
  double th, ph;
};

struct remollEvent_t {
  double A, Am;
  double xs;
  double Q2, W2;
  double thcom;
  double beamp;
};

struct remollEventParticle_t {
  int pid;
  double x, y, z;
  double px, py, pz;
  double th, ph, p;
  double tpx, tpy, tpz;
};

struct remollGenericDetectorSum_t {
  int det;
  int vid;
  double edep;
};

struct remollGenericDetectorHit_t {
  int det;
  int id;
  int trid;
  int pid;
  int gen;
  int mtrid;
  double x, y, z;
  double r, ph;
  double px, py, pz;
  double p, e, m;
  double vx, vy, vz;
};

#endif//__REMOLL_TYPES_HH
