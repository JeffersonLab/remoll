#ifndef __REMOLL_TYPES_HH
#define __REMOLL_TYPES_HH

/*
   remolltypes.hh

   Generic data types which are classes.  Used for passing IO
   data around and useful enumerations
*/

//#if !defined(__CINT__) && !defined(__CLING__)
//#include <G4Types.hh>
//#endif

#define __RUNSTR_LEN 255
#define __MAXFILE_LEN 1048576 // MB file size limit

#include "TTimeStamp.h"

enum SampType_t { kNoTargetVolume, kActiveTargetVolume, kAllTargetVolumes };

struct filedata_t {
    char filename[__RUNSTR_LEN];
    char hashsum[__RUNSTR_LEN];
    TTimeStamp timestamp;
};

// Event structure
struct remollEvent_t {
  double A, Am;
  double xs;
  double Q2, W2;
  double thcom;
  double beamp;
};

// Beam and target structure
struct remollBeamTarget_t {
  double x, y, z;
  double dx, dy, dz;
  double th, ph;
};

// Particle trajectory structure
struct remollTrajectoryPoint_t {
  double x, y, z;
};

// Primary particle structure
struct remollEventParticle_t {
  int pid;
  double vx, vy, vz;
  double px, py, pz;
  double sx, sy, sz;
  double th, ph, p;
  double tpx, tpy, tpz;
  int trid;
  std::vector<double> tjx, tjy, tjz; //Trajectory information
};

// Generic detector hit and sum structure
struct remollGenericDetectorHit_t {
  int det;
  int id;
  int trid;
  int pid;
  int gen;
  int mtrid;
  double t;
  double x, y, z;
  double xl, yl, zl;
  double r, ph;
  double px, py, pz;
  double pxl, pyl, pzl;
  double sx, sy, sz;
  double p, e, m, k;
  double vx, vy, vz;
  double edep;
};
struct remollGenericDetectorSumByPID_t {
  double x,y,z;
  double edep;
  int pid;
  int n;
};
struct remollGenericDetectorSum_t {
  std::vector<remollGenericDetectorSumByPID_t> by_pid;
  double edep;
  int det;
  int vid;
  int n;
};

// System of units structure
struct remollUnits_t
{
  remollUnits_t();
  // constructor impl in remollSystemOfUnits.cc due to conflicts between
  // rootcint and required G4/CLHEP headers

  // Asymmetry
  const double ppm;
  const double ppb;
  // Distance
  const double nm;
  const double um;
  const double mm;
  const double cm;
  const double m;
  // Area
  const double mm2;
  const double cm2;
  const double m2;
  // Volume
  const double mm3;
  const double cm3;
  const double m3;
  // Energy
  const double eV;
  const double keV;
  const double MeV;
  const double GeV;
  // Angle
  const double rad;
  const double deg;
  const double sr;
  // Cross Section
  const double barn;
  const double mbarn;
  const double ubarn;
  // Time
  const double nsec;
  const double msec;
  const double sec;
  // Frequency
  const double Hz;
  const double kHz;
  const double MHz;
  const double GHz;
};

#endif // __REMOLL_TYPES_HH
