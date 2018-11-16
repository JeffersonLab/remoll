#ifndef __PE_TYPES_HH
#define __PE_TYPES_HH

/*
   petypes.hh

   Generic data types which are classes.  Used for passing IO
   data around and useful enumerations in PE calculations
*/

//#if !defined(__CINT__) && !defined(__CLING__)
//#include <G4Types.hh>
//#endif

#define __RUNSTR_LEN 255
#define __MAXFILE_LEN 1048576 // MB file size limit

#include "TTimeStamp.h"

// Event structure

struct catPEs_t { // The full spectrum regardless of source
  int nPEs; // PE counts per event
  std::vector<int> detIDs; // store the det IDs too
  std::vector<double> catHitx, catHity, catHitz; //Cathode PE hit information
};

struct Q_t { // Quartz Hits
  int nPEs;
  int det;
  double r;
  double x;
  double y;
  double z;
  double px, py, pz;
  double p, e, m;
  double vx, vy, vz;
  std::vector<double> catHitx, catHity, catHitz; //Cathode PE hit information
};

struct Ref_t { // Reflector Primary Hits
  int nPEs;
  int det;
  double r;
  double x;
  double y;
  double z;
  double px, py, pz;
  double p, e, m;
  double vx, vy, vz;
  std::vector<double> catHitx, catHity, catHitz; //Cathode PE hit information
};

struct RefX_t { // Reflector Accidental Side Hits
  int nPEs;
  int det;
  double r;
  double x;
  double y;
  double z;
  double px, py, pz;
  double p, e, m;
  double vx, vy, vz;
  std::vector<double> catHitx, catHity, catHitz; //Cathode PE hit information
};

struct Refair_t { // Reflector Air Volume Hits
  int nPEs;
  int det;
  double r;
  double x;
  double y;
  double z;
  double px, py, pz;
  double p, e, m;
  double vx, vy, vz;
  std::vector<double> catHitx, catHity, catHitz; //Cathode PE hit information
};

struct LG_t { // Light Guide Hits
  int nPEs;
  int det;
  double r;
  double x;
  double y;
  double z;
  double px, py, pz;
  double p, e, m;
  double vx, vy, vz;
  std::vector<double> catHitx, catHity, catHitz; //Cathode PE hit information
};

struct LGair_t { // Light Guide Air Volume Hits
  int nPEs;
  int det;
  double r;
  double x;
  double y;
  double z;
  double px, py, pz;
  double p, e, m;
  double vx, vy, vz;
  std::vector<double> catHitx, catHity, catHitz; //Cathode PE hit information
};

struct PMTcat_t { // PMT Cathode Hits
  int nPEs;
  int det;
  double r;
  double x;
  double y;
  double z;
  double px, py, pz;
  double p, e, m;
  double vx, vy, vz;
  std::vector<double> catHitx, catHity, catHitz; //Cathode PE hit information
};

struct PMTbulk_t { // PMT Bulk Volume Hits
  int nPEs;
  int det;
  double r;
  double x;
  double y;
  double z;
  double px, py, pz;
  double p, e, m;
  double vx, vy, vz;
  std::vector<double> catHitx, catHity, catHitz; //Cathode PE hit information
};

struct elseX_t { // PMT Bulk Volume Hits
  int nPEs;
  std::vector<double> catHitx, catHity, catHitz; //Cathode PE hit information
};


#endif // __PE_TYPES_HH
