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
  int npes; // PE counts per event
  double all_bounces, ref_bounces, lg_bounces;
  int det; // store the source det ID too
  std::vector<int> detids; // store the source det IDs too
  std::vector<int> mtrids; // store the source mtrids too
  std::vector<int> pids; // store the source pids too
  std::vector<double> cathitx, cathity, cathitz; //Cathode PE hit information
};

struct hitPEs_t { // Quartz Hits
  int npes;
  int det;
  double r;
  double x;
  double y;
  double z;
  double px, py, pz;
  double p, e, m;
  double vx, vy, vz;
  std::vector<double> cathitx, cathity, cathitz; //Cathode PE hit information
};



#endif // __PE_TYPES_HH
