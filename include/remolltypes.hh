#ifndef __REMOLL_TYPES_HH
#define __REMOLL_TYPES_HH

/*
   remolltypes.hh

   Generic data types which are classes.  Used for passing IO
   data around and useful enumerations
*/

enum SampType_t { kCryogen, kWalls, kFullTarget };

const double GF = 1.16637e-5; 
const double alpha = 0.007299; 
const double sin2thW = 0.2312;
const double QeW = 1.0 - 4.0*sin2thW; 


#endif//__REMOLL_TYPES_HH
