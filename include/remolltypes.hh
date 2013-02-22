#ifndef __REMOLL_TYPES_HH
#define __REMOLL_TYPES_HH

/*
   remolltypes.hh

   Generic data types which are classes.  Used for passing IO
   data around and useful enumerations
*/

#define __RUNSTR_LEN 255
#define __MAXFILE_LEN 1048576 // MB file size limit

#include "TTimeStamp.h"

enum SampType_t { kCryogen, kWalls, kFullTarget };

struct filedata_t {
    char filename[__RUNSTR_LEN];
    char hashsum[__RUNSTR_LEN];
    TTimeStamp timestamp;
};

#endif//__REMOLL_TYPES_HH
