#include "remollRun.hh"

#include <string.h>

remollRun *remollRun::gSingleton = NULL;

remollRun::remollRun(){
    gSingleton = this;

    fNthrown = -1;
    fBeamE   = -1e9;
    strcpy(fGenName, "<none>");
}

remollRun::~remollRun(){
}

remollRun *remollRun::GetRun(){
    if( gSingleton == NULL ){
	gSingleton = new remollRun();
    }
    return gSingleton;
}
