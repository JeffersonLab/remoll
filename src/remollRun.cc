#include "remollRun.hh"
#include "remollRunData.hh"

remollRun *remollRun::gSingleton = NULL;

remollRun::remollRun() {
    gSingleton = this;
    fRunData = new remollRunData();
    fRunData->Init();
}

remollRun::~remollRun() {
}

remollRun *remollRun::GetInstance() {
    if( gSingleton == NULL ) {
        gSingleton = new remollRun();
    }
    return gSingleton;
}
