#include "remollRunData.hh"

#include <string.h>

remollRunData::remollRunData(){
    fNthrown = -1;
    fBeamE   = -1e9;
    strcpy(fGenName, "oops");
}

remollRunData::~remollRunData(){
}

void remollRunData::Print(){
    printf("HELLO I AM remollRunData!!!!\n");
}
