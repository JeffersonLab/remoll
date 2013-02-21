#include "remollRunData.hh"

#include <string.h>

remollRunData::remollRunData(){
    fNthrown = -1;
    fBeamE   = -1e9;
    strcpy(fGenName, "default");
}

remollRunData::~remollRunData(){
}

void remollRunData::Print(){
    printf("N generated = %ld\n", fNthrown);
    printf("Beam Energy = %f GeV\n", fBeamE);
    printf("Generator = %s\n", fGenName);
}
