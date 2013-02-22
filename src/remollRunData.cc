#include "remollRunData.hh"

#include <string.h>

remollRunData::remollRunData(){
    fNthrown = -1;
    fBeamE   = -1e9;
    strcpy(fGenName, "default");
    strcpy(fGitInfo, gGitInfoStr);
}

remollRunData::~remollRunData(){
}

void remollRunData::Print(){
    printf("git repository info\n-----------------------------------------------\n%s-----------------------------------------------\n\n", fGitInfo);
    printf("N generated = %ld\n", fNthrown);
    printf("Beam Energy = %f GeV\n", fBeamE);
    printf("Generator   = %s\n", fGenName);

    printf("Field maps:\n");
    unsigned int i;
    for( i = 0; i < fMagData.size(); i++ ){
	printf("\t%s\n", fMagData[i].filename);
	printf("\t%s\n", fMagData[i].hashsum);
	printf("\t%s\n\n", fMagData[i].timestamp.AsString("ls"));
    }
}
