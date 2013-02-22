#include "remollRunData.hh"

#include <string.h>

remollRunData::remollRunData(){
    fNthrown = -1;
    fBeamE   = -1e9;
    fGenName[0]  = NULL;
    fGitInfo[0]  = NULL;
    fHostName[0] = NULL;
}

remollRunData::~remollRunData(){
}

void remollRunData::Init(){
    fNthrown = 0;
    fBeamE   = 0;
    strcpy(fGenName, "default");
    strcpy(fGitInfo, gGitInfoStr);
    if(gethostname(fHostName,__RUNSTR_LEN) == -1){
	fprintf(stderr, "%s line %d: ERROR could not get hostname\n", __PRETTY_FUNCTION__ ,  __LINE__ );
    }
}

void remollRunData::Print(){
    printf("git repository info\n-----------------------------------------------\n%s-----------------------------------------------\n\n", fGitInfo);
    printf("Run at %s on %s\n", fRunTime.AsString("ls"), fHostName);
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

    printf("Macro run:\n-----------------------------------------------\n%s-----------------------------------------------\n\n", GetMacroBuffer());

}
