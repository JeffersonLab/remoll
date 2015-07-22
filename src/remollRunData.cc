#include "remollRunData.hh"

#include <string.h>
#include <errno.h>

remollRunData::remollRunData(){
    fNthrown = -1;
    fBeamE   = -1e9;
    fGenName[0]  = '\0';
    fGitInfo[0]  = '\0';
    fHostName[0] = '\0';
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
	fprintf(stderr, "%s\n",strerror(errno));
    }
    if(getcwd(fRunPath,__RUNSTR_LEN) == NULL){
	fprintf(stderr, "%s line %d: ERROR could not get current working directory\n", __PRETTY_FUNCTION__ ,  __LINE__ );
	fprintf(stderr, "%s\n",strerror(errno));
    }
}

void remollRunData::Print(){
    printf("git repository info\n-------------------------------------------------\n%s-------------------------------------------------\n\n", fGitInfo);
    printf("Run at %s on %s\n", fRunTime.AsString("ls"), fHostName);
    printf("Run Path %s\n", fRunPath);
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

    printf("Macro run:\n-------------------------------------------------\n");

    fMacro.Print();
    
    printf("-------------------------------------------------\n\n");
    printf("Stored GDML Files:\n");
    for( i = 0; i < fGDMLFiles.size(); i++ ){
	if( fGDMLFiles[i].GetBufferSize() >= 1024 ){
	    printf("\t%32s %4lld kB\n", fGDMLFiles[i].GetFilename(), fGDMLFiles[i].GetBufferSize()/1024 );
	} else {
	    printf("\t%32s   <1 kB\n", fGDMLFiles[i].GetFilename());
	}
    }
    printf("-------------------------------------------------\n\n");

}

void remollRunData::AddGDMLFile( const char *fn ){
    // Check for duplicates I guess

    unsigned int i;

    for( i = 0; i < fGDMLFiles.size(); i++ ){
	if( strcmp(fn, fGDMLFiles[i].GetFilename()) == 0 ){
	    // Already added
	    return;
	}
    }

    fGDMLFiles.push_back(remollTextFile(fn)); 
}

void remollRunData::RecreateGDML( const char *adir, bool clobber ){
    unsigned int idx;

    for( idx = 0; idx < fGDMLFiles.size(); idx++ ){
	fGDMLFiles[idx].RecreateInDir(adir, clobber);
    }
    return;
}

ClassImp(remollRunData);











