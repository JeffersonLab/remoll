#include "remollRunData.hh"

#include "G4ios.hh"

#include <string.h>
#include <errno.h>

#ifdef __APPLE__
#include <unistd.h>
#endif

// External objects
extern const char* const gGitInfo;

remollRunData::remollRunData()
{
    fSeed = 0;
    fNthrown = -1;
    fBeamE   = -1e9;
    fGenName[0]  = '\0';
    fHostName[0] = '\0';
}

remollRunData::~remollRunData(){
}

void remollRunData::Init()
{
    fNthrown = 0;
    fBeamE   = 0;
    strcpy(fGenName, "default");
    if(gethostname(fHostName,__RUNSTR_LEN) == -1){
	fprintf(stderr, "%s line %d: ERROR could not get hostname\n", __PRETTY_FUNCTION__ ,  __LINE__ );
	fprintf(stderr, "%s\n",strerror(errno));
    }
    if(getcwd(fRunPath,__RUNSTR_LEN) == NULL){
	fprintf(stderr, "%s line %d: ERROR could not get current working directory\n", __PRETTY_FUNCTION__ ,  __LINE__ );
	fprintf(stderr, "%s\n",strerror(errno));
    }
}

void remollRunData::Print()
{
    G4cout << "git repository info" << G4endl;
    G4cout << "-------------------------------------------------" << G4endl;
    G4cout << gGitInfo << G4endl;
    G4cout << "-------------------------------------------------" << G4endl;
    G4cout << "Run at " << fRunTime.AsString("ls") << " on " << fHostName << G4endl;
    G4cout << "Run Path " << fRunPath << G4endl;;
    G4cout << "N generated = " << fNthrown << G4endl;
    G4cout << "Beam Energy = " << fBeamE << "GeV" << G4endl;
    G4cout << "Generator   = " << fGenName << G4endl;

    G4cout << "Field maps:" << G4endl;
    for (unsigned int i = 0; i < fMagData.size(); i++ ){
	G4cout << "\t" << fMagData[i].filename << G4endl;
	G4cout << "\t" << fMagData[i].hashsum << G4endl;
	G4cout << "\t" << fMagData[i].timestamp.AsString("ls") << G4endl;
    }

    G4cout << "Macro run:" << G4endl;
    G4cout << "-------------------------------------------------" << G4endl;
    fMacro.Print();
    G4cout << "-------------------------------------------------" << G4endl;
    G4cout << "Stored GDML Files:" << G4endl;
    for (unsigned int i = 0; i < fGDMLFiles.size(); i++ ) {
	if( fGDMLFiles[i].GetBufferSize() >= 1024 ){
	    G4cout << "\t" << fGDMLFiles[i].GetFilename() << " " << fGDMLFiles[i].GetBufferSize()/1024 << "kB" << G4endl;
	} else {
	    G4cout << "\t" << fGDMLFiles[i].GetFilename() << " < 1kB" << G4endl;
	}
    }
    G4cout << "-------------------------------------------------" << G4endl;
}

void remollRunData::AddGDMLFile( const char *fn )
{
    // Check for duplicates I guess
    for(unsigned int i = 0; i < fGDMLFiles.size(); i++ ){
	if( strcmp(fn, fGDMLFiles[i].GetFilename()) == 0 ){
	    // Already added
	    return;
	}
    }

    fGDMLFiles.push_back(remollTextFile(fn)); 
}

void remollRunData::RecreateGDML( const char *adir, bool clobber )
{
    for(unsigned int idx = 0; idx < fGDMLFiles.size(); idx++ ){
	fGDMLFiles[idx].RecreateInDir(adir, clobber);
    }
}

ClassImp(remollRunData)
