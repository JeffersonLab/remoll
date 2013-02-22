#ifndef __REMOLLRUNDATA_HH
#define __REMOLLRUNDATA_HH

#include "TObject.h"

#include <vector>
#include <string>
#include <remolltypes.hh>

#include "gitinfo.hh"

/*!
 * All the information on the run
 * This will get put into the output
 * stream
*/

class TGeoManager;

class remollRunData : public TObject {

    public:
	 remollRunData();
	~remollRunData();

	unsigned long long int GetNthrown(){ return fNthrown; }
	void SetNthrown(unsigned long long int n){ fNthrown = n; }

	void Init();

	void SetGenName(const char *n){ strcpy(fGenName, n); }

	void SetBeamE(double E){ fBeamE = E; }

	void AddMagData(filedata_t d){fMagData.push_back(d);}
	char *GetMacroBuffer(){ return fMacro; }
	long int GetMacroBufferSize(){ return __MAXFILE_LEN; }

	void Print();

	TTimeStamp fRunTime;

	long int  fNthrown;
	double fBeamE;
	char fGenName[__RUNSTR_LEN];
	char fGitInfo[__GITMAXINFO_SIZE];

	char fHostName[__RUNSTR_LEN];

	char fMacro[__MAXFILE_LEN];

	std::vector<filedata_t> fMagData;

	ClassDef(remollRunData, 1);
};

#endif//__REMOLLRUNDATA_HH
