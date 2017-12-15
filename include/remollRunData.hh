#ifndef __REMOLLRUNDATA_HH
#define __REMOLLRUNDATA_HH

#include "TObject.h"

#include <vector>
#include <string>

#include "remolltypes.hh"
#include "remollTextFile.hh"

/*!
 * All the information on the run
 * This will get put into the output
 * stream
*/

class TGeoManager;

class remollRunData : public TObject {
  using TObject::Print;
    public:
	remollRunData();
	virtual ~remollRunData();

	unsigned long long int GetNthrown(){ return fNthrown; }
	void SetNthrown(unsigned long long int n){ fNthrown = n; }

	void Init();

	void SetGenName(const char *n){ strcpy(fGenName, n); }
	const char *GetGenName(){ return fGenName; }

	void SetBeamE(double E){ fBeamE = E; }
	void SetSeed(unsigned long int seed){ fSeed = seed; }

	void AddMagData(filedata_t d){fMagData.push_back(d);}
	void SetMacroFile(const char *fn){ fMacro = remollTextFile(fn); }
	void AddGDMLFile(const char *fn);
	void ClearGDMLFiles(){ fGDMLFiles.clear(); }

	void RecreateGDML(const char *adir = NULL, bool clobber = false);

	remollTextFile GetGDMLFile(int i){ return fGDMLFiles[i]; }

	void Print();

	TTimeStamp fRunTime;

	long int  fNthrown;
	long int  fSeed;
	double fBeamE;
	char fGenName[__RUNSTR_LEN];

	char fHostName[__RUNSTR_LEN];
	char fRunPath[__RUNSTR_LEN];

	remollTextFile              fMacro;
	std::vector<remollTextFile> fGDMLFiles;

	std::vector<filedata_t> fMagData;

	ClassDef(remollRunData, 1);
};

#endif//__REMOLLRUNDATA_HH
