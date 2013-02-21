#ifndef __REMOLLRUNDATA_HH
#define __REMOLLRUNDATA_HH

#include "TObject.h"

/*!
 * All the information on the run
 * This will get put into the output
 * stream
*/

class remollRunData : public TObject {

    public:
	 remollRunData();
	~remollRunData();

	unsigned long long int GetNthrown(){ return fNthrown; }
	void SetNthrown(unsigned long long int n){ fNthrown = n; }

	void Print();

    private:
	long int  fNthrown;
	double fBeamE;
	char fGenName[255];

	ClassDef(remollRunData, 1);
};

#endif//__REMOLLRUNDATA_HH
