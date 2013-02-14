#ifndef __REMOLLRUN_HH
#define __REMOLLRUN_HH


/*!
 * All the information on the run
 * This will get put into the output
 * stream
  
   This is implemented in the soliton model
 */

class remollRun {

    private:
	static remollRun *gSingleton;
	 remollRun();

    public:
	 static remollRun *GetRun();
	~remollRun();

	unsigned long long int GetNthrown(){ return fNthrown; }
	void SetNthrown(unsigned long long int n){ fNthrown = n; }

    private:
	long int    fNthrown;
	double fBeamE;
	char fGenName[255];
};

#endif//__REMOLLRUN_HH
