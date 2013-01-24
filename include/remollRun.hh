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

	int GetNthrown(){ return fNthrown; }

    private:
	int    fNthrown;
	double fBeamE;
	char fGenName[255];
};

#endif//__REMOLLRUN_HH
