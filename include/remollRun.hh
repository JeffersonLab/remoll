#ifndef __REMOLLRUN_HH
#define __REMOLLRUN_HH

/*!
 * All the information on the run
 * The data object will get put into the output
 * stream
  
   This is implemented in the soliton model
 */

#include "remollRunData.hh"

class remollRun {

    private:
	static remollRun *gSingleton;
	 remollRun();

	remollRunData *fRunData;

    public:
	 static remollRun *GetRun();
	~remollRun();

	remollRunData *GetData(){return fRunData;}
};

#endif//__REMOLLRUN_HH
