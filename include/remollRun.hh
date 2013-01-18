#ifndef __REMOLLRUN_HH
#define __REMOLLRUN_HH

#include "G4UImanager.hh"

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

	G4int GetNthrown(){ return fNthrown; }

    private:
	G4int    fNthrown;
	G4double fBeamE;
	G4String fGenName;
};

#endif//__REMOLLRUN_HH
