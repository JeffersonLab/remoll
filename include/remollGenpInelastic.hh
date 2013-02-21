#ifndef __REMOLLGENPINELASTIC_HH 
#define __REMOLLGENPINELASTIC_HH 
/*!
 * Inelastic ep event generator
 *
 * Seamus Riordan
 * February 14, 2013
 *
 * Uses Christy/Bosted parameterization
*/

#include "remollVEventGen.hh"

class remollGenpInelastic : public remollVEventGen {
    public:
	 remollGenpInelastic();
	~remollGenpInelastic();

    private:
	void SamplePhysics(remollVertex *, remollEvent *);

};

#endif//__REMOLLGENMOLLER_HH 
