#ifndef __REMOLLGENMOLLER_HH 
#define __REMOLLGENMOLLER_HH 
/*!
 * Moller (ee) event generator
 *
 * Seamus Riordan
 * February 4, 2013
 *
 * Based heavily on previous work from mollersim
*/

#include "remollVEventGen.hh"

class remollGenMoller : public remollVEventGen {
    public:
	 remollGenMoller();
	~remollGenMoller();

    private:
	void SamplePhysics(remollVertex *, remollEvent *);

};

#endif//__REMOLLGENMOLLER_HH 
