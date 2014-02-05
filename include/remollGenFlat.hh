#ifndef __REMOLLGENFLAT_HH 
#define __REMOLLGENFLAT_HH 
/*!
 * Flat event generator
 *
 * Seamus Riordan
 * February 5, 2014
 *
*/

#include "remollVEventGen.hh"

class remollGenFlat : public remollVEventGen {
    public:
	 remollGenFlat();
	~remollGenFlat();

	double fE_max;

    private:
	void SamplePhysics(remollVertex *, remollEvent *);


};

#endif//__REMOLLGENMOLLER_HH 
