#ifndef __REMOLLGENBEAM_HH 
#define __REMOLLGENBEAM_HH 
/*!
 * Boring beam event generator
 *
 * Seamus Riordan
 * July 9, 2013
 *
*/

#include "remollVEventGen.hh"

class remollBeamTarget;

class remollGenBeam : public remollVEventGen {
    public:
	remollGenBeam();
	~remollGenBeam();

    private:
	void SamplePhysics(remollVertex *, remollEvent *);

	remollBeamTarget *fBeamTarg;

	double fZpos;
};

#endif//__REMOLLGENBEAM_HH 
