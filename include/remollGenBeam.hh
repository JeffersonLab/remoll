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

class remollGenBeam : public remollVEventGen {
    public:
	remollGenBeam();
	virtual ~remollGenBeam();
    void SetOriginX(double x);
    void SetOriginY(double y);
    void SetOriginZ(double z);
    private:
	void SamplePhysics(remollVertex *, remollEvent *);

	double fXpos;
	double fYpos;
	double fZpos;
};

#endif//__REMOLLGENBEAM_HH 
