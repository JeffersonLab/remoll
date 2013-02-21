#ifndef __REMOLLGENPION_HH 
#define __REMOLLGENPION_HH 
/*!
 * Pion event generator
 * from Wiser parameterization
 *
 * Seamus Riordan
 * February 4, 2013
 *
 * Based heavily on previous work from mollersim
*/

#include "remollVEventGen.hh"

class remollGenPion : public remollVEventGen {
    public:
	 remollGenPion();
	~remollGenPion();

	enum Pion_t {kPiPlus, kPiMinus};

    private:
	void SamplePhysics(remollVertex *, remollEvent *);

	double fThMin, fThMax;
	double fEmin;

	Pion_t fPionType;
};

#endif//__REMOLLGENPION_HH 
