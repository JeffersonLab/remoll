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

	void SetPionType(Pion_t t) { fPionType = t; }

    private:
	void SamplePhysics(remollVertex *, remollEvent *);


	Pion_t fPionType;
};

#endif//__REMOLLGENPION_HH 
