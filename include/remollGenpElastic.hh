#ifndef __REMOLLGENPELASTIC_HH 
#define __REMOLLGENPELASTIC_HH 
/*!
 * ep elastic event generator
 *
 * Seamus Riordan
 * February 12, 2013
 *
 * Based heavily on previous work from mollersim
*/

#include "remollVEventGen.hh"

class remollBeamTarget;

class remollGenpElastic : public remollVEventGen {
    public:
	remollGenpElastic();
	virtual ~remollGenpElastic();

    private:
	void SamplePhysics(remollVertex *, remollEvent *);

	G4double RadProfile(G4double,G4double);
	G4double EnergNumInt(G4double,G4double,G4double);
};

#endif//__REMOLLGENPELASTIC_HH 
