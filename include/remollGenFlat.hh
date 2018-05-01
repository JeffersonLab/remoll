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
	virtual ~remollGenFlat();

	void SetParticleName(const G4String& name) {
	  fParticleName = name;
	}

    private:
	void SamplePhysics(remollVertex *, remollEvent *);

        G4String fParticleName;
};

#endif//__REMOLLGENMOLLER_HH 
