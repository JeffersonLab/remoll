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

    void SetMomentumX(double px);
    void SetMomentumY(double py);
    void SetMomentumZ(double pz);

    void SetPolarizationX(double px);
    void SetPolarizationY(double py);
    void SetPolarizationZ(double pz);

    void SetPartName(G4String& name);

    private:
	void SamplePhysics(remollVertex *, remollEvent *);

	double fXpos;
	double fYpos;
	double fZpos;

    double fXmomentum;
    double fYmomentum;
    double fZmomentum;

    double fXPolarization;
    double fYPolarization;
    double fZPolarization;

    G4String fParticleName;
};

#endif//__REMOLLGENBEAM_HH 
