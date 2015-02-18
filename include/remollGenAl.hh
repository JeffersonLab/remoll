#ifndef __REMOLLGENAL_HH
#define __REMOLLGENAL_HH
/*!
 * Aluminium event generator
 *
 * Ciprian Gal
 * November 29, 2014
 *
 * Uses Christy/Bosted parameterization
*/

#include "remollVEventGen.hh"

class remollGenAl : public remollVEventGen {
public:
    remollGenAl(int physicsType);
    virtual ~remollGenAl();

private:
    int type;
    void SamplePhysics(remollVertex *, remollEvent *);
    void GenInelastic(double beamE,double th,
		      double &Q2,double &W2,double &effectiveXsection,
		      double &fWeight,double &eOut,double &asym);

};

#endif//__REMOLLGENAL_HH 
