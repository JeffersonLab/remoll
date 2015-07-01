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
#include "Randomize.hh"

class remollGenAl : public remollVEventGen {
public:
  remollGenAl(G4int physicsType);
  virtual ~remollGenAl();
  
private:
  G4int type;
  void SamplePhysics(remollVertex *, remollEvent *);

  void GenInelastic(G4double beamE,G4double th,
		    G4double &Q2,G4double &W2,G4double &effectiveXsection,
		    G4double &fWeight,G4double &eOut,G4double &asym);

  void GenQuasiElastic(G4double beamE,G4double theta,
		       G4double &Q2,G4double &W2,G4double &effectiveXsection,
		       G4double &fWeight,G4double &eOut);


};

#endif//__REMOLLGENAL_HH 
