#ifndef __REMOLLEVENT_HH
#define __REMOLLEVENT_HH

/*!
   Event information class.  This needs to
   contain all the information we need to
   generate particles and provide sufficient
   output.
*/

#include <vector>
#include "G4ThreeVector.hh"

class G4ParticleDefinition;

class remollEvent {
    public:
	 remollEvent();
	~remollEvent();

	void ProduceNewParticle( G4ThreeVector, G4ThreeVector, G4String );
	void SetEffCrossSection( G4double xs ){ fEffXs = xs; }

	void Reset();
	void UndoLastParticle();

    private:

    public:
	// Interaction information
	G4ThreeVector fBeamMomentum;
	G4ThreeVector fVertexPos;

	// Particles to be produced
	std::vector<G4ThreeVector>    fPartPos;
	std::vector<G4ThreeVector>    fPartMom;  // Generated direction (no ms)
	std::vector<G4ThreeVector>    fPartRealMom; // Direction to go to Geant4
	std::vector<G4ParticleDefinition *> fPartType;

	G4double fRate;
	G4double fEffXs;
	G4double fAsym, fmAsym;

};

#endif//__REMOLLEVENT_HH
