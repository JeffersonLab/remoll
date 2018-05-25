#ifndef __REMOLLVEVENTGEN_HH
#define __REMOLLVEVENTGEN_HH

#include "remolltypes.hh"
#include "remollglobs.hh"
#include "remollVertex.hh"

#include "G4String.hh"
#include "G4ThreeVector.hh"

/*!
   Generic base class for event generators
   This provides an interface for everyone to
   derive from.

   Ultimately this returns a remollEvent which is
   what the PrimaryGeneratorAction is going to use and
   contains information that will go in the output.

   It needs to be aware of remollBeamTarget and remollRunData,
   take a generically generated event assuming ideal beam
   and transform it into what is going to be simulated.
*/

class G4ParticleGun;
class G4GenericMessenger;

class remollEvent;
class remollBeamTarget;
class remollRunData;

class remollVEventGen {
    public:
	remollVEventGen(G4String name);
	virtual ~remollVEventGen();

	void PrintEventGen();

	remollEvent *GenerateEvent();

	G4String GetName() { return fName; }

	void SetBeamTarget(remollBeamTarget* bt) {
	  fBeamTarg = bt;
	}

	void SetSampType( SampType_t st ) { fSampType = st; }
	void SetDoMultScatt( G4bool multscatt ){ fApplyMultScatt = multscatt; }


    protected:
	// Generator name
        G4String fName;

    protected:
	// Generation limits
	G4double fThCoM_min, fThCoM_max;
	G4double fTh_min, fTh_max;
	G4double fPh_min, fPh_max;
	G4double fE_min, fE_max;

    protected:
	// Number of particles
	G4int fNumberOfParticles;
	// Particle gun
        G4ParticleGun* fParticleGun;
    public:
	// Set the number of particles
	void SetNumberOfParticles(G4int n);
	// Get a new particle gun for this generator
        G4ParticleGun* GetParticleGun() const { return fParticleGun; }

    protected:
	remollBeamTarget* fBeamTarg;

	void PolishEvent(remollEvent *);

	// Pure virtual function that needs to be filled out
	virtual void SamplePhysics(remollVertex *, remollEvent *) = 0;

    protected:

	SampType_t fSampType;
	G4bool     fApplyMultScatt;

    protected:
	// Generic messenger as protected to be used in derived classes
	G4GenericMessenger* fMessenger;
	G4GenericMessenger* fEvGenMessenger;
	G4GenericMessenger* fThisGenMessenger;
};


#endif//__REMOLLVEVENTGEN_HH
