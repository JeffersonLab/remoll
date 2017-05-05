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

class G4GenericMessenger;

class remollEvent;
class remollBeamTarget;
class remollRunData;

class remollVEventGen {
    public:
	remollVEventGen();
	virtual ~remollVEventGen();

	remollEvent *GenerateEvent();

	G4String GetName(){ return fName; }

	void SetSampType( SampType_t st ) { fSampType = st; }
	void SetDoMultScatt( G4bool multscatt ){ fApplyMultScatt = multscatt; }

	virtual void SetThMin( double th ){ fTh_min = th; }
	virtual void SetThMax( double th ){ fTh_max = th; }
	virtual void SetPhMin( double ph ){ fPh_min = ph; }
	virtual void SetPhMax( double ph ){ fPh_max = ph; }
	virtual void SetEmin( double emin ){ fE_min = emin; }
	virtual void SetEmax( double emax ){ fE_max = emax; }
	virtual void SetThCoM_min( double th ){ fThCoM_min = th; }
	virtual void SetThCoM_max( double th ){ fThCoM_max = th; }

    protected:

	G4double fThCoM_min, fThCoM_max;
	G4double fTh_min, fTh_max;
	G4double fPh_min, fPh_max;
	G4double fE_min, fE_max;

    private:
	const G4String fName;

	remollBeamTarget *fBeamTarg;
	remollRunData    *fRunData;

	void PolishEvent(remollEvent *);
	
	// Pure virtual function that needs to be filled out
	virtual void SamplePhysics(remollVertex *, remollEvent *) = 0;

    protected:

	SampType_t fSampType;
	G4bool     fApplyMultScatt;

    protected:

	G4GenericMessenger* fMessenger;
};


#endif//__REMOLLVEVENTGEN_HH
