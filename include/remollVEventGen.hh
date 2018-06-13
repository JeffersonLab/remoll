#ifndef __REMOLLVEVENTGEN_HH
#define __REMOLLVEVENTGEN_HH

#include "remolltypes.hh"
#include "remollglobs.hh"
#include "G4String.hh"
#include "G4ThreeVector.hh"
#include "remollVertex.hh"

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
	void SetDoMultScatt( G4bool ms ){ fApplyMultScatt = ms; }

	virtual void SetThMin( double th ){ fTh_min = th; }
	virtual void SetThMax( double th ){ fTh_max = th; }
	virtual void SetPhMin( double ph ){ fPh_min = ph; }
	virtual void SetPhMax( double ph ){ fPh_max = ph; }
	virtual void SetEmin( double ){ G4cerr << __FILE__ << " line " << __LINE__ << " " << __PRETTY_FUNCTION__ << " :  Generator does not respond to this command" << G4endl; }
	virtual void SetEmax( double ){ G4cerr << __FILE__ << " line " << __LINE__ << " " << __PRETTY_FUNCTION__ << " :  Generator does not respond to this command" << G4endl; }
	virtual void SetThCoM_min( double ){ G4cerr << __FILE__ << " line " << __LINE__ << " " << __PRETTY_FUNCTION__ << " :  Generator does not respond to this command" << G4endl; }
	virtual void SetThCoM_max( double ){ G4cerr << __FILE__ << " line " << __LINE__ << " " << __PRETTY_FUNCTION__ << " :  Generator does not respond to this command" << G4endl; }

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

};


#endif//__REMOLLVEVENTGEN_HH
