#include "remollVEventGen.hh"

#include "remollBeamTarget.hh"
#include "remollVertex.hh"
#include "remollEvent.hh"
#include "remollRun.hh"

remollVEventGen::remollVEventGen(){
    fBeamTarg = remollBeamTarget::GetBeamTarget();
    fRun      = remollRun::GetRun();

    fSampType       = kCryogen;
    fApplyMultScatt = false;
}

remollVEventGen::~remollVEventGen(){
}

remollEvent *remollVEventGen::GenerateEvent(){
    // Set up beam/target vertex
    remollVertex vert   = fBeamTarg->SampleVertex(fSampType);

    remollEvent *thisev = SamplePhysics(&vert);

    PolishEvent(thisev);

    return thisev;
}


void remollVEventGen::PolishEvent(remollEvent *ev){
    /*!
       Here it's our job to:
          Apply multiple scattering effects to the final
	    products if applicable
	  Calculate rates from our given luminosity
     */

    if( fApplyMultScatt ){
	// FIXME - rotate direction vectors
	// based on sampling
    } else {
	// FIXME too
    }

    ev->fRate = ev->fEffXs*fBeamTarg->GetEffLumin()/((G4double) fRun->GetNthrown());

    return;
}
