#include "remollVEventGen.hh"

#include "G4RotationMatrix.hh"

#include "remollBeamTarget.hh"
#include "remollVertex.hh"
#include "remollEvent.hh"
#include "remollRun.hh"
#include "remollRunData.hh"

remollVEventGen::remollVEventGen(){
    fBeamTarg = remollBeamTarget::GetBeamTarget();
    fRunData  = remollRun::GetRun()->GetData();

    fSampType       = kCryogen;
    fApplyMultScatt = false;
}

remollVEventGen::~remollVEventGen(){
}

remollEvent *remollVEventGen::GenerateEvent(){
    // Set up beam/target vertex
    remollVertex vert   = fBeamTarg->SampleVertex(fSampType);

    /////////////////////////////////////////////////////////////////////
    // Create and initialize values for event
    remollEvent *thisev = new remollEvent();
    thisev->fVertexPos    = fBeamTarg->fVer;
    if( fApplyMultScatt ){
	thisev->fBeamMomentum = fBeamTarg->fSampE*(fBeamTarg->fDir.unit());
    } else {
	thisev->fBeamMomentum = fBeamTarg->fSampE*G4ThreeVector(0.0, 0.0, 1.0);
    }
    /////////////////////////////////////////////////////////////////////
   
    SamplePhysics(&vert, thisev);

    PolishEvent(thisev);

    return thisev;
}


void remollVEventGen::PolishEvent(remollEvent *ev){
    /*!
       Here it's our job to:
          Make sure the event is sane
          Apply multiple scattering effects to the final
	    products if applicable
	  Calculate rates from our given luminosity
	  Calculate measured asymmetry from polarization
	  Calculate vertex offsets
     */

    if( !ev->EventIsSane() ){
	G4cerr << __FILE__ << " line " << __LINE__ << ":  Event check failed for generator " << fName << ".  Aborting" << G4endl;
	ev->Print();
	exit(1);
    }

    G4ThreeVector rotax      = (fBeamTarg->fDir.cross(G4ThreeVector(0.0, 0.0, 1.0))).unit();
    G4RotationMatrix msrot;
    msrot.rotate(fBeamTarg->fDir.theta(), rotax);

    std::vector<G4ThreeVector>::iterator iter;

    if( fApplyMultScatt ){
	for( iter = ev->fPartRealMom.begin(); iter != ev->fPartRealMom.end(); iter++ ){
	    //  rotate direction vectors based on multiple scattering
	    (*iter) *= msrot;
	}

	// Rotate position offsets due to multiple scattering
	for( iter = ev->fPartPos.begin(); iter != ev->fPartPos.end(); iter++ ){
	    //  rotate direction vectors based on multiple scattering
	    (*iter) *= msrot;
	}
    }

    // Add base vertex
    for( iter = ev->fPartPos.begin(); iter != ev->fPartPos.end(); iter++ ){
	(*iter) += ev->fVertexPos;
    }

    ev->fRate  = ev->fEffXs*fBeamTarg->GetEffLumin()/((G4double) fRunData->GetNthrown());
    ev->fmAsym = ev->fAsym*fBeamTarg->fBeamPol;

    return;
}


















