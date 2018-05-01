#include "remollVEventGen.hh"

#include <cassert>

#include "G4ParticleGun.hh"
#include "G4RotationMatrix.hh"
#include "G4GenericMessenger.hh"

#include "remollBeamTarget.hh"
#include "remollVertex.hh"
#include "remollEvent.hh"
#include "remollRun.hh"
#include "remollRunData.hh"

remollVEventGen::remollVEventGen(const G4String name)
: fName(name),
  fThCoM_min(0.0), fThCoM_max(180.0*deg),
  fTh_min(0.0), fTh_max(180.0*deg),
  fPh_min(0.0), fPh_max(360.0*deg),
  fE_min(0.0), fE_max(11.0*GeV),
  fNumberOfParticles(1),fParticleGun(0),
  fBeamTarg(0), fMessenger(0)
{
    // Set initial number of particles and create particle gun
    SetNumberOfParticles(fNumberOfParticles);

    // Create event generator messenger
    fMessenger = new G4GenericMessenger(this,"/remoll/","Remoll properties");
    fMessenger->DeclarePropertyWithUnit("emax","GeV",fE_max,"Maximum generation energy");
    fMessenger->DeclarePropertyWithUnit("emin","GeV",fE_min,"Minimum generation energy");
    fMessenger->DeclarePropertyWithUnit("thcommax","deg",fThCoM_max,"Maximum CoM generation theta angle");
    fMessenger->DeclarePropertyWithUnit("thcommin","deg",fThCoM_min,"Minimum CoM generation theta angle");
    fMessenger->DeclarePropertyWithUnit("thmax","deg",fTh_max,"Maximum generation theta angle");
    fMessenger->DeclarePropertyWithUnit("thmin","deg",fTh_min,"Minimum generation theta angle");
    fMessenger->DeclarePropertyWithUnit("phmax","deg",fPh_max,"Maximum generation phi angle");
    fMessenger->DeclarePropertyWithUnit("phmin","deg",fPh_min,"Minimum generation phi angle");
    fMessenger->DeclareMethod(
        "printlimits",
        &remollVEventGen::PrintEventGen,
        "Print the event generator limits");

    // Create event generator messenger
    fEvGenMessenger = new G4GenericMessenger(this,"/remoll/evgen/","Remoll event generator properties");
    fEvGenMessenger->DeclarePropertyWithUnit("emax","GeV",fE_max,"Maximum generation energy");
    fEvGenMessenger->DeclarePropertyWithUnit("emin","GeV",fE_min,"Minimum generation energy");
    fEvGenMessenger->DeclarePropertyWithUnit("thmax","deg",fTh_max,"Maximum generation theta angle");
    fEvGenMessenger->DeclarePropertyWithUnit("thmin","deg",fTh_min,"Minimum generation theta angle");
    fEvGenMessenger->DeclarePropertyWithUnit("phmax","deg",fPh_max,"Maximum generation phi angle");
    fEvGenMessenger->DeclarePropertyWithUnit("phmin","deg",fPh_min,"Minimum generation phi angle");
    fEvGenMessenger->DeclarePropertyWithUnit("thcommax","deg",fThCoM_max,"Maximum CoM generation theta angle");
    fEvGenMessenger->DeclarePropertyWithUnit("thcommin","deg",fThCoM_min,"Minimum CoM generation theta angle");
    fEvGenMessenger->DeclareMethod(
        "printlimits",
        &remollVEventGen::PrintEventGen,
        "Print the event generator limits");

    // Create specific event generator messenger
    fThisGenMessenger = new G4GenericMessenger(this,"/remoll/evgen/" + name + "/","Remoll " + name + " generator properties");

    fSampType       = kActiveTargetVolume;
    fApplyMultScatt = false;
}

remollVEventGen::~remollVEventGen()
{
    delete fThisGenMessenger;
    delete fEvGenMessenger;
    delete fMessenger;
}

void remollVEventGen::PrintEventGen()
{
  G4cout << "Event generator: " << fName << G4endl;
  G4cout << "E =     [" << fE_min/GeV  << "," << fE_max/GeV  << "] GeV" << G4endl;
  G4cout << "phi =   [" << fPh_min/deg << "," << fPh_max/deg << "] deg" << G4endl;
  G4cout << "theta = [" << fTh_min/deg << "," << fTh_max/deg << "] deg" << G4endl;
  G4cout << "theta (COM) = [" << fThCoM_min/deg << "," << fThCoM_max/deg << "] deg" << G4endl;
}

void remollVEventGen::SetNumberOfParticles(G4int n)
{
  // Store new number of particles
  fNumberOfParticles = n;

  // Delete old particle gun
  if (fParticleGun) {
    delete fParticleGun;
    fParticleGun = 0;
  }
  // Create new particle gun
  fParticleGun = new G4ParticleGun(fNumberOfParticles);
}

remollEvent* remollVEventGen::GenerateEvent()
{
    // Set up beam/target vertex
    remollVertex vert   = fBeamTarg->SampleVertex(fSampType);

    /////////////////////////////////////////////////////////////////////
    // Create and initialize values for event
    remollEvent *thisev = new remollEvent();
    thisev->SetBeamTarget(fBeamTarg);

    thisev->fVertexPos    = fBeamTarg->fVer;
    if( fApplyMultScatt ) {
        thisev->fBeamMomentum = fBeamTarg->fSampledEnergy*(fBeamTarg->fDir.unit());
    } else {
        thisev->fBeamMomentum = fBeamTarg->fSampledEnergy*G4ThreeVector(0.0, 0.0, 1.0);
    }
    /////////////////////////////////////////////////////////////////////

    SamplePhysics(&vert, thisev);

    PolishEvent(thisev);

    return thisev;
}


void remollVEventGen::PolishEvent(remollEvent *ev) {
    /*!
       Here it's our job to:
          Make sure the event is sane
          Apply multiple scattering effects to the final
        products if applicable
      Calculate rates from our given luminosity
      Calculate measured asymmetry from polarization
      Calculate vertex offsets
     */

    if( !ev->EventIsSane() ) {
        G4cerr << __FILE__ << " line " << __LINE__ << ":  Event check failed for generator " << fName << ".  Aborting" << G4endl;
        ev->Print();
        exit(1);
    }

    G4ThreeVector rotax      = (fBeamTarg->fDir.cross(G4ThreeVector(0.0, 0.0, 1.0))).unit();
    G4RotationMatrix msrot;
    msrot.rotate(fBeamTarg->fDir.theta(), rotax);

    std::vector<G4ThreeVector>::iterator iter;

    if( fApplyMultScatt ) {
        for( iter = ev->fPartRealMom.begin(); iter != ev->fPartRealMom.end(); iter++ ) {
            //  rotate direction vectors based on multiple scattering
            (*iter) *= msrot;
        }

        // Rotate position offsets due to multiple scattering
        for( iter = ev->fPartPos.begin(); iter != ev->fPartPos.end(); iter++ ) {
            //  rotate direction vectors based on multiple scattering
            (*iter) *= msrot;
        }
    }

    // Add base vertex
    for( iter = ev->fPartPos.begin(); iter != ev->fPartPos.end(); iter++ ) {
        (*iter) += ev->fVertexPos;
    }
    
    // Get number of thrown events
    remollRunData* rundata = remollRun::GetRunData();
    G4double nthrown = rundata->GetNthrown();

    // Calculate rate
    if ( ev->fRate == 0 ){// If the rate is set to 0 then calculate it using the cross section
    	ev->fRate  = ev->fEffXs*fBeamTarg->GetEffLumin()/nthrown;
    }
    else { // For LUND - calculate rate and cross section	
    	ev->fEffXs = ev->fRate*nthrown/(fBeamTarg->GetEffLumin());
    	ev->fRate = ev->fRate/nthrown;
    }

    ev->fmAsym = ev->fAsym*fBeamTarg->fBeamPolarization;
}

