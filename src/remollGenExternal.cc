/*
 * remollGenExternal.cc
 *
 *  Created on: Mar 17, 2017
 *      Author: wdconinc
 */

#include "remollGenExternal.hh"
// Geant4 headers
#include "G4ParticleTable.hh"
#include "G4GenericMessenger.hh"

// ROOT headers
#include "TFile.h"
#include "TTree.h"
#include "remollEvent.hh"
#include "remollVertex.hh"
#include "remolltypes.hh"

G4Mutex inFileMutex = G4MUTEX_INITIALIZER;

remollGenExternal::remollGenExternal()
: remollVEventGen("external"),
  fFile(0), fTree(0),
  fEntry(0), fEntries(0),
  fEvent(0), fHit(0),
  fzOffset(0), fDetectorID(28), fLoopID(1)
{
  fSampType = kNoTargetVolume;
  // Add to generic messenger
  fThisGenMessenger->DeclareMethod("file",&remollGenExternal::SetGenExternalFile,"External generator event filename");
  fThisGenMessenger->DeclareMethod("zOffset",&remollGenExternal::SetGenExternalZOffset,"External generator zOffset");
  fThisGenMessenger->DeclareMethod("detid",&remollGenExternal::SetGenExternalDetID,"External generator detector ID");
  fThisGenMessenger->DeclareMethod("startEvent",&remollGenExternal::SetGenExternalEntry,"External generator starting event: -1 starts random,  n starts at n (default n=0)");
}

remollGenExternal::~remollGenExternal()
{
  G4AutoLock inFileLock(&inFileMutex);
  // Close file which deletes tree
  if (fFile) {
    fFile->Close();
    fTree = 0;
  }
}

void remollGenExternal::SetGenExternalFile(G4String& filename)
{
  G4AutoLock inFileLock(&inFileMutex);
  G4cout << "Setting the external file to " << filename << " from " << fFile << G4endl;
  // Close previous file
  if (fFile) {
    fFile->Close();
    fFile = 0;
  }

  // Try to open filename
  fFile = new TFile(filename);
  if (! fFile) {
    G4cerr << "Could not open external event file " << filename << G4endl;
    return;
  }

  // Try to find tree in file
  fFile->GetObject("T",fTree);
  if (! fTree) {
    G4cerr << "Could not find tree T in event file (SetGenExternalFile)" << filename << G4endl;
    return;
  }
  inFileLock.unlock();

  // Get number of entries
  fEntries = fTree->GetEntries();

  // Initialize tree
  if (fTree->GetBranch("hit")) {
    fTree->SetBranchAddress("hit", &fHit);
  } else {
    G4cerr << "Could not find branch hit in event file " << filename << G4endl;
    return;
  }

  if (fTree->GetBranch("rate")) {
    fTree->SetBranchAddress("rate", &rate);
  }else{
    G4cerr << "Warning! could not find rate branch. Set rate to 1"<<G4endl;
    rate = 1;
  }
/* event tree removed by Cameron 11/15/2018
*  if (fTree->GetBranch("ev")) {
*    fTree->SetBranchAddress("ev", &fEvent);
*  } else {
*    G4cerr << "Could not find branch ev in event file " << filename << G4endl;
*    return;
*  }
*/
}

void remollGenExternal::SamplePhysics(remollVertex* /* vert */, remollEvent* evt)
{
  // Check whether three exists
  if (! fTree) {
    G4cerr << "Could not find tree T in event file (SamplePhysics)" << G4endl;
    return;
  }
  // Loop until we find at least one event with some particles
  int number_of_particles = 0;
  do {

    // Read next event from tree and increment
    //fTree->GetEntry(fEntry++);
    if (fEntry >= fEntries)
        fEntry = 0;
    fTree->GetEntry(fEntry++);
    
/* event tree removed by Cameron 11/15/2018
*    // Weighting completely handled by event file
*    evt->SetEffCrossSection(fEvent->xs*microbarn);
*    evt->SetQ2(fEvent->Q2);
*    evt->SetW2(fEvent->W2);
*    evt->SetAsymmetry(fEvent->A*ppb);
*/
    evt->SetEffCrossSection(619.5*microbarn);
    evt->SetQ2(0.0);
    evt->SetW2(4e15);
    evt->SetAsymmetry(-42.0*ppb);
    
    if(!std::isnan(rate) && !std::isinf(rate)){
      evt->SetRate(rate/s);
    }

    // Loop over all hits in this event
    for (size_t i = 0; i < fHit->size(); i++) {
      // Create local copy of this hit
      remollGenericDetectorHit_t hit = fHit->at(i);

      // Select only the requested detector ID
      if (hit.det != fDetectorID) continue;

      // Get particle name
      G4ParticleTable* particletable = G4ParticleTable::GetParticleTable();
      G4ParticleDefinition* particle = particletable->FindParticle(hit.pid);
      G4String particlename = particle->GetParticleName();

      // Throw new particle
      evt->ProduceNewParticle(
          G4ThreeVector(hit.x, hit.y, hit.z + fzOffset),
          G4ThreeVector(hit.px, hit.py, hit.pz),
          particlename);
      number_of_particles++;
    }

  } while (number_of_particles == 0);

}
