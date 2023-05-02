/*
 * remollGenExternal.cc
 *
 *  Created on: Mar 17, 2017
 *      Author: wdconinc
 */

#include "remollGenExternal.hh"

// Geant4 headers
#include "G4ParticleTable.hh"

// ROOT headers
#include "TFile.h"
#include "TTree.h"
#include "remollEvent.hh"
#include "remollVertex.hh"
#include "remolltypes.hh"

G4Mutex inFileMutex = G4MUTEX_INITIALIZER;

TFile* remollGenExternal::fFile = 0;
TTree* remollGenExternal::fTree = 0;
remollEvent_t* remollGenExternal::fEvent = 0;
std::vector<remollGenericDetectorHit_t>* remollGenExternal::fHit = 0;
Int_t remollGenExternal::fEntry = 0;
Int_t remollGenExternal::fEntries = 0;

remollGenExternal::remollGenExternal()
: remollVEventGen("external"),
  fzOffset(0), fDetectorID(28), fLoopID(1)
{
  fSamplingType = kNoTargetVolume;

  // Add to generic messenger
  fThisGenMessenger.DeclareMethod("file",&remollGenExternal::SetGenExternalFile,"External generator event filename");
  fThisGenMessenger.DeclareMethod("zOffset",&remollGenExternal::SetGenExternalZOffset,"External generator zOffset");
  fThisGenMessenger.DeclareMethod("detid",&remollGenExternal::SetGenExternalDetID,"External generator detector ID");
  fThisGenMessenger.DeclareMethod("startEvent",&remollGenExternal::SetGenExternalEntry,"External generator starting event: -1 starts random,  n starts at n (default n=0)");
}

remollGenExternal::~remollGenExternal()
{
  G4AutoLock inFileLock(&inFileMutex);

  // Close file which deletes tree
  if (fFile != nullptr) {
    fFile->Close();
    fTree = 0;
  }
}

void remollGenExternal::SetGenExternalFile(G4String& filename)
{
  G4AutoLock inFileLock(&inFileMutex);

  G4cout << "Setting the external file to " << filename << " from " << fFile << G4endl;
  // Close previous file
  if (fFile != nullptr) {
    fFile->Close();
    fFile = 0;
  }

  // Try to open filename
  fFile = new TFile(filename);
  if (fFile == nullptr) {
    G4cerr << "Could not open external event file " << filename << G4endl;
    return;
  }

  // Try to find tree in file
  fFile->GetObject("T",fTree);
  if (fTree == nullptr) {
    G4cerr << "Could not find tree T in event file (SetGenExternalFile)" << filename << G4endl;
    return;
  }

  // Nunber of entries
  fEntries = fTree->GetEntries();

  // Initialize tree
  if (fTree->GetBranch("hit") != nullptr) {
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

  if (fTree->GetBranch("ev") != nullptr) {
    fTree->SetBranchAddress("ev", &fEvent);
  } else {
    G4cerr << "Could not find branch ev in event file " << filename << G4endl;
    return;
  }
}

void remollGenExternal::SamplePhysics(remollVertex* /* vert */, remollEvent* evt)
{
  G4AutoLock inFileLock(&inFileMutex);
  // Check whether three exists
  if (fTree == nullptr) {
    G4cerr << "Could not find tree T in event file (SamplePhysics)" << G4endl;
    return;
  }
  // Loop until we find at least one event with some particles
  int number_of_particles = 0;
  do {
    // Read next event from tree and increment
    if (fEntry >= fEntries)
        fEntry = 0;
    fTree->GetEntry(fEntry++);
    // Weighting completely handled by event file
    if(fEvent){
      evt->SetEffCrossSection(fEvent->xs*microbarn);
      evt->SetQ2(fEvent->Q2);
      evt->SetW2(fEvent->W2);
      evt->SetAsymmetry(fEvent->A*ppb);
    }else{
      evt->SetEffCrossSection(microbarn);
      evt->SetQ2(999);
      evt->SetW2(999);
      evt->SetAsymmetry(1*ppb);
    }
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
      G4ThreeVector r(hit.x,hit.y,hit.z);
      G4ThreeVector p(hit.px,hit.py,hit.pz);
      G4ThreeVector zdir(0.0,0.0,1.0);
      if (fzOffset!=0.0) r += fzOffset*zdir.unit();
      evt->ProduceNewParticle(r,p,particlename);

      number_of_particles++;
    }

  } while (number_of_particles == 0);

}
