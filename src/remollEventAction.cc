#include "remollEventAction.hh"
#include "remollGenericDetectorHit.hh"
#include "remollGenericDetectorSum.hh"
#include "remollPrimaryGeneratorAction.hh"

#include "G4Event.hh"
#include "G4HCofThisEvent.hh"
#include "G4VHitsCollection.hh"

#include "remollIO.hh"
#include "remollEvent.hh"
#include "remollTrackReconstruct.hh"

#include "G4Threading.hh"
#include "G4AutoLock.hh"
namespace { G4Mutex remollEventActionMutex = G4MUTEX_INITIALIZER; }

remollEventAction::remollEventAction()
  : fPrimaryGeneratorAction(0),fEventSeed("") { }

remollEventAction::~remollEventAction() { }

void remollEventAction::BeginOfEventAction(const G4Event* /* event */) { }

void remollEventAction::EndOfEventAction(const G4Event* aEvent)
{
  // We collect all interaction with remollIO in this thread for as
  // little locking as possible. This means that all the thread local
  // information must be retrieved from here.

  // Lock mutex
  G4AutoLock lock(&remollEventActionMutex);
  remollIO* io = remollIO::GetInstance();

  // Store random seed
  //fEventSeed = aEvent->GetRandomNumberStatus();
  io->SetEventSeed(fEventSeed);

  // Get primary event action information
  const remollEvent* event = fPrimaryGeneratorAction->GetEvent();
  io->SetEventData(event);

  // Create track reconstruction object
  remollTrackReconstruct track;

  // Traverse all hit collections, sort by output type
  G4HCofThisEvent *HCE = aEvent->GetHCofThisEvent();
  for (int hcidx = 0; hcidx < HCE->GetCapacity(); hcidx++) {
    G4VHitsCollection* thiscol = HCE->GetHC(hcidx);
    if (thiscol){ // This is NULL if nothing is stored

      // Dynamic cast to test types, process however see fit and feed to IO

      ////  Generic Detector Hits ///////////////////////////////////
      if (remollGenericDetectorHitCollection *thiscast =
          dynamic_cast<remollGenericDetectorHitCollection*>(thiscol)) {
        for (unsigned int hidx = 0; hidx < thiscast->GetSize(); hidx++) {

	  remollGenericDetectorHit *currentHit =
	    (remollGenericDetectorHit *) thiscast->GetHit(hidx);

	  ////  store GEM hits for track reconstruction
	  if(currentHit->fDetID >= 501 && currentHit->fDetID <= 504){
	    track.AddHit(currentHit);
	  }
	  // non-GEM hits
	  else io->AddGenericDetectorHit(currentHit);
        }
      }

      ////  Generic Detector Sum ////////////////////////////////////
      if (remollGenericDetectorSumCollection *thiscast =
          dynamic_cast<remollGenericDetectorSumCollection*>(thiscol)) {
        for (unsigned int hidx = 0; hidx < thiscast->GetSize(); hidx++) {
          io->AddGenericDetectorSum((remollGenericDetectorSum *)
                                    thiscast->GetHit(hidx));
        }
      }

    }
  }

  ////  reconstruct tracks, and store them into rootfile
  if (track.GetTrackHitSize() > 0) {

    track.ReconstructTrack();

    std::vector<remollGenericDetectorHit*> rechits = track.GetTrack();

    for (size_t j = 0; j < rechits.size(); j++)
      io->AddGenericDetectorHit((remollGenericDetectorHit *) rechits[j]);
  }

  // Fill tree and reset buffers
  io->FillTree();
  io->Flush();
}
