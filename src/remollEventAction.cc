#include "remollEventAction.hh"
#include "remollGenericDetectorHit.hh"
#include "remollGenericDetectorSum.hh"

#include "G4Event.hh"
#include "G4HCofThisEvent.hh"
#include "G4VHitsCollection.hh"

#include "remollIO.hh"


remollEventAction::remollEventAction() { }

remollEventAction::~remollEventAction() { }

void remollEventAction::BeginOfEventAction(const G4Event*) { }

void remollEventAction::EndOfEventAction(const G4Event* evt )
{
  G4HCofThisEvent *HCE = evt->GetHCofThisEvent();

  // Traverse all hit collections, sort by output type
  for (int hcidx = 0; hcidx < HCE->GetCapacity(); hcidx++) {
    G4VHitsCollection* thiscol = HCE->GetHC(hcidx);
    if (thiscol){ // This is NULL if nothing is stored
      // Dynamic cast to test types, process however see fit and feed to IO
      
      ////  Generic Detector Hits ///////////////////////////////////
      if (remollGenericDetectorHitCollection *thiscast =
          dynamic_cast<remollGenericDetectorHitCollection*>(thiscol)) {
        for (unsigned int hidx = 0; hidx < thiscast->GetSize(); hidx++) {
          remollIO::GetInstance()->AddGenericDetectorHit((remollGenericDetectorHit *)
              thiscast->GetHit(hidx));
        }
      }

      ////  Generic Detector Sum ////////////////////////////////////
      if (remollGenericDetectorSumCollection *thiscast =
          dynamic_cast<remollGenericDetectorSumCollection*>(thiscol)) {
        for (unsigned int hidx = 0; hidx < thiscast->GetSize(); hidx++) {
          remollIO::GetInstance()->AddGenericDetectorSum((remollGenericDetectorSum *)
              thiscast->GetHit(hidx));
        }
      }

    }
  }

  // Fill tree and reset buffers
  remollIO* io = remollIO::GetInstance();
  io->FillTree();
  io->Flush();
}
