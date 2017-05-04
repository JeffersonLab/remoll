#include "remollEventAction.hh"
#include "remollGenericDetectorHit.hh"
#include "remollGenericDetectorSum.hh"

#include "G4Event.hh"
#include "G4HCofThisEvent.hh"
#include "G4VHitsCollection.hh"

#include "remollIO.hh"


remollEventAction::remollEventAction()
: fCounter(0) { }

remollEventAction::~remollEventAction() { }

void remollEventAction::BeginOfEventAction(const G4Event* ev) {
  // Start timer at first event
  if (fCounter == 0) fTimer.Start();

  // Pretty ongoing status
  const G4int interval = 1000;
  if ((fCounter % interval) == 0) {

    // Stop timer (running timer cannot be read)
    fTimer.Stop();

    // Print event number
    G4cout << "Event " << ev->GetEventID();
    // Only print duration per event when meaningful (avoid division by zero)
    if (ev->GetEventID() > 0)
      G4cout << " (" << std::setprecision(3) << std::fixed
        << 1000.0 * fTimer.GetRealElapsed()/float(interval) << " ms/event)";
    // Carriage return without newline
    G4cout << "\r" << std::flush;

    // Start timer again
    fTimer.Start();
  }
}

void remollEventAction::EndOfEventAction(const G4Event* evt )
{
  G4HCofThisEvent *HCE = evt->GetHCofThisEvent();

  G4VHitsCollection *thiscol;

  // Traverse all hit collections, sort by output type
  for( int hcidx = 0; hcidx < HCE->GetCapacity(); hcidx++ ){
    thiscol = HCE->GetHC(hcidx);
    if(thiscol){ // This is NULL if nothing is stored
      // Dyanmic cast to test types, process however see fit and feed to IO
      
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

  // Increment counter
  fCounter++;

  // Fill tree and reset buffers
  remollIO* io = remollIO::GetInstance();
  //fIO->FillTree();
  //fIO->Flush();
}
