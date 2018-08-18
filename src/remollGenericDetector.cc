#include "remollGenericDetector.hh"

#include "G4OpticalPhoton.hh"
#include "G4SDManager.hh"
#include "G4GenericMessenger.hh"

#include "remollGenericDetectorHit.hh"
#include "remollGenericDetectorSum.hh"

#include "G4RunManager.hh"
#include "G4TrajectoryContainer.hh"
#include "G4TrajectoryPoint.hh"

#include <sstream>

std::list<remollGenericDetector*> remollGenericDetector::fGenericDetectors = std::list<remollGenericDetector*>();
G4GenericMessenger* remollGenericDetector::fStaticMessenger = 0;

remollGenericDetector::remollGenericDetector( G4String name, G4int detnum )
: G4VSensitiveDetector(name),fHitColl(0),fSumColl(0),fEnabled(true)
{
  fDetNo = detnum;
  assert( fDetNo > 0 );

  fDetectSecondaries = true;
  fDetectOpticalPhotons = false;
  fDetectLowEnergyNeutrals = false;
  fBoundaryHits = false;

  std::stringstream genhit;
  genhit << "genhit_" << detnum;
  collectionName.insert(G4String(genhit.str()));

  std::stringstream gensum;
  gensum << "gensum_" << detnum;
  collectionName.insert(G4String(gensum.str()));

  fHCID = -1;
  fSCID = -1;

  // Create generic detector messenger
  std::stringstream ss;
  ss << fDetNo;
  fMessenger = new G4GenericMessenger(this,"/remoll/SD/det_" + ss.str() + "/","Remoll SD properties for " + name);
  fMessenger->DeclareProperty(
      "enable",
      fEnabled,
      "Enable recording of hits in this detector")
      .SetParameterName("flag",true).SetDefaultValue(true);

  // Create static messenger
  fStaticMessenger = new G4GenericMessenger(this,"/remoll/SD/","Remoll SD properties");
  fStaticMessenger->DeclareMethod(
    "enable_all",
    &remollGenericDetector::SetAllEnabled,
    "Enable recording of hits in all detectors");
  fStaticMessenger->DeclareMethod(
    "disable_all",
    &remollGenericDetector::SetAllDisabled,
    "Disable recording of hits in all detectors");
  fStaticMessenger->DeclareMethod(
    "print_all",
    &remollGenericDetector::PrintAll,
    "Print all detectors");

  // Add to static list
  InsertGenericDetector(this);
}

remollGenericDetector::~remollGenericDetector()
{
  EraseGenericDetector(this);
  delete fMessenger;
}

void remollGenericDetector::Initialize(G4HCofThisEvent*)
{
    fHitColl = new remollGenericDetectorHitCollection(SensitiveDetectorName, collectionName[0]);
    fSumColl = new remollGenericDetectorSumCollection(SensitiveDetectorName, collectionName[1]);

    fSumMap.clear();
}

G4bool remollGenericDetector::ProcessHits(G4Step *step, G4TouchableHistory *)
{
    G4bool badedep = false;
    G4bool badhit  = false;

    // Ignore this detector if disabled
    if (! fEnabled) {
      static bool has_been_warned = false;
      if (! has_been_warned) {
        G4cout << "remoll: Some detectors have been explicitly disabled in macros." << G4endl;
        G4cout << "remoll: To disable/enable detectors, use the following syntax:" << G4endl;
        G4cout << "remoll:   /remoll/SD/print_all" << G4endl;
        G4cout << "remoll:   /remoll/SD/enable_all" << G4endl;
        G4cout << "remoll:   /remoll/SD/disable_all" << G4endl;
        G4cout << "remoll:   /remoll/SD/det_4001/enable" << G4endl;
        G4cout << "remoll:   /remoll/SD/det_4001/disable" << G4endl;
        has_been_warned = true;
      }
      return false;
    }

    // Ignore optical photons as hits (but still simulate them
    // so they can knock out electrons of the photocathode)
    if (! fDetectOpticalPhotons
        && step->GetTrack()->GetDefinition() == G4OpticalPhoton::OpticalPhotonDefinition()) {
      static bool has_been_warned = false;
      if (! has_been_warned) {
        G4cout << "remoll: Optical photons simulated but not stored for all detectors." << G4endl;
        G4cout << "remoll: To save optical photon hits, use the following in gdml:" << G4endl;
        G4cout << "remoll:   <auxiliary auxtype=\"DetType\" auxvalue=\"opticalphoton\"/>" << G4endl;
        has_been_warned = true;
      }
      return false;
    }

    // Det Type: Ignore neutral particles below 0.1 MeV
    G4double charge = step->GetTrack()->GetDefinition()->GetPDGCharge();
    if (! fDetectLowEnergyNeutrals
        && charge == 0.0 && step->GetTrack()->GetTotalEnergy() < 0.1*CLHEP::MeV) {
      static bool has_been_warned = false;
      if (! has_been_warned) {
        G4cout << "remoll: <0.1 MeV neutrals simulated but not stored for all detectors." << G4endl;
        G4cout << "remoll: To save low energy neutral hits, use the following in gdml:" << G4endl;
        G4cout << "remoll:   <auxiliary auxtype=\"DetType\" auxvalue=\"lowenergyneutral\"/>" << G4endl;
        has_been_warned = true;
      }
      return false;
    }

    // Get the step point and track
    G4StepPoint* prepoint = step->GetPreStepPoint();
    G4StepPoint* postpoint = step->GetPostStepPoint();
    G4Track*     track = step->GetTrack();

    // Get touchable volume info
    G4TouchableHistory *hist = (G4TouchableHistory*)(prepoint->GetTouchable());
    G4int  copyID = hist->GetVolume()->GetCopyNo();//return the copy id of the logical volume

    G4double edep = step->GetTotalEnergyDeposit();

    // We're just going to record primary particles and things
    // that have just entered our boundary
    //the following condition ensure that not all the hits are recorded. This will reflect in the energy deposit sum from the hits compared to the energy deposit from the hit sum detectors.
    badhit = true;
    if (track->GetCreatorProcess() == 0 ||
	(fDetectSecondaries && prepoint->GetStepStatus() == fGeomBoundary)) {
	badhit = false;
    }

    badedep = false;
    if (edep <= 0.0) {
        badedep = true;
    }

    // Det Type: Only detect hits that are on the incident boundary edge of the geometry in question
    if (fBoundaryHits
        && prepoint->GetStepStatus() != fGeomBoundary){
      static bool has_been_warned = false;
      if (! has_been_warned) {
        G4cout << "remoll: only hits on the boundary are being stored for boundaryhits==true detectors." << G4endl;
        G4cout << "remoll: To save just boundary hits alone, use the following in gdml:" << G4endl;
        G4cout << "remoll:   <auxiliary auxtype=\"DetType\" auxvalue=\"boundaryhits\"/>" << G4endl;
        has_been_warned = true;
      }
      return false;
    }

    /////////////////////////////////////////////////////

    // Do the actual data grabbing

    if (! badedep) {
        // Sum
        remollGenericDetectorSum* thissum = 0;
        if (! fSumMap.count(copyID)) {
	    thissum = new remollGenericDetectorSum(fDetNo, copyID);
	    fSumMap[copyID] = thissum;
	    fSumColl->insert(thissum);
        } else thissum = fSumMap[copyID];

        // Add energy deposit
        thissum->AddEDep(track->GetDefinition()->GetPDGEncoding(), prepoint->GetPosition(), edep );
    }

    if (! badhit) {
        // Hit
        remollGenericDetectorHit* thishit = new remollGenericDetectorHit(fDetNo, copyID);
        fHitColl->insert( thishit );

        // Which point do we store?
        G4StepPoint* point = 0;
        // optical absorption
        if (step->GetTrack()->GetDefinition() == G4OpticalPhoton::OpticalPhotonDefinition()
         && postpoint->GetStepStatus() == fGeomBoundary) {
          point = postpoint;
        // all other cases
        } else {
          point = prepoint;
        }

        // Positions
        G4ThreeVector global_position = point->GetPosition();
        G4ThreeVector local_position = point->GetTouchable()->GetHistory()->GetTopTransform().TransformPoint(global_position);
        thishit->f3X  = global_position;
        thishit->f3Xl = local_position;

        thishit->f3V  = track->GetVertexPosition();
        thishit->f3P  = track->GetMomentum();
        thishit->f3S  = track->GetPolarization();

        thishit->fTime = point->GetGlobalTime();

        thishit->f3dP = track->GetMomentumDirection();

        thishit->fP = track->GetMomentum().mag();
        thishit->fE = track->GetTotalEnergy();
        thishit->fM = track->GetDefinition()->GetPDGMass();

        thishit->fTrID  = track->GetTrackID();
        thishit->fmTrID = track->GetParentID();
        thishit->fPID   = track->GetDefinition()->GetPDGEncoding();
        thishit->fEdep  = edep; 
        // FIXME - Enumerate encodings
        thishit->fGen   = (long int) track->GetCreatorProcess();

        thishit->fEdep  = step->GetTotalEnergyDeposit();
    }

    return !badedep && !badhit;
}

void remollGenericDetector::EndOfEvent(G4HCofThisEvent* HCE)
{
    G4SDManager *sdman = G4SDManager::GetSDMpointer();

    if (fHCID < 0) { fHCID = sdman->GetCollectionID(collectionName[0]); }
    if (fSCID < 0) { fSCID = sdman->GetCollectionID(collectionName[1]); }

    HCE->AddHitsCollection(fHCID, fHitColl);
    HCE->AddHitsCollection(fSCID, fSumColl);
}


