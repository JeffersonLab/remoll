#include "remollGenericDetector.hh"

#include "G4OpticalPhoton.hh"
#include "G4SDManager.hh"
#include "G4GenericMessenger.hh"

#include "remollGenericDetectorHit.hh"
#include "remollGenericDetectorSum.hh"
#include "remollUserTrackInformation.hh"

#include "G4RunManager.hh"
#include "G4TrajectoryContainer.hh"
#include "G4TrajectoryPoint.hh"

#include <sstream>

std::list<remollGenericDetector*> remollGenericDetector::fGenericDetectors = std::list<remollGenericDetector*>();

G4GenericMessenger* remollGenericDetector::fStaticMessenger = 0;

remollGenericDetector::remollGenericDetector( G4String name, G4int detnum )
: G4VSensitiveDetector(name),fHitColl(0),fSumColl(0),fEnabled(true)
{
  assert(detnum > 0);
  SetDetNo(detnum);

  fDetectSecondaries = false;
  fDetectOpticalPhotons = false;
  fDetectLowEnergyNeutrals = false;
  fDetectBoundaryHits = false;

  std::stringstream genhit;
  genhit << "genhit_" << detnum;
  collectionName.insert(G4String(genhit.str()));

  std::stringstream gensum;
  gensum << "gensum_" << detnum;
  collectionName.insert(G4String(gensum.str()));

  fHCID = -1;
  fSCID = -1;

  // Create static messenger
  BuildStaticMessenger();

  // Add to static list
  InsertGenericDetector(this);
}

remollGenericDetector::~remollGenericDetector()
{
  EraseGenericDetector(this);

  fSumMap.clear();
}

void remollGenericDetector::BuildStaticMessenger()
{
  // Mutex before accessing static members
  G4AutoLock lock(&remollGenericDetectorMutex);

  // If already built, just return
  if (fStaticMessenger) return;

  fStaticMessenger = new G4GenericMessenger(this,"/remoll/SD/","Remoll SD properties");
  fStaticMessenger->DeclareMethod(
    "enable",
    &remollGenericDetector::SetOneEnabled,
    "Enable recording of hits in specified detectors");
  fStaticMessenger->DeclareMethod(
    "disable",
    &remollGenericDetector::SetOneDisabled,
    "Disable recording of hits in specified detectors");
  fStaticMessenger->DeclareMethod(
    "enable_all",
    &remollGenericDetector::SetAllEnabled,
    "Enable recording of hits in all detectors");
  fStaticMessenger->DeclareMethod(
    "disable_all",
    &remollGenericDetector::SetAllDisabled,
    "Disable recording of hits in all detectors");
  fStaticMessenger->DeclareMethod(
    "enable_range",
    &remollGenericDetector::SetRangeEnabled,
    "Enable recording of hits in range of detectors");
  fStaticMessenger->DeclareMethod(
    "disable_range",
    &remollGenericDetector::SetRangeDisabled,
    "Disable recording of hits in range of detectors");
  fStaticMessenger->DeclareMethod(
    "print_all",
    &remollGenericDetector::PrintAll,
    "Print all detectors");

  fStaticMessenger->DeclareMethod(
    "summary",
    &remollGenericDetector::PrintSummary,
    "Print all detectors");

  fStaticMessenger->DeclareMethod(
    "detect",
    &remollGenericDetector::SetOneDetectorType,
    "Set detector type");
  fStaticMessenger->DeclareMethod(
    "detect_range",
    &remollGenericDetector::SetRangeDetectorType,
    "Set detector type");
}

void remollGenericDetector::Initialize(G4HCofThisEvent*)
{
  fHitColl = new remollGenericDetectorHitCollection(SensitiveDetectorName, collectionName[0]);
  fSumColl = new remollGenericDetectorSumCollection(SensitiveDetectorName, collectionName[1]);

  fSumMap.clear();
}

G4bool remollGenericDetector::ProcessHits(G4Step* step, G4TouchableHistory*)
{
    // Process the hits in this detector:
    // - if the detector is disabled entirely, we do not store anything, not even sums
    // - all hits are always processed into detector sums with energy deposition
    // - depending on the detector settings we can return false without writing an
    //   individual hit
    // - if we do write an individual hit we return true

    // Ignore this detector if disabled as set by /remoll/SD/disable
    if (! fEnabled) {
      static bool has_been_warned = false;
      if (! has_been_warned) {
        G4cout << "remoll: Some detectors have been explicitly disabled in macros." << G4endl;
        G4cout << "remoll: To disable/enable detectors, use the following syntax:" << G4endl;
        G4cout << "remoll:   /remoll/SD/print_all" << G4endl;
        G4cout << "remoll:   /remoll/SD/enable_all" << G4endl;
        G4cout << "remoll:   /remoll/SD/disable_all" << G4endl;
        G4cout << "remoll:   /remoll/SD/enable 4001" << G4endl;
        G4cout << "remoll:   /remoll/SD/disable 4001" << G4endl;
        has_been_warned = true;
      }
      return false;
    }


    // Get the step point and track
    G4Track*     track = step->GetTrack();
    G4StepPoint* prepoint  = step->GetPreStepPoint();
    G4StepPoint* postpoint = step->GetPostStepPoint();
    G4ParticleDefinition* particle = track->GetDefinition();

    // Get copy ID from touchable history
    G4TouchableHistory* hist
      = (G4TouchableHistory*) (prepoint->GetTouchable());
    G4VPhysicalVolume* volume = hist->GetVolume();
    G4int copyID = volume->GetCopyNo();


    // Add energy deposit to detector sum
    G4int pid = particle->GetPDGEncoding();
    G4ThreeVector pos = prepoint->GetPosition();
    G4double edep = step->GetTotalEnergyDeposit();

    // Create a detector sum for this detector, if necessary
    if (! fSumMap.count(copyID)) {
      remollGenericDetectorSum* sum = new remollGenericDetectorSum(fDetNo, copyID);
      fSumMap[copyID] = sum;
      fSumColl->insert(sum);
    }
    // Add to sum for this event only
    remollGenericDetectorSum* sum = fSumMap[copyID];
    sum->AddEDep(pid, pos, edep);

    // Create a running sum for this detector, if necessary
    if (! fRunningSumMap.count(copyID)) {
      remollGenericDetectorSum* sum = new remollGenericDetectorSum(fDetNo, copyID);
      fRunningSumMap[copyID] = sum;
    }
    // Add to running sum of all events
    remollGenericDetectorSum* runningsum = fRunningSumMap[copyID];
    runningsum->AddEDep(pid, pos, edep);

    // Ignore optical photons as hits as set by DetType == opticalphoton
    if (! fDetectOpticalPhotons
        && particle == G4OpticalPhoton::OpticalPhotonDefinition()) {
      static bool has_been_warned = false;
      if (! has_been_warned) {
        G4cout << "remoll: Optical photons simulated but not stored for some detectors." << G4endl;
        G4cout << "remoll: To save optical photon hits, use the following in gdml:" << G4endl;
        G4cout << "remoll:   <auxiliary auxtype=\"DetType\" auxvalue=\"opticalphoton\"/>" << G4endl;
        G4cout << "remoll: or use the following in your macro:" << G4endl;
        G4cout << "remoll:   /remoll/SD/detect opticalphoton 4001" << G4endl;
        has_been_warned = true;
      }
      return false;
    }


    // Ignore neutral particles below 0.1 MeV (non optical photon) as set by DetType == lowenergyneutral
    G4double charge = particle->GetPDGCharge();
    if (! fDetectLowEnergyNeutrals
        && particle != G4OpticalPhoton::OpticalPhotonDefinition()
        && charge == 0.0 && track->GetKineticEnergy() < 0.1*CLHEP::MeV) {
      static bool has_been_warned = false;
      if (! has_been_warned) {
        G4cout << "remoll: <0.1 MeV neutrals simulated but not stored for some detectors." << G4endl;
        G4cout << "remoll: To save low energy neutral hits, use the following in gdml:" << G4endl;
        G4cout << "remoll:   <auxiliary auxtype=\"DetType\" auxvalue=\"lowenergyneutral\"/>" << G4endl;
        G4cout << "remoll: or use the following in your macro:" << G4endl;
        G4cout << "remoll:   /remoll/SD/detect lowenergyneutral 4001" << G4endl;
        has_been_warned = true;
      }
      return false;
    }

    // First step in volume?
    G4bool firststepinvolume = false;
    // if prepoint status is fGeomBoundary it's easy
    if (prepoint->GetStepStatus() == fGeomBoundary)
      firststepinvolume = true;
    // if prepoint status is fUndefined it could be because of optical photons
    else if (prepoint->GetStepStatus() == fUndefined) {
      // get track user information and cast in our own format
      G4VUserTrackInformation* usertrackinfo = track->GetUserInformation();
      remollUserTrackInformation* remollusertrackinfo =
          dynamic_cast<remollUserTrackInformation*>(usertrackinfo);
      if (remollusertrackinfo) {
        // if stored postpoint status is fGeomBoundary
        if (remollusertrackinfo->GetStepStatus() == fGeomBoundary)
          firststepinvolume = true;
      }
    }

    // Only detect hits that are on the incident boundary edge of the geometry in question
    // as set by DetType == boundaryhits
    if (fDetectBoundaryHits
        && ! firststepinvolume){
      static bool has_been_warned = false;
      if (! has_been_warned) {
        G4cout << "remoll: Only hits at the entry boundary are stored for some detectors." << G4endl;
        G4cout << "remoll: To save entry boundary hits alone, use the following in gdml:" << G4endl;
        G4cout << "remoll:   <auxiliary auxtype=\"DetType\" auxvalue=\"boundaryhits\"/>" << G4endl;
        G4cout << "remoll: or use the following in your macro:" << G4endl;
        G4cout << "remoll:   /remoll/SD/detect boundaryhits 4001" << G4endl;
        has_been_warned = true;
      }
      return false;
    }


    // Ignore interior tracks
    if (! fDetectSecondaries
        && track->GetCreatorProcess() != 0 && ! firststepinvolume) {
      static bool has_been_warned = false;
      if (! has_been_warned) {
        G4cout << "remoll: Only full hits from primary tracks are being stored by default." << G4endl;
        G4cout << "remoll: For secondaries only the first hit in the volume is stored." << G4endl;
        G4cout << "remoll: To save secondary track hits too, use the following in gdml:" << G4endl;
        G4cout << "remoll:   <auxiliary auxtype=\"DetType\" auxvalue=\"secondaries\"/>" << G4endl;
        G4cout << "remoll: or use the following in your macro:" << G4endl;
        G4cout << "remoll:   /remoll/SD/detect secondaries 4001" << G4endl;
        has_been_warned = true;
      }
      return false;
    }


    /////////////////////////////////////////////////////

    remollGenericDetectorHit* hit = new remollGenericDetectorHit(fDetNo, copyID);
    fHitColl->insert(hit);


    // Which point do we store? (important for optical photons at boundaries)
    G4StepPoint* point = 0;
    // optical absorption
    if (particle == G4OpticalPhoton::OpticalPhotonDefinition()
     && postpoint->GetStepStatus() == fGeomBoundary) {
      point = postpoint;
    // all other cases
    } else {
      point = prepoint;
    }


    // Positions
    G4ThreeVector global_position = point->GetPosition();
    G4ThreeVector local_position = point->GetTouchable()->GetHistory()->GetTopTransform().TransformPoint(global_position);
    hit->f3X  = global_position;
    hit->f3Xl = local_position;
    hit->f3V  = track->GetVertexPosition();
    
    G4ThreeVector global_momentum = track->GetMomentum();
    //just do the rotation without the translation
    hit->f3Pl = point->GetTouchable()->GetHistory()->GetTopTransform().TransformAxis(global_momentum);
    hit->f3P  = global_momentum;

    hit->f3S  = track->GetPolarization();

    hit->fTime = point->GetGlobalTime();

    hit->f3dP = track->GetMomentumDirection();

    hit->fP = track->GetMomentum().mag();
    hit->fE = track->GetTotalEnergy();
    hit->fM = particle->GetPDGMass();
    hit->fK = track->GetKineticEnergy();

    hit->fTrID  = track->GetTrackID();
    hit->fmTrID = track->GetParentID();
    hit->fPID   = particle->GetPDGEncoding();

    hit->fGen   = (long int) track->GetCreatorProcess();

    hit->fEdep  = step->GetTotalEnergyDeposit();

    return true;
}

void remollGenericDetector::EndOfEvent(G4HCofThisEvent* HCE)
{
    G4SDManager *sdman = G4SDManager::GetSDMpointer();

    if (fHCID < 0) { fHCID = sdman->GetCollectionID(collectionName[0]); }
    if (fSCID < 0) { fSCID = sdman->GetCollectionID(collectionName[1]); }

    HCE->AddHitsCollection(fHCID, fHitColl);
    HCE->AddHitsCollection(fSCID, fSumColl);
}


