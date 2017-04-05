#include "remollGenericDetector.hh"
#include "G4SDManager.hh"
#include "G4ThreeVector.hh"

#include "remollEvent.hh"
#include "remollGenericDetectorHit.hh"

#include <unordered_map>

remollGenericDetector::remollGenericDetector( G4String name, G4int detnum ) : G4VSensitiveDetector(name){
    char colname[255];

    fDetNo = detnum;
    assert( fDetNo > 0 );

//    fTrackSecondaries = false;
    fTrackSecondaries = true;

    sprintf(colname, "genhit_%d", detnum);
    collectionName.insert(G4String(colname));

    sprintf(colname, "gensum_%d", detnum);
    collectionName.insert(G4String(colname));

    fHCID = -1;
    fSCID = -1;
}

remollGenericDetector::~remollGenericDetector(){
}

void remollGenericDetector::Initialize(G4HCofThisEvent *){

    fHitColl = new remollGenericDetectorHitsCollection( SensitiveDetectorName, collectionName[0] );
    fSumColl = new remollGenericDetectorSumCollection ( SensitiveDetectorName, collectionName[1] );

    fSumMap.clear();
}

///////////////////////////////////////////////////////////////////////

G4bool remollGenericDetector::ProcessHits( G4Step *step, G4TouchableHistory *){
    G4bool badedep = false;
    G4bool badhit  = false;

    // Get touchable volume info
    G4TouchableHistory *hist = 
	      (G4TouchableHistory*)(step->GetPreStepPoint()->GetTouchable());
    //G4int  copyID = hist->GetVolume(1)->GetCopyNo();//return the copy id of the parent volume
    G4int  copyID = hist->GetVolume()->GetCopyNo();//return the copy id of the logical volume

    G4StepPoint *prestep = step->GetPreStepPoint();
    G4Track     *track   = step->GetTrack();

    G4double edep = step->GetTotalEnergyDeposit();

    // We're just going to record primary particles and things
    // that have just entered our boundary
    //the following condition ensure that not all the hits are recorded. This will reflect in the energy deposit sum from the hits compared to the energy deposit from the hit sum detectors.
    badhit = true;
    if( track->GetCreatorProcess() == 0 ||
	              (prestep->GetStepStatus() == fGeomBoundary && fTrackSecondaries)){
        badhit = false;
    }
    //badhit = false;

    //  Make pointer to new hit if it's a valid track
    remollGenericDetectorHit *thishit;
    if( !badhit ){
        thishit = new remollGenericDetectorHit(fDetNo, copyID);
        fHitColl->insert( thishit );
    } 

    //  Get pointer to our sum  /////////////////////////
    remollGenericDetectorSum *thissum = NULL;

    if( !fSumMap.count(copyID) ){
	      if( edep > 0.0 ){
            thissum = new remollGenericDetectorSum(fDetNo, copyID);
            fSumMap[copyID] = thissum;
            fSumColl->insert( thissum );
        } else {
            badedep = true;
	      }
    } else {
	      thissum = fSumMap[copyID];
    } 
    /////////////////////////////////////////////////////

    // Do the actual data grabbing

    if( !badedep ){
	      // This is all we need to do for the sum
        thissum->AddEDep( track->GetDefinition()->GetPDGEncoding(), prestep->GetPosition(), edep );
    }

    if( !badhit ){
	      // Hit
	      thishit->f3X = prestep->GetPosition();
	      thishit->f3V = track->GetVertexPosition();
	      thishit->f3P = track->GetMomentum();

        // FIXME Plan:
        // request the current event to harvest its data, if the event is dead (as I suspect it may be) 
        // then I should use the backup plan, which is to put these variables back into the 
        // remollSteppingAction and hope that they persist long enough to be used here too.
        remollEvent *event = remollEvent::GetRemollEvent(); // NEW

        G4int ID = track->GetTrackID();
        // Declare an iterator to the unordered_map (fPartDeltaEMap or whatever)
        std::unordered_map<G4int, G4double>::iterator ite;
        // Find if an element with key "id" exists or not.
        // find() returns an iterator
        ite = event->fPartDeltaEMap.find(ID);

        // Check if the iterator points to the end of the map (meaning that it didn't find the key, i.e. the particle never registered a significant vertex at all)
        if ( ite == event->fPartDeltaEMap.end() ){ // Then add a null result signifier
            thishit->fLastPos = track->GetVertexPosition(); // NEW
            thishit->fDeltaE  = 0.0; // NEW
            thishit->fDeltaTh = 0.0; // NEW
        }
        else { // Then add apply the actual values
            thishit->fLastPos = event->fPartLastPosMap[ID]; // NEW
            thishit->fDeltaE  = event->fPartDeltaEMap[ID];  // NEW
            thishit->fDeltaTh = event->fPartDeltaThMap[ID]; // NEW
        }

	      thishit->fP = track->GetMomentum().mag();
	      thishit->fE = track->GetTotalEnergy();
	      thishit->fM = track->GetDefinition()->GetPDGMass();

	      thishit->fTrID  = track->GetTrackID();
	      thishit->fmTrID = track->GetParentID();
	      thishit->fPID   = track->GetDefinition()->GetPDGEncoding();
	      thishit->fEdep  = edep; 
	      // FIXME - Enumerate encodings
	      thishit->fGen   = (long int) track->GetCreatorProcess();
    }

    return !badedep && !badhit;
}

///////////////////////////////////////////////////////////////////////

void remollGenericDetector::EndOfEvent(G4HCofThisEvent*HCE) {
    G4SDManager *sdman = G4SDManager::GetSDMpointer();

    if(fHCID<0){ fHCID = sdman->GetCollectionID(collectionName[0]); }
    if(fSCID<0){ fSCID = sdman->GetCollectionID(collectionName[1]); }

    HCE->AddHitsCollection( fHCID, fHitColl );
    HCE->AddHitsCollection( fSCID, fSumColl );

    return;
}


