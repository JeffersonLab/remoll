#include "remollGenericDetector.hh"
#include "G4SDManager.hh"

remollGenericDetector::remollGenericDetector( G4String name ) : G4VSensitiveDetector(name){
    collectionName.insert("genhit");
    collectionName.insert("gensum");

    fHCID = -1;
    fSCID = -1;
}

remollGenericDetector::~remollGenericDetector(){
}

void remollGenericDetector::Initialize(G4HCofThisEvent *){
    fHitColl = new remollGenericDetectorHitsCollection( SensitiveDetectorName,collectionName[0] );
    fSumColl = new remollGenericDetectorSumCollection ( SensitiveDetectorName,collectionName[1] );

    fSumMap.clear();
}

///////////////////////////////////////////////////////////////////////

G4bool remollGenericDetector::ProcessHits( G4Step *step, G4TouchableHistory *hist ){

    //  Make pointer to new hit
    remollGenericDetectorHit *thishit = new remollGenericDetectorHit();
    fHitColl->insert( thishit );

    //  Get pointer to our sum  /////////////////////////
    remollGenericDetectorSum *thissum = NULL;
    G4int  copyID = hist->GetReplicaNumber();

    if( !fSumMap.count(copyID) ){
	thissum = new remollGenericDetectorSum();
	fSumMap[copyID] = thissum;
	fSumColl->insert( thissum );
    } else {
	thissum = fSumMap[copyID];
    }
    /////////////////////////////////////////////////////

    /* FIXME

       // Do the actual data grabbing
     
     */

    G4double edep = step->GetTotalEnergyDeposit();


    return true;
}

///////////////////////////////////////////////////////////////////////

void remollGenericDetector::EndOfEvent(G4HCofThisEvent*HCE) {
    G4SDManager *sdman = G4SDManager::GetSDMpointer();

    if(fHCID<0) { 
	fHCID = sdman->GetCollectionID(collectionName[0]); 
    }
    if(fSCID<0) { 
	fSCID = sdman->GetCollectionID(collectionName[1]); 
    }

    HCE->AddHitsCollection( fHCID, fHitColl );
    HCE->AddHitsCollection( fSCID, fSumColl );

    return;
}


