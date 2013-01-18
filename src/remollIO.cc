#include "remollIO.hh"

#include <TFile.h>
#include <TTree.h>
#include <TClonesArray.h>

#include "G4ParticleDefinition.hh"

#include "remollGenericDetectorHit.hh"
#include "remollGenericDetectorSum.hh"
#include "remollEvent.hh"

remollIO::remollIO(){
    fTree = NULL;
    InitializeTree();
    // Default filename
    strcpy(fFilename, "remollout.root");
    fFile = NULL;

}

remollIO::~remollIO(){
    delete fTree;
    fTree = NULL;
}


void remollIO::InitializeTree(){
    if( fTree ){ delete fTree; }

    fTree = new TTree("T", "Geant4 Moller Simulation");

    // Event information
    fTree->Branch("ev.rate",  &fEvRate,   "ev.rate/D");
    fTree->Branch("ev.A",     &fEvAsym,   "ev.A/D");
    fTree->Branch("ev.Am",    &fEvmAsym,  "ev.Am/D");
    fTree->Branch("ev.xs",    &fEvEffXS,  "ev.xs/D");

    fTree->Branch("ev.npart", &fNEvPart   ,     "ev.npart/I");
    fTree->Branch("ev.pid",   &fEvPID,      "ev.pid[ev.npart]/I");
    fTree->Branch("ev.vx",    &fEvPart_X,   "ev.vx[ev.npart]/D");
    fTree->Branch("ev.vy",    &fEvPart_Y,   "ev.vy[ev.npart]/D");
    fTree->Branch("ev.vz",    &fEvPart_Z,   "ev.vz[ev.npart]/D");
    fTree->Branch("ev.px",    &fEvPart_Px,  "ev.px[ev.npart]/D");
    fTree->Branch("ev.py",    &fEvPart_Py,  "ev.py[ev.npart]/D");
    fTree->Branch("ev.pz",    &fEvPart_Pz,  "ev.pz[ev.npart]/D");
    fTree->Branch("ev.tpx",    &fEvPart_tPx,  "ev.tpx[ev.npart]/D");
    fTree->Branch("ev.tpy",    &fEvPart_tPy,  "ev.tpy[ev.npart]/D");
    fTree->Branch("ev.tpz",    &fEvPart_tPz,  "ev.tpz[ev.npart]/D");

    // GenericDetectorHit
    fTree->Branch("hit.n",    &fNGenDetHit,     "hit.n/I");
    fTree->Branch("hit.det",  &fGenDetHit_det,  "hit.det[hit.n]/I");
    fTree->Branch("hit.vid",  &fGenDetHit_id,   "hit.vid[hit.n]/I");

    fTree->Branch("hit.pid",  &fGenDetHit_pid,   "hit.pid[hit.n]/I");
    fTree->Branch("hit.trid", &fGenDetHit_trid,  "hit.trid[hit.n]/I");
    fTree->Branch("hit.mtrid",&fGenDetHit_mtrid, "hit.mtrid[hit.n]/I");
    fTree->Branch("hit.gen",  &fGenDetHit_gen,   "hit.gen[hit.n]/I");

    fTree->Branch("hit.x",    &fGenDetHit_X,   "hit.x[hit.n]/D");
    fTree->Branch("hit.y",    &fGenDetHit_Y,   "hit.y[hit.n]/D");
    fTree->Branch("hit.z",    &fGenDetHit_Z,   "hit.z[hit.n]/D");

    fTree->Branch("hit.px",   &fGenDetHit_Px,   "hit.px[hit.n]/D");
    fTree->Branch("hit.py",   &fGenDetHit_Py,   "hit.py[hit.n]/D");
    fTree->Branch("hit.pz",   &fGenDetHit_Pz,   "hit.pz[hit.n]/D");

    fTree->Branch("hit.vx",   &fGenDetHit_Vx,   "hit.vx[hit.n]/D");
    fTree->Branch("hit.vy",   &fGenDetHit_Vy,   "hit.vy[hit.n]/D");
    fTree->Branch("hit.vz",   &fGenDetHit_Vz,   "hit.vz[hit.n]/D");

    fTree->Branch("hit.p",    &fGenDetHit_P,   "hit.p[hit.n]/D");
    fTree->Branch("hit.e",    &fGenDetHit_E,   "hit.e[hit.n]/D");
    fTree->Branch("hit.m",    &fGenDetHit_M,   "hit.m[hit.n]/D");
    
    // GenericDetectorSum
    fTree->Branch("sum.n",    &fNGenDetSum,     "sum.n/I");
    fTree->Branch("sum.det",  &fGenDetSum_det,  "sum.det[sum.n]/I");
    fTree->Branch("sum.vid",  &fGenDetSum_id,   "sum.vid[sum.n]/I");
    fTree->Branch("sum.edep", &fGenDetSum_edep, "sum.edep[sum.n]/D");

    return;
}

void remollIO::FillTree(){
    if( !fTree ){ 
	fprintf(stderr, "Error %s: %s line %d - Trying to fill non-existant tree\n", __PRETTY_FUNCTION__, __FILE__, __LINE__ );
	return; 
    }

    fTree->Fill();
}

void remollIO::Flush(){
    //  Set arrays to 0
    fNGenDetHit = 0;
    fNGenDetSum = 0;
}

void remollIO::WriteTree(){
    fFile = new TFile(fFilename, "RECREATE");
    fFile->cd();
    fTree->Write("T", TObject::kOverwrite);
    fFile->Close();
    delete fFile;
    fFile = NULL;

    return;
}

///////////////////////////////////////////////////////////////////////////////
// Interfaces to output section ///////////////////////////////////////////////

// Event Data

void remollIO::SetEventData(remollEvent *ev){
    int n = ev->fPartType.size();
    if( n > __IO_MAXHIT ){
	G4cerr << "WARNING: " << __PRETTY_FUNCTION__ << " line " << __LINE__ << ":  Buffer size exceeded!" << G4endl;
	return;
    }

    fNEvPart = n;

    fEvRate   = ev->fRate;
    fEvEffXS  = ev->fEffXs/nanobarn;
    fEvAsym   = ev->fAsym/__ASYMM_SCALE;
    fEvmAsym  = ev->fmAsym/__ASYMM_SCALE;

    int idx;
    for( idx = 0; idx < n; idx++ ){
	fEvPID[idx] = ev->fPartType[idx]->GetPDGEncoding();

	fEvPart_X[idx] = ev->fPartPos[idx].x()/__L_UNIT;
	fEvPart_Y[idx] = ev->fPartPos[idx].y()/__L_UNIT;
	fEvPart_Z[idx] = ev->fPartPos[idx].z()/__L_UNIT;

	fEvPart_Px[idx] = ev->fPartRealMom[idx].x()/__E_UNIT;
	fEvPart_Py[idx] = ev->fPartRealMom[idx].y()/__E_UNIT;
	fEvPart_Pz[idx] = ev->fPartRealMom[idx].z()/__E_UNIT;

	fEvPart_tPx[idx] = ev->fPartMom[idx].x()/__E_UNIT;
	fEvPart_tPy[idx] = ev->fPartMom[idx].y()/__E_UNIT;
	fEvPart_tPz[idx] = ev->fPartMom[idx].z()/__E_UNIT;
    }

    return;
}

// GenericDetectorHit

void remollIO::AddGenericDetectorHit(remollGenericDetectorHit *hit){
    int n = fNGenDetHit;
    if( n > __IO_MAXHIT ){
	G4cerr << "WARNING: " << __PRETTY_FUNCTION__ << " line " << __LINE__ << ":  Buffer size exceeded!" << G4endl;
	return;
    }

    fGenDetHit_det[n]  = hit->fDetID;
    fGenDetHit_id[n]   = hit->fCopyID;

    fGenDetHit_trid[n] = hit->fTrID;
    fGenDetHit_mtrid[n]= hit->fmTrID;
    fGenDetHit_pid[n]  = hit->fPID;
    fGenDetHit_gen[n]  = hit->fGen;

    fGenDetHit_X[n]  = hit->f3X.x()/__L_UNIT;
    fGenDetHit_Y[n]  = hit->f3X.y()/__L_UNIT;
    fGenDetHit_Z[n]  = hit->f3X.z()/__L_UNIT;

    fGenDetHit_Px[n]  = hit->f3P.x()/__E_UNIT;
    fGenDetHit_Py[n]  = hit->f3P.y()/__E_UNIT;
    fGenDetHit_Pz[n]  = hit->f3P.z()/__E_UNIT;

    fGenDetHit_Vx[n]  = hit->f3V.x()/__L_UNIT;
    fGenDetHit_Vy[n]  = hit->f3V.y()/__L_UNIT;
    fGenDetHit_Vz[n]  = hit->f3V.z()/__L_UNIT;

    fGenDetHit_P[n]  = hit->fP/__E_UNIT;
    fGenDetHit_E[n]  = hit->fE/__E_UNIT;
    fGenDetHit_M[n]  = hit->fM/__E_UNIT;

    fNGenDetHit++;
}


// GenericDetectorSum

void remollIO::AddGenericDetectorSum(remollGenericDetectorSum *hit){
    int n = fNGenDetSum;
    if( n > __IO_MAXHIT ){
	G4cerr << "WARNING: " << __PRETTY_FUNCTION__ << " line " << __LINE__ << ":  Buffer size exceeded!" << G4endl;
	return;
    }

    fGenDetSum_edep[n] = hit->fEdep/__E_UNIT;
    fGenDetSum_det[n]  = hit->fDetID;
    fGenDetSum_id[n]   = hit->fCopyID;

    fNGenDetSum++;
}











