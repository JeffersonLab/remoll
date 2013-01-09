#include "remollIO.hh"

#include <TFile.h>
#include <TTree.h>
#include <TClonesArray.h>

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

    /* FIXME
     * set branches for output
    */

    return;
}

void remollIO::FillTree(){
    if( !fTree ){ 
	fprintf(stderr, "Error %s: %s line %d - Trying to fill non-existant tree\n", __PRETTY_FUNCTION__, __FILE__, __LINE__ );
	return; 
    }

    fTree->Fill();
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




