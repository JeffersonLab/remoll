#include "remollIO.hh"

#include <TFile.h>
#include <TTree.h>
#include <TClonesArray.h>

#include "G4ParticleDefinition.hh"

#include "remollGenericDetectorHit.hh"
#include "remollGenericDetectorSum.hh"
#include "remollEvent.hh"
#include "remollRun.hh"
#include "remollRunData.hh"
#include "remollBeamTarget.hh"

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/dom/DOMElement.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include <xercesc/dom/DOMNode.hpp>


remollIO::remollIO(){
    fTree = NULL;
    fFile = NULL;

    // Default filename
    strcpy(fFilename, "remollout.root");

    InitializeTree();
}

remollIO::~remollIO(){
    if (fTree) {
	delete fTree;
	fTree = NULL;
    }
    if (fFile) {
	delete fFile;
	fFile = NULL;
    }
}

void remollIO::SetFilename(G4String fn){
    G4cout << "Setting output file to " << fn << G4endl;
    strcpy(fFilename, fn.data());
}

void remollIO::InitializeTree(){
    if (fFile) {
	fFile->Close();
	delete fFile;
	fFile = NULL;
	fTree = NULL;
    }

    if (fTree) {
	delete fTree;
	fTree = NULL;
    }

    fFile = new TFile(fFilename, "RECREATE");

    fTree = new TTree("T", "Geant4 Moller Simulation");

    fTree->SetMaxTreeSize(1900000000); // 1.9GB

    // Units
    fTree->Branch("units",    &fUnits);

    // Event information
    fTree->Branch("rate",     &fEvRate,   "rate/D");
    fTree->Branch("ev",       &fEv);
    fTree->Branch("bm",       &fBm);
    fTree->Branch("part",     &fEvPart);

    // GenericDetectorHit
    fTree->Branch("hit",      &fGenDetHit);
    // GenericDetectorSum
    fTree->Branch("sum",      &fGenDetSum);

    // Cut variables derived from hit information
    fTree->Branch("colCut",    &fCollCut,     "colCut/I");
}

void remollIO::FillTree(){
    if( !fTree ){ 
	fprintf(stderr, "Error %s: %s line %d - Trying to fill non-existant tree\n", __PRETTY_FUNCTION__, __FILE__, __LINE__ );
	return; 
    }

    fTree->Fill();
    fTree->GetCurrentFile();
}

void remollIO::Flush(){
    // Set individual structs to zero
    static remollEvent_t ev0 = { };
    fEv = ev0;
    static remollBeamTarget_t bm0 = { };
    fBm = bm0;

    // Set arrays to 0
    fEvPart.clear();
    fGenDetHit.clear();
    fGenDetSum.clear();

    fCollCut = 1; // default
}

void remollIO::WriteTree(){
    assert( fFile );
    assert( fTree );

    if( !fFile->IsOpen() ){
	G4cerr << "ERROR: " << __FILE__ << " line " << __LINE__ << ": TFile not open" << G4endl;
	exit(1);
    }

    G4cout << "Writing output to " << fFile->GetName() << "... ";

    fFile->cd();

    fTree->Write("T", TObject::kOverwrite);
    remollRun::GetRun()->GetData()->Write("run_data", TObject::kOverwrite); 

    fTree->ResetBranchAddresses();
    delete fTree;
    fTree = NULL;

    fFile->Close();

    delete fFile;
    fFile = NULL;

    G4cout << "written" << G4endl;

    return;
}

///////////////////////////////////////////////////////////////////////////////
// Interfaces to output section ///////////////////////////////////////////////

// Event Data

void remollIO::SetEventData(remollEvent *ev){
    fEvRate   = ev->fRate*s;

    // Event variables
    fEv     = ev->GetEventIO();
    // Primary particles
    fEvPart = ev->GetEventParticleIO();

    // Beam data
    remollBeamTarget *bt = remollBeamTarget::GetBeamTarget();
    fBm = bt->GetBeamTargetIO();
}

// GenericDetectorHit
void remollIO::AddGenericDetectorHit(remollGenericDetectorHit *hit){
    fGenDetHit.push_back(hit->GetGenericDetectorHitIO());

    // for collimator cut
    if( (hit->fDetID==200 && hit->f3X.perp()/__L_UNIT < 0.03) || 
      	(hit->fDetID==201 && hit->f3X.perp()/__L_UNIT < 0.05) )
      fCollCut=0;
}

// GenericDetectorSum
void remollIO::AddGenericDetectorSum(remollGenericDetectorSum *hit){
    fGenDetSum.push_back(hit->GetGenericDetectorSumIO());
}

/*---------------------------------------------------------------------------------*/

void remollIO::GrabGDMLFiles(G4String fn){
    // Reset list
    fGDMLFileNames.clear();

    remollRunData *rundata = remollRun::GetRun()->GetData();
    rundata->ClearGDMLFiles();

    xercesc::XMLPlatformUtils::Initialize();
    SearchGDMLforFiles(fn);
    xercesc::XMLPlatformUtils::Terminate();


    // Store filename

    unsigned int idx;

    // Copy into buffers
    for( idx = 0; idx < fGDMLFileNames.size(); idx++ ){
	G4cout << "Found GDML file " << fGDMLFileNames[idx] << G4endl;
	rundata->AddGDMLFile(fGDMLFileNames[idx]);
    }

    return;
}

void remollIO::SearchGDMLforFiles(G4String fn){
    /*!  Chase down files to be included by GDML.
     *   Mainly look for file tags and perform recursively */

    struct stat thisfile;

    int ret = stat(fn.data(), &thisfile);

    if( ret != 0 ){
	G4cerr << "ERROR opening file " << fn <<  " in " << __PRETTY_FUNCTION__ << ": " << strerror(errno) << G4endl;
	exit(1);
    }

   xercesc::XercesDOMParser *xmlParser = new xercesc::XercesDOMParser();

   // Make sure file exists - otherwise freak out

   fGDMLFileNames.push_back(fn.data());

   xmlParser->parse( fn.data() );
   xercesc::DOMDocument* xmlDoc = xmlParser->getDocument();

   xercesc::DOMElement* elementRoot = xmlDoc->getDocumentElement();

   TraverseChildren( elementRoot );
   return;
}

void remollIO::TraverseChildren( xercesc::DOMElement *thisel ){

   xercesc::DOMNodeList*      children = thisel->getChildNodes();
   const XMLSize_t nodeCount = children->getLength();

   for( XMLSize_t xx = 0; xx < nodeCount; ++xx ){
       xercesc::DOMNode* currentNode = children->item(xx);
       if( currentNode->getNodeType() ){   // true is not NULL

	   if( currentNode->getNodeType() == xercesc::DOMNode::ELEMENT_NODE ){ // is element 
	       xercesc::DOMElement* currentElement
		   = dynamic_cast< xercesc::DOMElement* >( currentNode );
	       if( xercesc::XMLString::equals(currentElement->getTagName(), xercesc::XMLString::transcode("file"))){
		   SearchGDMLforFiles(G4String(xercesc::XMLString::transcode(currentElement->getAttribute(xercesc::XMLString::transcode("name")))));
	       }

	       if( currentElement->getChildNodes()->getLength() > 0 ){
		   TraverseChildren( currentElement );
	       }
	   }
       }
   }

}









