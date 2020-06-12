#include "remollIO.hh"

#include <TFile.h>
#include <TTree.h>
#include <TClonesArray.h>

#include "G4ParticleDefinition.hh"
#include "G4GenericMessenger.hh"

#include "remollGenericDetectorHit.hh"
#include "remollGenericDetectorSum.hh"
#include "remollEvent.hh"
#include "remollRun.hh"
#include "remollRunData.hh"
#include "remollBeamTarget.hh"

#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/dom/DOMEntityReference.hpp>
#include <xercesc/dom/DOMEntity.hpp>
#include <xercesc/dom/DOMElement.hpp>
#include <xercesc/dom/DOMNamedNodeMap.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include <xercesc/dom/DOMNode.hpp>

ClassImp(remollSeed_t)

// Singleton
remollIO* remollIO::gInstance = 0;
remollIO* remollIO::GetInstance() {
  if (gInstance == 0) {
    gInstance = new remollIO();
  }
  return gInstance;
}

remollIO::remollIO()
: fFile(0),fTree(0),fFilename("remollout.root"),fRate(0)
{
    // Create generic messenger
    fMessenger = new G4GenericMessenger(this,"/remoll/","Remoll properties");
    fMessenger->DeclareProperty("filename",fFilename,"Output filename");
}

remollIO::~remollIO()
{
    // Delete tree
    if (fTree) {
        delete fTree;
        fTree = NULL;
    }
    // Delete file
    if (fFile) {
        delete fFile;
        fFile = NULL;
    }

    delete fMessenger;
}

void remollIO::InitializeTree()
{
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

//    fTree->SetMaxTreeSize(1900000000); // 1.9GB

    // Units
    fTree->Branch("units",    &fUnits);

    // Detectors
    fTree->Branch("dets.sd",  &fDetSDNos, fDetSDNames);
    fTree->Branch("dets.lv",  &fDetLVNos, fDetLVNames);

    // Event information
    fTree->Branch("seed",     &fSeed);
    fTree->Branch("rate",     &fRate,   "rate/D");
    fTree->Branch("ev",       &fEv);
    fTree->Branch("bm",       &fBm);
    fTree->Branch("part",     &fEvPart);

    // GenericDetectorHit
    fTree->Branch("hit",      &fGenDetHit);
    // GenericDetectorSum
    fTree->Branch("sum",      &fGenDetSum);

    G4cout << "Initialized tree." << G4endl;
}

void remollIO::FillTree()
{
    if( !fTree ){
        G4cerr << "Error " << __PRETTY_FUNCTION__ << ": "
            << __FILE__ <<  " line " << __LINE__
            << " - Trying to fill non-existent tree" << G4endl;
        return;
    }

    fTree->Fill();
    fTree->GetCurrentFile();
}

void remollIO::Flush()
{
    // Set individual structs to zero
    static remollEvent_t ev0 = { };
    fEv = ev0;
    static remollBeamTarget_t bm0 = { };
    fBm = bm0;

    // Set arrays to 0
    fEvPart.clear();
    fGenDetHit.clear();
    fGenDetSum.clear();
}

void remollIO::WriteTree()
{
    if (!fFile)
      return;

    if (!fFile->IsOpen()) {
        G4cerr << "ERROR: " << __FILE__ << " line " << __LINE__ << ": TFile not open" << G4endl;
        exit(1);
    }

    G4cout << "Writing output to " << fFile->GetName() << " ... ";

    fFile->cd();

    fTree->Write("T", TObject::kOverwrite);
    remollRun::GetRunData()->Write("run_data", TObject::kOverwrite);

    fTree->ResetBranchAddresses();
    delete fTree;
    fTree = NULL;

    fFile->Close();

    delete fFile;
    fFile = NULL;

    G4cout << "done" << G4endl;
}

///////////////////////////////////////////////////////////////////////////////
// Interfaces to output section ///////////////////////////////////////////////

// Event Data

void remollIO::SetEventData(const remollEvent *ev)
{
  if (! ev) return;

  fRate   = ev->fRate*s;

  // Event variables
  fEv     = ev->GetEventIO();
  // Primary particles
  fEvPart = ev->GetEventParticleIO();

  // Beam data
  const remollBeamTarget* bt = ev->GetBeamTarget();
  if (bt)
    fBm = bt->GetBeamTargetIO();
}

// GenericDetectorHit
void remollIO::AddGenericDetectorHit(remollGenericDetectorHit *hit)
{
    fGenDetHit.push_back(hit->GetGenericDetectorHitIO());
}

// GenericDetectorSum
void remollIO::AddGenericDetectorSum(remollGenericDetectorSum *hit){
    fGenDetSum.push_back(hit->GetGenericDetectorSumIO());
}

/*---------------------------------------------------------------------------------*/

void remollIO::GrabGDMLFiles(G4String fn)
{
    // Reset list
    fGDMLFileNames.clear();
    fXMLFileNames.clear();

    remollRunData *rundata = remollRun::GetRunData();

    SearchGDMLforFiles(fn);

    // Resolve relative path, turn daughter/../ into nothing
    for(unsigned int idx = 0; idx < fXMLFileNames.size(); idx++ ){
      size_t pos = std::string::npos;
      // as long as there is a ../ keep replacing
      while ((pos = fXMLFileNames[idx].find("/../")) != std::string::npos) {
        auto begin = fXMLFileNames[idx].begin();
        // find last / before /../, cut from character after that /
        size_t parent = fXMLFileNames[idx].find_last_of("/", pos - 1) + 1;
        // if not found, we have daughter/../file.xml, so cut from 0
        if (parent == std::string::npos) parent = 0;
        // cut until 4 characters after start of /../
        fXMLFileNames[idx].erase(begin + parent, begin + pos + 4);
      }
    }

    // Remove non-unique XML filenames
    std::sort(fXMLFileNames.begin(), fXMLFileNames.end());
    fXMLFileNames.erase(std::unique(fXMLFileNames.begin(), fXMLFileNames.end()), fXMLFileNames.end());

    // Store filename and copy content into buffers
    for(unsigned int idx = 0; idx < fGDMLFileNames.size(); idx++ ){
        G4cout << "Found GDML file " << fGDMLFileNames[idx] << G4endl;
        rundata->AddGDMLFile(fGDMLFileNames[idx]);
    }
    for(unsigned int idx = 0; idx < fXMLFileNames.size(); idx++ ){
        G4cout << "Found XML file " << fXMLFileNames[idx] << G4endl;
        rundata->AddGDMLFile(fXMLFileNames[idx]);
    }
}

void remollIO::SearchGDMLforFiles(G4String fn)
{
    /*!  Chase down files to be included by GDML.
     *   Mainly look for file tags and perform recursively */

    struct stat thisfile;

    int ret = stat(fn.data(), &thisfile);
    if( ret != 0 ){
        G4cerr << "ERROR opening file " << fn <<  " in " << __PRETTY_FUNCTION__ << ": " << strerror(errno) << G4endl;
        exit(1);
    }

    fGDMLFileNames.push_back(fn);


    xercesc::XMLPlatformUtils::Initialize();

    xercesc::XercesDOMParser *xmlParser = new xercesc::XercesDOMParser();
    xmlParser->parse(fn.data());

    // Get document
    xercesc::DOMDocument* xmlDoc = xmlParser->getDocument();
    const XMLCh* docURI = xmlDoc->getDocumentURI();
    char* cstr_docURI = xercesc::XMLString::transcode(docURI);
    G4String str_docURI(cstr_docURI);
    xercesc::XMLString::release(&cstr_docURI);
    // remove file:// at begin of URI
    str_docURI.erase(str_docURI.begin(), str_docURI.begin() + str_docURI.find_first_of(':') + 3);
    // remove filename at end
    str_docURI.erase(str_docURI.find_last_of('/') + 1);
    // remove cwd at begin
    char cwd[MAXPATHLEN];
    if (getcwd(cwd,MAXPATHLEN) && str_docURI.find(cwd) == 0) {
      str_docURI.erase(0, strlen(cwd) + 1);
    }

    // Get doctype and entities
    xercesc::DOMDocumentType* xmlDocType = xmlDoc->getDoctype();
    if (xmlDocType) {
        xercesc::DOMNamedNodeMap* xmlNamedNodeMap = xmlDocType->getEntities();
        if (xmlNamedNodeMap) {
            for (XMLSize_t xx = 0; xx < xmlNamedNodeMap->getLength(); ++xx) {
                xercesc::DOMNode* currentNode = xmlNamedNodeMap->item(xx);
                if (currentNode->getNodeType() == xercesc::DOMNode::ENTITY_NODE) { // is entity
                    xercesc::DOMEntity* currentEntity
                      = dynamic_cast< xercesc::DOMEntity* >( currentNode );
                    const XMLCh* systemId = currentEntity->getSystemId();
                    char* cstr_systemId = xercesc::XMLString::transcode(systemId);
                    G4String str_systemId(cstr_systemId);
                    xercesc::XMLString::release(&cstr_systemId);
                    // Add this xml file to list to save
                    fXMLFileNames.push_back(str_docURI + str_systemId);
                }
            }
        }
    }

    // Get root element
    xercesc::DOMElement* elementRoot = xmlDoc->getDocumentElement();

    TraverseChildren( elementRoot );

    xercesc::XMLPlatformUtils::Terminate();

    delete xmlParser;
}

void remollIO::TraverseChildren( xercesc::DOMElement *thisel )
{
    xercesc::DOMNodeList* children = thisel->getChildNodes();
    const XMLSize_t nodeCount = children->getLength();

    for( XMLSize_t xx = 0; xx < nodeCount; ++xx ){
        xercesc::DOMNode* currentNode = children->item(xx);
        if( currentNode->getNodeType() ){   // true is not NULL

            if (currentNode->getNodeType() == xercesc::DOMNode::ELEMENT_NODE) { // is element
                xercesc::DOMElement* currentElement
                  = dynamic_cast< xercesc::DOMElement* >( currentNode );
                // transcode
                XMLCh* str_file = xercesc::XMLString::transcode("file");
                if( xercesc::XMLString::equals(currentElement->getTagName(), str_file)){
                    // transcode
                    XMLCh* str_name = xercesc::XMLString::transcode("name");
                    const XMLCh* attr = currentElement->getAttribute(str_name);
                    char* str_attr = xercesc::XMLString::transcode(attr);
                    // search files
                    SearchGDMLforFiles(G4String(str_attr));
                    // release
                    xercesc::XMLString::release(&str_name);
                    xercesc::XMLString::release(&str_attr);
                }
                // release
                xercesc::XMLString::release(&str_file);

                if( currentElement->getChildNodes()->getLength() > 0 ){
                    TraverseChildren( currentElement );
                }
            }
        }
    }
}
