#include <sstream>
#include <string>
#include <iostream>

#include "TTree.h"
#include "TFile.h"
#include "remolltypes.hh"
#include "petypes.hh"
#include <vector> 


#define pi 3.141592653589793238462643383279502884L

catPEs_t catPEsTrim(std::vector<int> detids, size_t peLen, std::vector<double> catHitx, std::vector<double> catHity, std::vector<double> catHitz)
{
    catPEs_t newHit;
    newHit.nPEs=peLen;
    newHit.detIDs=detids;
    newHit.catHitx=catHitx;
    newHit.catHity=catHity;
    newHit.catHitz=catHitz;
    return newHit; 
}

Q_t QTrim(remollGenericDetectorHit_t hit, size_t peLen, std::vector<double> catHitx, std::vector<double> catHity, std::vector<double> catHitz)
{
    Q_t newHit;
    newHit.det = hit.det;
    newHit.x=hit.x;
    newHit.y=hit.y;
    newHit.z=hit.z;
    newHit.r=hit.r;
    newHit.px=hit.px;
    newHit.py=hit.py;
    newHit.pz=hit.pz;
    newHit.p=hit.p;
    newHit.e=hit.e;
    newHit.m=hit.m;
    newHit.vx=hit.vx;
    newHit.vy=hit.vy;
    newHit.vz=hit.vz;
    newHit.nPEs=peLen;
    newHit.catHitx=catHitx;
    newHit.catHity=catHity;
    newHit.catHitz=catHitz;
    return newHit; 
}

Ref_t RefTrim(remollGenericDetectorHit_t hit, size_t peLen, std::vector<double> catHitx, std::vector<double> catHity, std::vector<double> catHitz)
{
    Ref_t newHit;
    newHit.det = hit.det;
    newHit.x=hit.x;
    newHit.y=hit.y;
    newHit.z=hit.z;
    newHit.r=hit.r;
    newHit.px=hit.px;
    newHit.py=hit.py;
    newHit.pz=hit.pz;
    newHit.p=hit.p;
    newHit.e=hit.e;
    newHit.m=hit.m;
    newHit.vx=hit.vx;
    newHit.vy=hit.vy;
    newHit.vz=hit.vz;
    newHit.nPEs=peLen;
    newHit.catHitx=catHitx;
    newHit.catHity=catHity;
    newHit.catHitz=catHitz;
    return newHit; 
}

RefX_t RefXTrim(remollGenericDetectorHit_t hit, size_t peLen, std::vector<double> catHitx, std::vector<double> catHity, std::vector<double> catHitz)
{
    RefX_t newHit;
    newHit.det = hit.det;
    newHit.x=hit.x;
    newHit.y=hit.y;
    newHit.z=hit.z;
    newHit.r=hit.r;
    newHit.px=hit.px;
    newHit.py=hit.py;
    newHit.pz=hit.pz;
    newHit.p=hit.p;
    newHit.e=hit.e;
    newHit.m=hit.m;
    newHit.vx=hit.vx;
    newHit.vy=hit.vy;
    newHit.vz=hit.vz;
    newHit.nPEs=peLen;
    newHit.catHitx=catHitx;
    newHit.catHity=catHity;
    newHit.catHitz=catHitz;
    return newHit; 
}

Refair_t RefairTrim(remollGenericDetectorHit_t hit, size_t peLen, std::vector<double> catHitx, std::vector<double> catHity, std::vector<double> catHitz)
{
    Refair_t newHit;
    newHit.det = hit.det;
    newHit.x=hit.x;
    newHit.y=hit.y;
    newHit.z=hit.z;
    newHit.r=hit.r;
    newHit.px=hit.px;
    newHit.py=hit.py;
    newHit.pz=hit.pz;
    newHit.p=hit.p;
    newHit.e=hit.e;
    newHit.m=hit.m;
    newHit.vx=hit.vx;
    newHit.vy=hit.vy;
    newHit.vz=hit.vz;
    newHit.nPEs=peLen;
    newHit.catHitx=catHitx;
    newHit.catHity=catHity;
    newHit.catHitz=catHitz;
    return newHit; 
}

LG_t LGTrim(remollGenericDetectorHit_t hit, size_t peLen, std::vector<double> catHitx, std::vector<double> catHity, std::vector<double> catHitz)
{
    LG_t newHit;
    newHit.det = hit.det;
    newHit.x=hit.x;
    newHit.y=hit.y;
    newHit.z=hit.z;
    newHit.r=hit.r;
    newHit.px=hit.px;
    newHit.py=hit.py;
    newHit.pz=hit.pz;
    newHit.p=hit.p;
    newHit.e=hit.e;
    newHit.m=hit.m;
    newHit.vx=hit.vx;
    newHit.vy=hit.vy;
    newHit.vz=hit.vz;
    newHit.nPEs=peLen;
    newHit.catHitx=catHitx;
    newHit.catHity=catHity;
    newHit.catHitz=catHitz;
    return newHit; 
}

LGair_t LGairTrim(remollGenericDetectorHit_t hit, size_t peLen, std::vector<double> catHitx, std::vector<double> catHity, std::vector<double> catHitz)
{
    LGair_t newHit;
    newHit.det = hit.det;
    newHit.x=hit.x;
    newHit.y=hit.y;
    newHit.z=hit.z;
    newHit.r=hit.r;
    newHit.px=hit.px;
    newHit.py=hit.py;
    newHit.pz=hit.pz;
    newHit.p=hit.p;
    newHit.e=hit.e;
    newHit.m=hit.m;
    newHit.vx=hit.vx;
    newHit.vy=hit.vy;
    newHit.vz=hit.vz;
    newHit.nPEs=peLen;
    newHit.catHitx=catHitx;
    newHit.catHity=catHity;
    newHit.catHitz=catHitz;
    return newHit; 
}

PMTcat_t PMTcatTrim(remollGenericDetectorHit_t hit, size_t peLen, std::vector<double> catHitx, std::vector<double> catHity, std::vector<double> catHitz)
{
    PMTcat_t newHit;
    newHit.det = hit.det;
    newHit.x=hit.x;
    newHit.y=hit.y;
    newHit.z=hit.z;
    newHit.r=hit.r;
    newHit.px=hit.px;
    newHit.py=hit.py;
    newHit.pz=hit.pz;
    newHit.p=hit.p;
    newHit.e=hit.e;
    newHit.m=hit.m;
    newHit.vx=hit.vx;
    newHit.vy=hit.vy;
    newHit.vz=hit.vz;
    newHit.nPEs=peLen;
    newHit.catHitx=catHitx;
    newHit.catHity=catHity;
    newHit.catHitz=catHitz;
    return newHit; 
}

PMTbulk_t PMTbulkTrim(remollGenericDetectorHit_t hit, size_t peLen, std::vector<double> catHitx, std::vector<double> catHity, std::vector<double> catHitz)
{
    PMTbulk_t newHit;
    newHit.det = hit.det;
    newHit.x=hit.x;
    newHit.y=hit.y;
    newHit.z=hit.z;
    newHit.r=hit.r;
    newHit.px=hit.px;
    newHit.py=hit.py;
    newHit.pz=hit.pz;
    newHit.p=hit.p;
    newHit.e=hit.e;
    newHit.m=hit.m;
    newHit.vx=hit.vx;
    newHit.vy=hit.vy;
    newHit.vz=hit.vz;
    newHit.nPEs=peLen;
    newHit.catHitx=catHitx;
    newHit.catHity=catHity;
    newHit.catHitz=catHitz;
    return newHit; 
}

elseX_t elseXTrim(int peLen, std::vector<double> catHitx, std::vector<double> catHity, std::vector<double> catHitz)
{
    elseX_t newHit;
    newHit.nPEs=peLen;
    newHit.catHitx=catHitx;
    newHit.catHity=catHity;
    newHit.catHitz=catHitz;
    return newHit; 
}

void pe(std::string file="tracking.root", int detid=50001)
{
    TTree::SetMaxTreeSize(Long64_t(1024)*1024*1024*200); //200 GB tree
    std::vector < remollGenericDetectorHit_t > *fHit = 0;
    std::vector < remollEventParticle_t > *fPart = 0;
    int dotPos = file.rfind(".");   
    std::ostringstream os;
    os << file.substr(0, dotPos) << "_PEs_"<<"det_"<<detid<<".root";
    std::string fileName = os.str();
    TFile *old = new TFile(file.c_str());
    TTree *oldTree = (TTree*)old->Get("T");
    TFile *newFile = new TFile(fileName.c_str(),"RECREATE", "", 1);

    TTree* newTree = new TTree("T", "Tree of detector hits");
    oldTree->SetBranchAddress("hit", &fHit); 
    oldTree->SetBranchAddress("part", &fPart); 
    std::vector < catPEs_t > *catPEs = new std::vector < catPEs_t > ;
    std::vector < Q_t > *Q = new std::vector < Q_t > ;
    std::vector < Ref_t > *Ref = new std::vector < Ref_t > ;
    std::vector < RefX_t > *RefX = new std::vector < RefX_t > ;
    std::vector < Refair_t > *Refair = new std::vector < Refair_t > ;
    std::vector < LG_t > *LG = new std::vector < LG_t > ;
    std::vector < LGair_t > *LGair = new std::vector < LGair_t > ;
    std::vector < PMTcat_t > *PMTcat = new std::vector < PMTcat_t > ;
    std::vector < PMTbulk_t > *PMTbulk = new std::vector < PMTbulk_t > ;
    std::vector < elseX_t > *elseX = new std::vector < elseX_t > ;
    std::vector < remollGenericDetectorHit_t > *hitCopy = new std::vector < remollGenericDetectorHit_t > ;
    std::vector < remollEventParticle_t > *partCopy = new std::vector < remollEventParticle_t > ;

    //TODO reading data into envelopes downstream could be sped up
    //by storing the data by Z instead of by hit
    
    newTree->Branch("catpes", catPEs);
    newTree->Branch("q", &Q);
    newTree->Branch("ref", &Ref);
    newTree->Branch("refx", &RefX);
    newTree->Branch("refair", &Refair);
    newTree->Branch("lg", &LG);
    newTree->Branch("lgair", &LGair);
    newTree->Branch("pmtcat", &PMTcat);
    newTree->Branch("pmtbulk", &PMTbulk);
    newTree->Branch("else", &elseX);
    for (size_t j = 0; j < oldTree->GetEntries(); j++)
    {
        if (j%10000 == 0) 
        {
            std::cerr << "\r" <<  j << "/" << oldTree->GetEntries() << " - " << (j*1.0)/oldTree->GetEntries() * 100 << "%";
        }
     
        oldTree->GetEntry(j);
        //std::cout << "Hits: " << fHit->size() << std::endl;
        //std::cout << "Parts: " << fPart->size() << std::endl;
        std::vector<int> eTRID;  
        std::vector<int> eDETID;
        std::vector<int> peTRID;
        int detSourcedPEs = 0;
        std::vector<double> x, y, z, px, py, pz, p, e, m, vx, vy, vz;
        std::vector<double> catHitx, catHity, catHitz;
        // Log which electron track IDs (and other info) hit which detectors, and which PE IDs hit the cathode and where
        // Attribute all npe counts and cathode hit positions to all eDETID branches that are hit during that event (in lieu of getting photon birth vertex properly down)
        // int npe = 0; // do this as peTRID->Size();

        for (size_t i = 0; i < fHit->size(); i++)
        {
            remollGenericDetectorHit_t hit = fHit->at(i); 
            //Count each optical photon hit in an entry, iff the primary electron hits a detector, also sum accidentals
            // Make a root tree out of the amounts (and locations) of photon hits on the various detectors
            // Each branch is a different detector that the electron can hit (quartz, wall, air, cathode, PMT, etc.)
            //  Store the electron hit location and energy, etc. for that hit
            //  Store the total number of photons that hit the cathode after that electron hit, and their cathode hit info
            
            if (hit.pid == 11 && hit.mtrid == 1){ // Then this is our primary signal of interest
                eTRID.push_back(hit.trid);
                eDETID.push_back(hit.det);
                x.push_back(hit.x);
                y.push_back(hit.y);
                z.push_back(hit.z);
                px.push_back(hit.px);
                py.push_back(hit.py);
                pz.push_back(hit.pz);
                vx.push_back(hit.vx);
                vy.push_back(hit.vy);
                vz.push_back(hit.vz);
                p.push_back(hit.p);
                e.push_back(hit.e);
                m.push_back(hit.m);
            }
            if (hit.pid == 0){ // Then this is an optical photon and we want to count ++ if it hits the PMT cathode
                peTRID.push_back(hit.trid);
                catHitx.push_back(hit.x);
                catHity.push_back(hit.y);
                catHitz.push_back(hit.z);
            }
        }

        for (size_t i = 0; i < fHit->size();i++)
        {
            remollGenericDetectorHit_t hit = fHit->at(i);
            for (size_t k = 0; k < eTRID.size(); k++)
            {
                if (hit.trid == eTRID.at(k)) { // Then this electron hit a detector we should know about and store all of the info and cathode hits too
                    detSourcedPEs=(int)peTRID.size();
                    //std::cout << "electron TRID " << hit.trid << std::endl;
                    // Make the electron hit info go into the appropriate detector branch
                    if (hit.det == detid) {     // quartz
                        Q->push_back(QTrim(hit,peTRID.size(),catHitx,catHity,catHitz)); 
                    }
                    if (hit.det == detid+100) { // reflector air
                        Refair->push_back(RefairTrim(hit,peTRID.size(),catHitx,catHity,catHitz)); 
                    }
                    if (hit.det == detid+200) { // reflector skin
                        Ref->push_back(RefTrim(hit,peTRID.size(),catHitx,catHity,catHitz)); 
                    }
                    if (hit.det == detid+300) { // reflector volume
                        RefX->push_back(RefXTrim(hit,peTRID.size(),catHitx,catHity,catHitz));  
                    }
                    if (hit.det == detid+400) { // light guide air
                        LGair->push_back(LGairTrim(hit,peTRID.size(),catHitx,catHity,catHitz));   
                    }
                    if (hit.det == detid+500) { // light guide skin
                        LG->push_back(LGTrim(hit,peTRID.size(),catHitx,catHity,catHitz));   
                    }
                    if (hit.det == detid+600) { // PMT bulk
                        PMTbulk->push_back(PMTbulkTrim(hit,peTRID.size(),catHitx,catHity,catHitz));   
                    }
                    if (hit.det == detid+700) { // PMT cathode
                        PMTcat->push_back(PMTcatTrim(hit,peTRID.size(),catHitx,catHity,catHitz));   
                    }
                    //break; //how is this useful?? I want to muliple count
                }
            }   
        }
        catPEs->push_back(catPEsTrim(eDETID,(int)peTRID.size(),catHitx,catHity,catHitz)); 
        elseX->push_back(elseXTrim(((int)peTRID.size()-detSourcedPEs),catHitx,catHity,catHitz));
        if (hitCopy->size() > 0){
            newTree->Fill();
	    }
        hitCopy->clear();
        partCopy->clear();
    }
    newFile = newTree->GetCurrentFile();
    newTree->Write("", TObject::kOverwrite);
    newTree->Print();
    old->Close();
    newFile->Close();
}

int main(int argc, char **argv)
{
    std::string fileString = "tracking.root";
    int detid=50001;
    if (argc <= 1 || argc > 3)
    {
        std::cerr << "Usage: ./pe char*:filename int:detid" << std::endl;
        exit(0);
    }
    if (argc >= 2) 
    {
        std::string fileName(argv[1]); 
        fileString = fileName;
    }
    if (argc >= 3)
    {
        detid = atoi(argv[2]);    
    }
    std::cout << "Running with file=" << fileString << ", detid=" << detid << std::endl; 
    pe(fileString, detid);
}

