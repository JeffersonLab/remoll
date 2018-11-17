#include <sstream>
#include <string>
#include <iostream>

#include "TTree.h"
#include "TFile.h"
#include "remolltypes.hh"
#include "petypes.hh"
#include <vector> 


#define pi 3.141592653589793238462643383279502884L

catPEs_t catPEsTrim(std::vector<int> sourcedetids, std::vector<int> quartzpids, std::vector<int> quartzmtrids, size_t peLen, std::vector<double> cathitx, std::vector<double> cathity, std::vector<double> cathitz)
{
    catPEs_t newHit;
    newHit.npes=peLen;
    newHit.detids=sourcedetids;
    newHit.pids=quartzpids;
    newHit.mtrids=quartzmtrids;
    newHit.cathitx=cathitx;
    newHit.cathity=cathity;
    newHit.cathitz=cathitz;
    return newHit; 
}

Q_t QTrim(remollGenericDetectorHit_t hit, size_t peLen, std::vector<double> cathitx, std::vector<double> cathity, std::vector<double> cathitz)
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
    newHit.npes=peLen;
    newHit.cathitx=cathitx;
    newHit.cathity=cathity;
    newHit.cathitz=cathitz;
    return newHit; 
}

Ref_t RefTrim(remollGenericDetectorHit_t hit, size_t peLen, std::vector<double> cathitx, std::vector<double> cathity, std::vector<double> cathitz)
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
    newHit.npes=peLen;
    newHit.cathitx=cathitx;
    newHit.cathity=cathity;
    newHit.cathitz=cathitz;
    return newHit; 
}

RefX_t RefXTrim(remollGenericDetectorHit_t hit, size_t peLen, std::vector<double> cathitx, std::vector<double> cathity, std::vector<double> cathitz)
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
    newHit.npes=peLen;
    newHit.cathitx=cathitx;
    newHit.cathity=cathity;
    newHit.cathitz=cathitz;
    return newHit; 
}

Refair_t RefairTrim(remollGenericDetectorHit_t hit, size_t peLen, std::vector<double> cathitx, std::vector<double> cathity, std::vector<double> cathitz)
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
    newHit.npes=peLen;
    newHit.cathitx=cathitx;
    newHit.cathity=cathity;
    newHit.cathitz=cathitz;
    return newHit; 
}

LG_t LGTrim(remollGenericDetectorHit_t hit, size_t peLen, std::vector<double> cathitx, std::vector<double> cathity, std::vector<double> cathitz)
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
    newHit.npes=peLen;
    newHit.cathitx=cathitx;
    newHit.cathity=cathity;
    newHit.cathitz=cathitz;
    return newHit; 
}

LGair_t LGairTrim(remollGenericDetectorHit_t hit, size_t peLen, std::vector<double> cathitx, std::vector<double> cathity, std::vector<double> cathitz)
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
    newHit.npes=peLen;
    newHit.cathitx=cathitx;
    newHit.cathity=cathity;
    newHit.cathitz=cathitz;
    return newHit; 
}

PMTcat_t PMTcatTrim(remollGenericDetectorHit_t hit, size_t peLen, std::vector<double> cathitx, std::vector<double> cathity, std::vector<double> cathitz)
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
    newHit.npes=peLen;
    newHit.cathitx=cathitx;
    newHit.cathity=cathity;
    newHit.cathitz=cathitz;
    return newHit; 
}

PMTbulk_t PMTbulkTrim(remollGenericDetectorHit_t hit, size_t peLen, std::vector<double> cathitx, std::vector<double> cathity, std::vector<double> cathitz)
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
    newHit.npes=peLen;
    newHit.cathitx=cathitx;
    newHit.cathity=cathity;
    newHit.cathitz=cathitz;
    return newHit; 
}

elseX_t elseXTrim(int peLen, std::vector<double> cathitx, std::vector<double> cathity, std::vector<double> cathitz)
{
    elseX_t newHit;
    newHit.npes=peLen;
    newHit.cathitx=cathitx;
    newHit.cathity=cathity;
    newHit.cathitz=cathitz;
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
    std::vector<int> eTRID;  
    std::vector<int> DETID;
    std::vector<int> PID;
    std::vector<int> MTRID;
    std::vector<int> peTRID;
    int Qcounted = 0;
    int Refaircounted = 0;
    int LGaircounted = 0;
    int PMTbulkcounted = 0;
    int detSourcedPEs = 0;

    //TODO reading data into envelopes downstream could be sped up
    //by storing the data by Z instead of by hit
    
    newTree->Branch("catpes", &catPEs);
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
        //std::vector<double> x, y, z, px, py, pz, p, e, m, vx, vy, vz;
        std::vector<double> cathitx, cathity, cathitz;
        // Log which electron track IDs (and other info) hit which detectors, and which PE IDs hit the cathode and where
        // Attribute all npe counts and cathode hit positions to all DETID branches that are hit during that event (in lieu of getting photon birth vertex properly down)
        // int npe = 0; // do this as peTRID->Size();

        for (size_t i = 0; i < fHit->size(); i++)
        {
            remollGenericDetectorHit_t hit = fHit->at(i); 
            //Count each optical photon hit in an entry, iff the primary electron hits a detector, also sum accidentals
            // Make a root tree out of the amounts (and locations) of photon hits on the various detectors
            // Each branch is a different detector that the electron can hit (quartz, wall, air, cathode, PMT, etc.)
            //  Store the electron hit location and energy, etc. for that hit
            //  Store the total number of photons that hit the cathode after that electron hit, and their cathode hit info
            
            if (hit.pid == 11 && hit.mtrid == 0){ // Then this is our primary signal of interest
            // if you do mtrid == 1 then you get the delta rays! About a 1% contribution 
                eTRID.push_back(hit.trid);
                DETID.push_back(hit.det);
                if (i<30 && hit.det == 50001){
                    std::cout<<"hit.trid of a hit.mtrid==0 and hit.pid==11 and hit.det==50001 (primary e on quartz): "<<hit.trid<<std::endl;
                }
            }
            if (hit.det == detid) {
                // If any particle hits the quartz detector then tell the particle ID and mother ID (so we can keep track of deltas) 
                PID.push_back(hit.pid);
                MTRID.push_back(hit.mtrid);
            }
            if (hit.pid == 0 && hit.det == detid+700){
                // This is an optical photon and we want to count ++ if it hits the PMT cathode
                peTRID.push_back(hit.trid);
                cathitx.push_back(hit.x);
                cathity.push_back(hit.y);
                cathitz.push_back(hit.z);
            }
        }

        for (size_t i = 0; i < fHit->size();i++)
        {
            remollGenericDetectorHit_t hit = fHit->at(i);
            for (size_t k = 0; k < eTRID.size(); k++)
            {
                if (hit.trid == eTRID.at(k)) { // Then this electron hit a detector we should know about and store all of the info and cathode hits too
                    //std::cout << "electron TRID " << hit.trid << std::endl;
                    // Make the electron hit info go into the appropriate detector branch
                    // quartz
                    if (hit.det == detid && Qcounted==0) {     
                        Q->push_back(QTrim(hit,peTRID.size(),cathitx,cathity,cathitz)); 
                        std::cout<<"hit.trid of a hit.mtrid==0 and hit.pid==11 and hit.det==50001 (primary e on quartz): "<<hit.trid<<", and NPE: "<<peTRID.size()<<std::endl;
                        Qcounted++;
                    }
                    // reflector air
                    if (hit.det == detid+100 && Refaircounted==0) { 
                        Refair->push_back(RefairTrim(hit,peTRID.size(),cathitx,cathity,cathitz)); 
                        Refaircounted++;
                    }
                    // reflector skin
                    if (hit.det == detid+200) { 
                        Ref->push_back(RefTrim(hit,peTRID.size(),cathitx,cathity,cathitz)); 
                    }
                    // reflector volume
                    if (hit.det == detid+300) { 
                        RefX->push_back(RefXTrim(hit,peTRID.size(),cathitx,cathity,cathitz));  
                    }
                    // light guide air
                    if (hit.det == detid+400 && LGaircounted==0) { 
                        LGair->push_back(LGairTrim(hit,peTRID.size(),cathitx,cathity,cathitz));   
                        LGaircounted++;
                    }
                    // light guide skin
                    if (hit.det == detid+500) { 
                        LG->push_back(LGTrim(hit,peTRID.size(),cathitx,cathity,cathitz));   
                    }
                    // PMT bulk
                    if (hit.det == detid+600 && PMTbulkcounted==0) {
                        PMTbulk->push_back(PMTbulkTrim(hit,peTRID.size(),cathitx,cathity,cathitz));   
                        PMTbulkcounted++;
                    }
                    // PMT cathode
                    if (hit.det == detid+700) { 
                        PMTcat->push_back(PMTcatTrim(hit,peTRID.size(),cathitx,cathity,cathitz));   
                    }
                    //break; //how is this useful?? I want to muliple count
                }
            }   
        }
        catPEs->push_back(catPEsTrim(DETID,PID,MTRID,(int)peTRID.size(),cathitx,cathity,cathitz)); 
        detSourcedPEs=(int)peTRID.size();
        elseX->push_back(elseXTrim(((int)peTRID.size()-detSourcedPEs),cathitx,cathity,cathitz));
        if (catPEs->size() > 0){
            newTree->Fill();
	    }
        Qcounted=0;
        Refaircounted=0;
        LGaircounted=0;
        PMTbulkcounted=0;
        detSourcedPEs=0;
        eTRID.clear();
        catPEs->clear();
        elseX->clear();
        Q->clear();
        Refair->clear();
        Ref->clear();
        RefX->clear();
        LGair->clear();
        LG->clear();
        PMTbulk->clear();
        PMTcat->clear();
        cathitx.clear();
        cathity.clear();
        cathitz.clear();
        DETID.clear();
        PID.clear();
        MTRID.clear();
        peTRID.clear();
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

