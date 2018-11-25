#include <sstream>
#include <string>
#include <iostream>

#include "TTree.h"
#include "TFile.h"
#include "remolltypes.hh"
#include <vector> 


#define pi 3.141592653589793238462643383279502884L

remollGenericDetectorHit_t trim(remollGenericDetectorHit_t hit)
{
    remollGenericDetectorHit_t newHit;
    newHit.det = hit.det;
    newHit.id = hit.id;
    newHit.trid=0;
    newHit.pid = hit.pid;
    newHit.gen=0;
    newHit.mtrid=hit.mtrid;
    newHit.x=0;
    newHit.y=0;
    newHit.z=0;
    newHit.xl=0;
    newHit.yl=0;
    newHit.zl=0;
    newHit.r=0;
    newHit.ph=0;
    newHit.px=0;
    newHit.py=0;
    newHit.pz=0;
    newHit.pxl=0;
    newHit.pyl=0;
    newHit.pzl=0;
    newHit.sx=0;
    newHit.sy=0;
    newHit.sz=0;
    newHit.p=0;
    newHit.e=0;
    newHit.m=0;
    newHit.vx=0;
    newHit.vy=0;
    newHit.vz=0;
    return newHit; 
}

remollEventParticle_t trim(remollEventParticle_t part)
{
    remollEventParticle_t newPart;
    newPart.trid = part.trid;
    newPart.pid = part.pid;
    newPart.vx=0;
    newPart.vy=0;
    newPart.vz=0;
    newPart.px=0;
    newPart.py=0;
    newPart.pz=0;
    newPart.sx=0;
    newPart.sy=0;
    newPart.sz=0;
    newPart.th=0;
    newPart.ph=0;
    newPart.p=0;
    newPart.tpx=0;
    newPart.tpy=0;
    newPart.tpz=0;
    newPart.tjx = part.tjx;
    newPart.tjy = part.tjy;
    newPart.tjz = part.tjz;
    return newPart;
}
const double septant = (2*pi/7.0);
const double septantStart = 3 * septant; 
const double septantStop = septantStart + septant; 

double getAngle(double x, double y)
{
    double angle = atan2(y, x);
    return (angle < 0) ? (2*pi)+angle : angle;
}

const double s = sin(2 * pi / 7.0);
const double c = cos(2 * pi / 7.0);
remollEventParticle_t rotateVector(remollEventParticle_t part, bool mir)
{
    remollEventParticle_t newPart;
    newPart.pid = part.pid;
    newPart.trid = part.trid;
    double startZ = 5880;//5980; //right after the end of the acceptance defining collimator
    double x, y;
    bool rot = false;
    for (int i = 0; i < part.tjz.size()-1; i++)
    {
        double zi = part.tjz.at(i);
        double zf = part.tjz.at(i+1);
        if(startZ == zi){
            x = part.tjx.at(i); 
            y = part.tjy.at(i); 
            rot = true;
            break;
        }
        else if(startZ == zf){
            x = part.tjx.at(i+1); 
            y = part.tjy.at(i+1); 
            rot = true;
            break;
        }
        else if(zi > startZ && zf > startZ){
            double dx = part.tjx.at(i+1) - part.tjx.at(i);
            double dy = part.tjy.at(i+1) - part.tjy.at(i);
            double dz = zf - zi;
            x = part.tjx.at(i) + (dx/dz)*(startZ-zi);
            y = part.tjy.at(i) + (dy/dz)*(startZ-zi);
            rot = true;
            break;
        }
    }
    //std::cout << "From " << getAngle(x, y) / septant << std::endl;
    int numSep = 0;
    while (getAngle(x, y) <= septantStart || getAngle(x, y) >= septantStop)
    {
        numSep++;
        double tX = x * c - y * s;
        double tY = x * s + y * c;
        x = tX;
        y = tY; 
    }

    for (int i = 0; i < part.tjz.size(); i++)
    {
        x = part.tjx.at(i);   
        y = part.tjy.at(i);   
        for (int j = 0; j < numSep; j++)
        {
            double tX = x * c - y * s;
            double tY = x * s + y * c;
            x = tX;
            y = tY;
        }
        newPart.tjx.push_back(x);
        if (mir) {
            newPart.tjy.push_back((y < 0 )? -y : y);
        }
        else{
            newPart.tjy.push_back(y);
        }
        newPart.tjz.push_back(part.tjz.at(i));
    }
    //std::cout << "Rotated " << std::endl;
    return newPart;
}

remollGenericDetectorHit_t rotateVector(remollGenericDetectorHit_t hit, bool mir)
{
    remollGenericDetectorHit_t newHit;// = new remollGenericDetectorHit_t();
    newHit.z = hit.z;
    newHit.pz = hit.pz;
    newHit.id = hit.id;
    newHit.det = hit.det;
    newHit.pid = hit.pid;

    double x, y;
    x = hit.x;
    y = hit.y;
    //std::cout << "From " << getAngle(x, y) / (2 * pi) * 7 << std::endl;
    while (getAngle(x, y) <= septantStart || getAngle(x, y) >= septantStop)
    {
        double tX, tY;
        tX = x * c - y * s;
        tY = x * s + y * c;
        x = tX;
        y = tY;
    }
    //std::cout << "To " << getAngle(x, y) / (2 * pi) * 7 << std::endl;
    newHit.x = x;
    if (mir){
        newHit.y = (y < 0)? -y : y;
    }
    else {
        newHit.y = y;
    }
    return newHit;
}

remollEventParticle_t interpolate(remollEventParticle_t part){
    remollEventParticle_t newPart;
    newPart.pid = part.pid;
    newPart.trid = part.trid;
    int stepSize = 10;
    for(size_t z = 4500; z <= 30000; z+=stepSize){
        //if (z >= 12500)
        //    stepSize = 500;
        //else if (z >= 10500)
        //    stepSize = 200;
        
        for(size_t i = 0; i < (part.tjx).size()-1; i++){
            double x, y, dx, dy, dz;
            double xi = part.tjx[i];
            double yi = part.tjy[i];
            double zi = part.tjz[i];
            double xf = part.tjx[i+1];
            double yf = part.tjy[i+1];
            double zf = part.tjz[i+1];

            if(z==zi){
                newPart.tjx.push_back(xi);
                newPart.tjy.push_back(yi);
                newPart.tjz.push_back(z);
            }
            else if(z==zf){
                newPart.tjx.push_back(xf);
                newPart.tjy.push_back(yf);
                newPart.tjz.push_back(z);
            }
            else if(z>zi && z <zf){
                dx = xf - xi;
                dy = yf - yi;
                dz = zf - zi;
                x = xi + (dx/dz)*(z-zi);
                y = yi + (dy/dz)*(z-zi);
                newPart.tjx.push_back(x);
                newPart.tjy.push_back(y);	
                newPart.tjz.push_back(z);
            }
            else {}
        }
    }
    return newPart;    
}

void pruneTreeEnvelope(std::string file="tracking.root", int detid=28, double energyCut=0.0, int ringCut=0, bool forceSeptant=true)
{
    TTree::SetMaxTreeSize(Long64_t(1024)*1024*1024*200); //200 GB tree
    std::vector < remollGenericDetectorHit_t > *fHit = 0;
    std::vector < remollEventParticle_t > *fPart = 0;
    int dotPos = file.rfind(".");   
    std::ostringstream os;
    os << file.substr(0, dotPos) << "_envelope_det" << detid << ".root";
    std::string fileName = os.str();
    bool mirror = false;
    if (forceSeptant=true){
        mirror = true;
    }
    // Default look at stuff hitting the entire detector array
    bool hitRcut = true;
    double lowR = 600.0;
    double highR = 1500.0;
    if (ringCut>0){
        hitRcut=true;
        if (ringCut==6){
            // Ring 6
            lowR = 1100.0;
            highR = 1200.0;
        }
        if (ringCut==5){
            // Ring 5 mollers
            lowR = 935.0;
            highR = 1100.0;
        }
        if (ringCut==4){
            // Ring 4 
            lowR = 855.0;
            highR = 935.0;
        }
        if (ringCut==3){
            // Ring 3
            lowR = 780.0;
            highR = 855.0;
        }
        if (ringCut==2){
            // Ring 2 ep elastics
            lowR = 730.0;
            highR = 780.0;
        }
        if (ringCut==1){
            // Ring 1 ep super elastics
            lowR = 690.0;
            highR = 730.0;
        }
    }
    double lowE = 0.0;
    bool lowEcut = false;
    if (energyCut>0){
        lowE = energyCut;
        lowEcut = true;
    }
    TFile *old = new TFile(file.c_str());
    TTree *oldTree = (TTree*)old->Get("T");
    TFile *newFile = new TFile(fileName.c_str(),"RECREATE", "", 1);

    TTree* newTree = new TTree("T", "Optimized Tree of Tracks");
    oldTree->SetBranchAddress("hit", &fHit); 
    oldTree->SetBranchAddress("part", &fPart); 
    std::vector < remollGenericDetectorHit_t > *hitCopy = new std::vector < remollGenericDetectorHit_t > ;
    std::vector < remollEventParticle_t > *partCopy = new std::vector < remollEventParticle_t > ;

    //TODO reading data into envelopes downstream could be sped up
    //by storing the data by Z instead of by hit
    
    newTree->Branch("hit", &hitCopy);
    newTree->Branch("part", &partCopy);
    //newTree->AutoSave();
    //oldTree->Print();
    for (size_t j = 0; j < oldTree->GetEntries(); j++)
    {
        if (j%10000 == 0) 
        {
            std::cerr << "\r" <<  j << "/" << oldTree->GetEntries() << " - " << (j*1.0)/oldTree->GetEntries() * 100 << "%";
        }
     
        oldTree->GetEntry(j);
        //std::cout << "Hits: " << fHit->size() << std::endl;
        //std::cout << "Parts: " << fPart->size() << std::endl;
        std::vector<int> goodTRID;  
        std::vector<int> worthyTRID;

        for (size_t i = 0; i < fHit->size(); i++)
        {
            remollGenericDetectorHit_t hit = fHit->at(i); 
            //Get all track ids that hit into desired det
            if (hit.det == detid && (!lowEcut || !(hit.e<lowE)) && (!hitRcut || !(hit.r<lowR || hit.r>highR)))
            {
                //std::cout << "good trid" << hit.trid << std::endl;
                goodTRID.push_back(hit.trid);
            }
        }
        
        for (size_t i = 0; i < fPart->size();i++)
        {
            remollEventParticle_t part = fPart->at(i);
            for (size_t k = 0; k < goodTRID.size(); k++)
            {
                //Assume vector index of part vector is the track id, trid starts at 1
                //Of track ids that hit into desired det, get those that are saved
                if (part.trid == goodTRID.at(k))
                {
                    //std::cout << "good part TRID " << part.trid << std::endl;
                    worthyTRID.push_back(part.trid);
	                //Interpolate at z = 4,500mm to 30,000mm in increments of 10mm.
                    if (forceSeptant) part = interpolate(rotateVector(part, mirror));
                    else part = interpolate(part);
                    partCopy->push_back(trim(part));
                    break;
                }
            }   
        }
        for (size_t k = 0; k < worthyTRID.size(); k++)
        {
            int trid = worthyTRID.at(k);
            for (size_t i = 0; i < fHit->size(); i++)
            {
                remollGenericDetectorHit_t hit = fHit->at(i); 
                //and save the corresponding hit aswell
                if (trid == hit.trid)
                {
                    if (forceSeptant) hit = rotateVector(hit, mirror);
                    hitCopy->push_back(trim(hit));
                    break;
                }

            }
        }
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
    int detid = 28;
    double energyCut = 0.0;
    int ringRadialCut = 0;
    bool forceSeptant = true;
    if (argc <= 1 || argc > 6)
    {
        std::cerr << "Usage: ./pruneTreeEnvelope char*:filename int:detid(default 28) double:energyCut(MeV, default 0) int:ringRadialCut(default 0 = all) y/n:rotateIntoSeptant(default y)" << std::endl;
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
    if (argc >= 4) 
    {
        energyCut = atof(argv[3]); 
    }
    if (argc >= 5)
    {
        ringRadialCut = atoi(argv[4]);    
    }
    if (argc >= 6)
    {
        forceSeptant = (argv[5][0] == 'y');
    }
    std::cout << "Running with file=" << fileString << ", detid=" << detid << ", energyCut=" << energyCut << " MeV, ringRadialCut=" << ringRadialCut <<", forceSeptant=" << forceSeptant << std::endl; 
    pruneTreeEnvelope(fileString, detid, energyCut, ringRadialCut, forceSeptant);
}

