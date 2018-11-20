#include "remolltypes.hh"
#include "petypes.hh"

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
