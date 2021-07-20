#include "remolltypes.hh"
#include "petypes.hh"

catPEs_t catPEsTrim(int sourcedetid, std::vector<int> sourcedetids, std::vector<int> quartzpids, std::vector<int> quartzmtrids, size_t peLen, double all_bounces, double ref_bounces, double lg_bounces, std::vector<double> cathitx, std::vector<double> cathity, std::vector<double> cathitz)
{
    catPEs_t newHit;
    newHit.npes=peLen;
    newHit.all_bounces=all_bounces;
    newHit.ref_bounces=ref_bounces;
    newHit.lg_bounces=lg_bounces;
    newHit.det=sourcedetid;
    newHit.detids=sourcedetids;
    newHit.pids=quartzpids;
    newHit.mtrids=quartzmtrids;
    newHit.cathitx=cathitx;
    newHit.cathity=cathity;
    newHit.cathitz=cathitz;
    return newHit; 
}

hitPEs_t hitPEsTrim(remollGenericDetectorHit_t hit, size_t peLen, std::vector<double> cathitx, std::vector<double> cathity, std::vector<double> cathitz)
{
    hitPEs_t newHit;
    newHit.det=hit.det;
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

