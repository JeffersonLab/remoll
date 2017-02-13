#if defined(__CINT__) || defined(__CLING__)

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;

#pragma link C++ class remollRunData+;
#pragma link C++ class remollTextFile+;
#pragma link C++ struct filedata_t+;

#pragma link C++ struct remollUnits_t+;

#pragma link C++ struct remollEvent_t+;
#pragma link C++ struct remollBeamTarget_t+;
#pragma link C++ struct remollEventParticle_t+;
#pragma link C++ class vector<remollEventParticle_t>+;

#pragma link C++ struct remollGenericDetectorHit_t+;
#pragma link C++ class vector<remollGenericDetectorHit_t>+;

#pragma link C++ struct remollGenericDetectorSum_t+;
#pragma link C++ class vector<remollGenericDetectorSum_t>+;

#endif

