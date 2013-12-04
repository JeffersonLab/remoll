#ifndef __REMOLLBEAMTARGET_HH
#define __REMOLLBEAMTARGET_HH

#include "remolltypes.hh"
#include "remollglobs.hh"
#include "remollVertex.hh"
#include "G4ThreeVector.hh"
#include <vector>

/*!
     Class that contains information on 
     the beam and target.  It needs to be
     aware of and consistant with what is
     in the geometry.

     This is responsible for:
         Rastering, arbitrary beam angle
	 Sampling along the target
	 Pre-vertex multiple scattering
	 External radiative effects
	 Luminosity calculations
 
     This is implemented in the singleton model

*/

class G4VPhysicalVolume;
class G4Material;
class remollMultScatt;

class remollBeamTarget {
    private: 
	static remollBeamTarget *gSingleton;
	remollBeamTarget();

    public:
	static remollBeamTarget *GetBeamTarget();
	~remollBeamTarget();

	G4double GetEffLumin();

	void Reset(){ fTargVols.clear(); fMother = NULL; UpdateInfo(); }
	void SetMotherVolume( G4VPhysicalVolume *v ){ fMother = v; UpdateInfo(); }
	void AddVolume( G4VPhysicalVolume *v ){ fTargVols.push_back(v);  UpdateInfo(); }
	void SetTargetPos(G4double z);
	void SetTargetLen(G4double l);

	void SetBeamCurrent(G4double I){ fBeamCurr = I; }

	remollVertex SampleVertex(SampType_t);

	G4double fBeamE;
	G4double fBeamCurr;
	G4double fBeamPol;

	std::vector <G4VPhysicalVolume *> GetTargVols(){ return fTargVols; }

	remollMultScatt *fMS;

    private:
	std::vector <G4VPhysicalVolume *> fTargVols;
	G4VPhysicalVolume *fMother;

	void UpdateInfo();


	G4double fTotalLength;
	G4double fLH2Length, fZpos, fLH2pos;

	G4Material *fDefaultMat;

	bool fAlreadyWarned;

	
    public:
	// Base position, angle *sampled* info
	G4ThreeVector fVer, fDir;
	G4double fSampE, fRadLen, fSampLen;
	G4double fTravLen;
	G4double fEcut, fEffMatLen;

	// Base position/angle sampling info
	G4double fRasterX, fRasterY;
	G4double fX0, fY0;
	G4double fTh0, fPh0;
	G4double fdTh, fdPh, fCorrTh, fCorrPh;

};


#endif//__REMOLLBEAMTARGET_HH

