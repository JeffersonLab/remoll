#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4VPhysicalVolume.hh"
#include "G4LogicalVolume.hh"
#include "G4VSolid.hh"
#include "G4Material.hh"

#ifdef G4MULTITHREADED
#include "G4MTRunManager.hh"
#else
#include "G4RunManager.hh"
#endif

#include "G4GeometryManager.hh"
#include "G4SystemOfUnits.hh"
#include "G4PhysicalConstants.hh"

#include "Randomize.hh"

#include "remollBeamTarget.hh"

#include <math.h>

#include "G4Threading.hh"
#include "G4AutoLock.hh"
namespace { G4Mutex remollBeamTargetMutex = G4MUTEX_INITIALIZER; }

// Initialize static geometry objects
G4String remollBeamTarget::fActiveTargetMotherName = "LH2";
G4String remollBeamTarget::fActiveTargetVolumeName = "LH2";
size_t remollBeamTarget::fActiveTargetMother = 0;
size_t remollBeamTarget::fActiveTargetVolume = 0;
std::vector<std::pair<G4VPhysicalVolume*,G4String>> remollBeamTarget::fTargetMothers;
std::vector<std::vector<std::pair<G4VPhysicalVolume*,G4String>>> remollBeamTarget::fTargetVolumes;

G4double remollBeamTarget::fActiveTargetEffectiveLength  = -1e9;
G4double remollBeamTarget::fMotherTargetAbsolutePosition = -1e9;
G4double remollBeamTarget::fTotalTargetEffectiveLength = 0.0;
G4bool remollBeamTarget::fUpdateNeeded = true;

remollBeamTarget::remollBeamTarget()
: fBeamEnergy(gDefaultBeamE),fBeamCurrent(gDefaultBeamCur),fBeamPolarization(gDefaultBeamPol),
  fOldRaster(true),fRasterX(5.0*mm),fRasterY(5.0*mm),
  fX0(0.0),fY0(0.0),fTh0(0.0),fPh0(0.0),
  fdTh(0.0),fdPh(0.0),fCorrTh(0.0),fCorrPh(0.0)
{
    // Infrared energy cutoff
    fEnergyCut = 1e-6 * MeV;

    // Default material if sampling volume not found
    fDefaultMat = new G4Material("Default_proton", 1.0, 1.0, 1e-19*g/mole);

    // Create generic messenger
    fMessenger.DeclarePropertyWithUnit("beamcurr","microampere",fBeamCurrent,"Beam current");
    fMessenger.DeclarePropertyWithUnit("beamene","GeV",fBeamEnergy,"Beam energy");

    fMessenger.DeclareProperty("oldras",fOldRaster,"Old (no ang corln) or new (ang corl) raster");
    fMessenger.DeclarePropertyWithUnit("rasx","cm",fRasterX,"Square raster width x (horizontal)");
    fMessenger.DeclarePropertyWithUnit("rasy","cm",fRasterY,"Square raster width y (vertical)");

    fMessenger.DeclarePropertyWithUnit("beam_x0","cm",fX0,"beam initial position in x (horizontal)");
    fMessenger.DeclarePropertyWithUnit("beam_y0","cm",fY0,"beam initial position in y (vertical)");
    fMessenger.DeclarePropertyWithUnit("beam_ph0","deg",fPh0,"beam initial direction in x (horizontal)");
    fMessenger.DeclarePropertyWithUnit("beam_th0","deg",fTh0,"beam initial direction in y (vertical)");
    fMessenger.DeclarePropertyWithUnit("beam_corrph","deg",fCorrPh,"beam correlated angle (horizontal)");
    fMessenger.DeclarePropertyWithUnit("beam_corrth","deg",fCorrTh,"beam correlated angle (vertical)");
    fMessenger.DeclarePropertyWithUnit("beam_dph","deg",fdPh,"beam gaussian spread in x (horizontal)");
    fMessenger.DeclarePropertyWithUnit("beam_dth","deg",fdTh,"beam gaussian spread in y (vertical)");

    fTargetMessenger.DeclareMethod("mother",&remollBeamTarget::SetActiveTargetMother,"Set target mother name").SetStates(G4State_Idle);
    fTargetMessenger.DeclareMethod("volume",&remollBeamTarget::SetActiveTargetVolume,"Set target volume name").SetStates(G4State_Idle);
    fTargetMessenger.DeclareMethod("print",&remollBeamTarget::PrintTargetInfo).SetStates(G4State_Idle);
}

remollBeamTarget::~remollBeamTarget()
{
}

G4double remollBeamTarget::GetEffLumin(SamplingType_t sampling_type) const
{
    if (sampling_type == kNoTargetVolume)
        return fBeamCurrent / (e_SI*coulomb); // no length, just frequency
    else
        return fBeamCurrent / (e_SI*coulomb) * fEffectiveMaterialLength;
}

void remollBeamTarget::PrintTargetInfo()
{
    if (fUpdateNeeded) UpdateInfo();

    for (auto mother  = fTargetMothers.begin();
              mother != fTargetMothers.end();
              mother++) {

        auto i_mother = mother - fTargetMothers.begin();

        G4cout << "Target mother " << (*mother).second << ":" << G4endl;

        for (auto daughter  = fTargetVolumes[i_mother].begin();
                  daughter != fTargetVolumes[i_mother].end();
                  daughter++) {

            G4LogicalVolume* volume = (*daughter).first->GetLogicalVolume();
            G4Material* material = volume->GetMaterial();

            G4cout << "  Target volume " << (*daughter).second << ":";
            G4cout << "  material: " << material->GetName() << G4endl;

        }
    }

    G4cout << "Current active target: " << G4endl;
    G4cout << "Target mother = " << fActiveTargetMotherName << G4endl;
    G4cout << "Target volume = " << fActiveTargetVolumeName << G4endl;

    G4cout << "Final target parameters: " << G4endl;
    G4cout << " total target effective length: " << fTotalTargetEffectiveLength/(gram/cm2) << " gram/cm2" << G4endl;
    G4cout << " active target effective length: " << fActiveTargetEffectiveLength/(gram/cm2) << " gram/cm2" << G4endl;
}

void remollBeamTarget::UpdateInfo()
{
    G4AutoLock lock(&remollBeamTargetMutex);

    fActiveTargetEffectiveLength  = -1e9;
    fMotherTargetAbsolutePosition = -1e9;
    fTotalTargetEffectiveLength = 0.0;

    // Can't calculate anything without mother, let's hope we find one later on
    if (fTargetMothers.size() == 0) {
      return;
    }

    // Find mother volume
    for (auto mother  = fTargetMothers.begin();
              mother != fTargetMothers.end();
              mother++) {

      if ((*mother).second == fActiveTargetMotherName) {
        fActiveTargetMother = mother - fTargetMothers.begin();
      }
    }

    // Find target volume
    for (auto daughter  = fTargetVolumes[fActiveTargetMother].begin();
              daughter != fTargetVolumes[fActiveTargetMother].end();
              daughter++) {

      if ((*daughter).second == fActiveTargetVolumeName) {
        fActiveTargetVolume = daughter - fTargetVolumes[fActiveTargetMother].begin();
      }

    }

    // Get absolute position
    fMotherTargetAbsolutePosition = fTargetMothers[fActiveTargetMother].first->GetTranslation().z() - 4500;

    for (auto it =  fTargetVolumes[fActiveTargetMother].begin();
              it != fTargetVolumes[fActiveTargetMother].end();
              it++) {

        // Try to cast the target volume into its tubs solid
        G4VPhysicalVolume* physvol = (*it).first;
        G4LogicalVolume* volume = physvol->GetLogicalVolume();
        G4Material* material = volume->GetMaterial();
        G4VSolid* solid = volume->GetSolid();

        G4Tubs* tubs = dynamic_cast<G4Tubs*>(solid);
        G4Box* box = dynamic_cast<G4Box*>(solid);

        // Assume everything is non-nested tubes
	if ((tubs == nullptr) && (box == nullptr)) {
	    G4cerr << "ERROR:  " << __PRETTY_FUNCTION__ << " line " << __LINE__ <<
		":  Target volume " << volume->GetName() << " not made of G4Tubs or G4Box" << G4endl;
	    exit(1);
	}

        G4double z_half_length = 0;
        if (tubs != nullptr) z_half_length = tubs->GetZHalfLength();
        if (box != nullptr)  z_half_length = box->GetZHalfLength();

        fTotalTargetEffectiveLength += 2.0 * z_half_length * material->GetDensity();

	if (it == fTargetVolumes[fActiveTargetMother].begin() + fActiveTargetVolume) {

	    fActiveTargetEffectiveLength = 2.0 * z_half_length * material->GetDensity();
	}
    }

    fUpdateNeeded = false;
}


void remollBeamTarget::SetActiveTargetMother(G4String name)
{
  G4AutoLock lock(&remollBeamTargetMutex);
  fActiveTargetMotherName = name;
  fUpdateNeeded = true;
}

void remollBeamTarget::SetActiveTargetVolume(G4String name)
{
  G4AutoLock lock(&remollBeamTargetMutex);
  fActiveTargetVolumeName = name;
  fUpdateNeeded = true;
}


////////////////////////////////////////////////////////////////////////////////////////////
//  Sampling functions

remollVertex remollBeamTarget::SampleVertex(SamplingType_t sampling_type)
{
    // Create vertex
    remollVertex vertex;

    // No sampling required
    static bool sampling_type_has_been_warned = false;
    if (sampling_type == kNoTargetVolume) {
      if (! sampling_type_has_been_warned) {
        G4cerr << "ERROR:  " << __PRETTY_FUNCTION__ << " line " << __LINE__ << ": " <<
                  "kNoTargetVolume!" << G4endl;
        sampling_type_has_been_warned = true;
      }
      return vertex;
    }

    // Check if target mother volume exists
    if (fTargetMothers.size() == 0) {
      G4cerr << "ERROR:  " << __PRETTY_FUNCTION__ << " line " << __LINE__ << ": " <<
                "No target mother volume defined!" << G4endl;
    }

    // Check if target volume exists
    if (fTargetVolumes[fActiveTargetMother].size() == 0) {
      G4cerr << "ERROR:  " << __PRETTY_FUNCTION__ << " line " << __LINE__ << ": " <<
                "No target volume defined!" << G4endl;
    }

    // Sample raster x and y positions on target
    // (assumed independent of z position)
    G4double rasx = G4RandFlat::shoot(fX0 - fRasterX/2.0, fX0 + fRasterX/2.0);
    G4double rasy = G4RandFlat::shoot(fY0 - fRasterY/2.0, fY0 + fRasterY/2.0);

    // Sample where along target weighted by density (which roughly corresponds to A
    // or the number of electrons, which is probably good enough for this

    // Update if needed
    if (fUpdateNeeded) UpdateInfo();

    // Figure out how far along the target we got
    G4double total_effective_length = 0;
    switch (sampling_type) {
        case kActiveTargetVolume:
            total_effective_length = fActiveTargetEffectiveLength;
            break;

        case kAllTargetVolumes:
            total_effective_length = fTotalTargetEffectiveLength;
            break;

        case kNoTargetVolume:
            // nothing to do, just avoid compilation warning
            break;
    }
    G4double effective_position = G4RandFlat::shoot(0.0, total_effective_length);


    G4bool found_active_volume = false;

    // Cumulative lengths
    G4double cumulative_effective_length = 0.0;
    G4double cumulative_radiation_length = 0.0;

    // Start with no multiple scattering materials loaded:
    // this may seem like something that can be made static,
    // but it's probably not worth it since only called once per event.
    std::vector<std::tuple<double,double,double>> ms;

    // Figure out the material we are in and the radiation length we traversed
    for (auto it =  fTargetVolumes[fActiveTargetMother].begin();
              it != fTargetVolumes[fActiveTargetMother].end() && !found_active_volume;
              it++) {

        // Target volume
        G4VPhysicalVolume* physvol = (*it).first;

        // Relative position of this target volume in mother volume
        G4double volume_relative_position = physvol->GetTranslation().z();

        // Try to cast the target volume into its tubs solid
        G4LogicalVolume* volume = physvol->GetLogicalVolume();
        G4Material* material = volume->GetMaterial();
        G4VSolid* solid = volume->GetSolid();

        G4Tubs* tubs = dynamic_cast<G4Tubs*>(solid);
        G4Box* box = dynamic_cast<G4Box*>(solid);

        if ((tubs == nullptr) && (box == nullptr)) {
          G4cerr << "ERROR:  " << __PRETTY_FUNCTION__ << " line " << __LINE__ <<
                    ":  Target volume " << volume->GetName() << " not made of G4Tubs or G4Box" << G4endl;
	    continue; // exit(1);
        }

        G4double z_half_length = 0;
        if (box != nullptr)  z_half_length = box->GetZHalfLength();
        if (tubs != nullptr) z_half_length = tubs->GetZHalfLength();

        // Effective length of this target volume
        G4double effective_length = z_half_length*2.0 * material->GetDensity();

        // Find position in this volume (if we are in it)
        G4double effective_position_in_volume;
        G4double actual_position_in_volume;
        switch (sampling_type) {
	    case kActiveTargetVolume:
	        if (it == fTargetVolumes[fActiveTargetMother].begin() + fActiveTargetVolume) {
	            // This is the active volume, and we only sample here
	            found_active_volume = true;
	            actual_position_in_volume = effective_position/material->GetDensity();
	            // but we still want cumulative radiation lengths of part of the volume
	            cumulative_radiation_length += actual_position_in_volume/material->GetRadlen();
	        } else {
	            // but we still want cumulative radiation lengths of all of the volume
                    cumulative_radiation_length += effective_length/material->GetDensity()/material->GetRadlen();
                }
		break;

	    case kAllTargetVolumes:
		if (effective_position - cumulative_effective_length < effective_length) {
                    // This is the volume where our sample landed
		    found_active_volume = true;
		    effective_position_in_volume = (effective_position - cumulative_effective_length);
		    actual_position_in_volume = effective_position_in_volume/material->GetDensity();
                    // but we still want cumulative radiation lengths of part of the volume
		    cumulative_radiation_length += actual_position_in_volume/material->GetRadlen();
		} else {
                    // but we still want cumulative radiation lengths of all of the volume
		    cumulative_radiation_length += effective_length/material->GetDensity()/material->GetRadlen();
		    cumulative_effective_length += effective_length;
		}
		break;
            case kNoTargetVolume:
                // nothing to do, just avoid compilation warning
                break;
	}

	if( material->GetBaseMaterial() != nullptr ){
	    G4cerr << __FILE__ << " " << __PRETTY_FUNCTION__ << ":  The material you're using isn't" <<
		" defined in a way we can use for multiple scattering calculations" << G4endl;
	    G4cerr << "Aborting" << G4endl; 
	    exit(1);
	}

	if( found_active_volume ){
	    // For our vertex
	    vertex.fMaterial = material;
	    vertex.fRadiationLength   = cumulative_radiation_length;

	    // For our own info
	    fTravelledLength = actual_position_in_volume;
	    fRadiationLength = cumulative_radiation_length;
	    fVer    = G4ThreeVector( rasx, rasy,
		      fMotherTargetAbsolutePosition
                      + volume_relative_position - z_half_length
                      + actual_position_in_volume );

	    G4double masssum = 0.0;
	    const G4int *atomvec = material->GetAtomsVector();
	    const G4ElementVector *elvec = material->GetElementVector();
	    const G4double *fracvec = material->GetFractionVector();

	    for( unsigned int i = 0; i < elvec->size(); i++ ){
		// Not sure why AtomsVector would ever return null
		// but it does - SPR 2/5/13.
		assert( atomvec );
		masssum += (*elvec)[i]->GetA()*atomvec[i];
		double t = material->GetDensity()*actual_position_in_volume*fracvec[i];
		double A = (*elvec)[i]->GetA()*mole/g;
		double Z = (*elvec)[i]->GetZ();
		ms.push_back(std::make_tuple(t,A,Z));
	    }

	    // Effective material length for luminosity calculation
	    fEffectiveMaterialLength = (total_effective_length/effective_length) * // Sample weighting
	      effective_length * Avogadro/masssum; // material thickness
	} else {
	    const G4ElementVector *elvec = material->GetElementVector();
	    const G4double *fracvec = material->GetFractionVector();
	    for( unsigned int i = 0; i < elvec->size(); i++ ){
		double t = effective_length*fracvec[i];
		double A = (*elvec)[i]->GetA()*mole/g;
		double Z = (*elvec)[i]->GetZ();
		ms.push_back(std::make_tuple(t,A,Z));
	    }
	}
    }


    // If no volume was found
    if( !found_active_volume ){
        static G4bool alreadywarned = false;
	if( !alreadywarned ){
	    G4cerr << "WARNING: " << __PRETTY_FUNCTION__ << " line " << __LINE__ <<
	            ": Could not find sampling volume" << G4endl;
	    alreadywarned = true;
	}
	// Set default material and no radiation length
	vertex.fMaterial = fDefaultMat;
	vertex.fRadiationLength = 0.0;
    }

    // Sample multiple scattering angles
    G4double msth = 0, msph = 0;
    if (ms.size() > 0) {
	fMS.Init(fBeamEnergy, ms);
	msth = fMS.GenerateMSPlane();
	msph = fMS.GenerateMSPlane();
    }
    assert( !std::isnan(msth) && !std::isnan(msph) );
    assert( !std::isinf(msth) && !std::isinf(msph) );
    assert( msth!=-1e9 && msph!=-1e9 );

    // Sample raster angles
    G4double bmth = 0, bmph = 0;
    if(fOldRaster){
      // Gaussian distribution with mean and sigma
      bmth = G4RandGauss::shoot(fTh0, fdTh);
      bmph = G4RandGauss::shoot(fPh0, fdPh);

      if( fRasterX > 0 ){ bmth += fCorrTh*(rasx-fX0)/fRasterX/2; }
      if( fRasterY > 0 ){ bmph += fCorrPh*(rasy-fY0)/fRasterY/2; }

      // Initial direction
      fDir = G4ThreeVector(0.0, 0.0, 1.0);

      fDir.rotateY( bmth); // Positive th pushes to positive X (around Y-axis)
      fDir.rotateX(-bmph); // Positive ph pushes to positive Y (around X-axis)
    } else{
      G4ThreeVector bmVec = G4ThreeVector(fVer.x(),fVer.y(),-1*(-19810.0*mm-fVer.z())); // in mm
      fDir = G4ThreeVector(bmVec.unit());
    }

    fDir.rotateY(msth);
    fDir.rotateX(msph);


    // Sample beam energy based on radiation
    // We do this so it doesn't affect the weighting
    //
    // This can be ignored and done in a generator by itself

    G4double  Ekin = fBeamEnergy - electron_mass_c2;
    G4double  bt   = fRadiationLength * 4.0 / 3.0;

    // Euler-Mascheroni constant for gamma function
    const static G4double Euler = 0.5772157;

    G4double prob = 1.- pow(fEnergyCut/Ekin,bt) - bt/(bt+1.)*(1.- pow(fEnergyCut/Ekin,bt+1.))
	+ 0.75*bt/(2.+bt)*(1.- pow(fEnergyCut/Ekin,bt+2.));
    prob = prob/(1.- bt*Euler + bt*bt/2.*(Euler*Euler+pi*pi/6.)); /* Gamma function */

    G4double prob_sample = G4UniformRand();
    if (prob_sample <= prob) {
        G4double eloss, sample, ref;
	do {
	    sample = G4UniformRand();
	    eloss = fEnergyCut*pow(Ekin/fEnergyCut,sample);
	    G4double env = 1./eloss;
	    G4double value = 1./eloss*(1.-eloss/Ekin+0.75*pow(eloss/Ekin,2))*pow(eloss/Ekin,bt);

	    sample = G4UniformRand(); // FIXME (wdc) again?
	    ref = value/env;
	} while (sample > ref);

	fSampledEnergy = fBeamEnergy - eloss;
	assert( fSampledEnergy >= electron_mass_c2 );
    } else {
	fSampledEnergy = fBeamEnergy;
    }

    vertex.fBeamEnergy = fSampledEnergy;
    //assert( fBeamEnergy >= electron_mass_c2 );

    return vertex;
}
