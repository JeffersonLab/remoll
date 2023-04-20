#include "remollPrimaryGeneratorAction.hh"

#include "G4Event.hh"
#include "G4ParticleGun.hh"
#include "G4ParticleTable.hh"
#include "G4ParticleDefinition.hh"
#include "G4Version.hh"

#include "remollHEPEvtInterface.hh"
#ifdef G4LIB_USE_HEPMC
#include "HepMCG4AsciiInterface.hh"
#ifdef G4LIB_USE_PYTHIA
#include "HepMCG4PythiaInterface.hh"
#endif
#endif

#include "remollBeamTarget.hh"
#include "remollVEventGen.hh"
#include "remollEvent.hh"
#include "remollRun.hh"
#include "remollRunData.hh"
#include "remolltypes.hh"
#include "globals.hh"

#include "remollGenMoller.hh"
#include "remollGenpElastic.hh"
#include "remollGenpInelastic.hh"
#include "remollGenPion.hh"
#include "remollGenBeam.hh"
#include "remollGenC12.hh"
#include "remollGenFlat.hh"
#include "remollGenExternal.hh"
#include "remollGenAl.hh"
#include "remollGenLUND.hh"
#include "remollGenHyperon.hh"

#include <memory>

remollPrimaryGeneratorAction::remollPrimaryGeneratorAction()
  : fEventGen(0),fPriGen(0),fParticleGun(0),fEvent(0),fRateCopy(0),fEffCrossSection(0)
{
    static bool has_been_warned = false;
    if (! has_been_warned) {
      G4cout << "remoll: All possible event generators are instantiated every time." << G4endl;
      G4cout << "remoll: This means some will not find necessary input files or" << G4endl;
      G4cout << "remoll: print other information in the next few lines." << G4endl;
      has_been_warned = true;
    }

    // Populate map with all possible event generators
    fEvGenMap["moller"] = std::make_shared<remollGenMoller>();
    fEvGenMap["elastic"] = std::make_shared<remollGenpElastic>();
    fEvGenMap["inelastic"] = std::make_shared<remollGenpInelastic>();
    fEvGenMap["pion"] = std::make_shared<remollGenPion>();
    fEvGenMap["beam"] = std::make_shared<remollGenBeam>();
    fEvGenMap["flat"] = std::make_shared<remollGenFlat>();
    fEvGenMap["elasticAl"] = std::make_shared<remollGenAl>(0);
    fEvGenMap["quasielasticAl"] = std::make_shared<remollGenAl>(1);
    fEvGenMap["inelasticAl"] = std::make_shared<remollGenAl>(2);
    fEvGenMap["external"] = std::make_shared<remollGenExternal>();
    fEvGenMap["pion_LUND"] = std::make_shared<remollGenLUND>();
    fEvGenMap["elasticC12"] = std::make_shared<remollGenC12>(0);
    fEvGenMap["quasielasticC12"] = std::make_shared<remollGenC12>(1);
    fEvGenMap["inelasticC12"] = std::make_shared<remollGenC12>(2);
    fEvGenMap["hyperon"] = std::make_shared<remollGenHyperon>();

    // Populate map with all possible primary generators
    fPriGenMap["particlegun"] = std::make_shared<G4ParticleGun>();
    fPriGenMap["HEPEvt"] = std::make_shared<remollHEPEvtInterface>();
    #ifdef G4LIB_USE_HEPMC
    fPriGenMap["hepmcAscii"] = std::make_shared<HepMCG4AsciiInterface>();
    #ifdef G4LIB_USE_PYTHIA
    fPriGenMap["hepmcPythia"] = std::make_shared<HepMCG4PythiaInterface>();
    #endif
    #endif

    // Default generator
    G4String default_generator = "moller";
    SetGenerator(default_generator);

    // Create event generator messenger
    fEvGenMessenger.DeclareMethod("set",&remollPrimaryGeneratorAction::SetGenerator,"Select physics generator");
    fEvGenMessenger.DeclarePropertyWithUnit("sigma","picobarn",fEffCrossSection,"Set effective cross section");
    fEvGenMessenger.DeclareProperty("copyRate",fRateCopy,"ExtGen: copy rate from previous sim");
}

remollPrimaryGeneratorAction::~remollPrimaryGeneratorAction()
{
}

void remollPrimaryGeneratorAction::SetGenerator(G4String& genname)
{
    // Set generator to null
    fEventGen = 0;
    fPriGen = 0;

    // Find event generator
    auto evgen = fEvGenMap.find(genname);
    if (evgen != fEvGenMap.end()) {
      G4cout << "Setting generator to " << genname << G4endl;
      fPriGen = 0;
      fPriGenName = "";
      fEventGen = evgen->second;
      fEventGenName = evgen->first;
      fParticleGun = fEventGen->GetParticleGun();
    }

    // Find primary generator
    auto prigen = fPriGenMap.find(genname);
    if (prigen != fPriGenMap.end()) {
      G4cout << "Setting generator to " << genname << G4endl;
      fPriGen = prigen->second;
      fPriGenName = prigen->first;
      fEventGen = 0;
      fEventGenName = "";
      fParticleGun = 0;
    }

    // No generator found
    if (!fEventGen && !fPriGen) {
      G4cerr << __FILE__ << " line " << __LINE__ << " - ERROR generator " << genname << " invalid" << G4endl;
      exit(1);
    }

    // Set the beam target
    if (fEventGen) {
      fEventGen->SetBeamTarget(&fBeamTarg);
    }
}

void remollPrimaryGeneratorAction::GeneratePrimaries(G4Event* anEvent)
{
    if (!fEventGen && !fPriGen) {
      G4cerr << __FILE__ << " line " << __LINE__ << " - No event generator found." << G4endl;
      exit(1);
    }

    // Delete old primary event
    if (fEvent != nullptr) {
      delete fEvent;
      fEvent = 0;
    }

    // 1. Using primary generator interface
    if (!fEventGen && fPriGen) {
      fPriGen->GeneratePrimaryVertex(anEvent);
      fEvent = new remollEvent(anEvent);
      fEvent->SetEffCrossSection(fEffCrossSection);
    }

    // 2. Using event generator interface
    if (fEventGen && !fPriGen) {

      // Helper function
      auto contains = [](const G4String& lhs, const G4String& rhs) {
        #if G4VERSION_NUMBER < 1100
          return lhs.contains(rhs);
        #else
          return G4StrUtil::contains(lhs, rhs);
        #endif
      };

      // Set beam polarization
      const G4String fBeamPol = fEventGen->GetBeamPolarization();
      G4ThreeVector cross(0,0,2);
      if (fBeamPol == "0") cross = G4ThreeVector(0,0,0);
      else {
        if     (contains(fBeamPol, "V")) cross = G4ThreeVector(1,0,0);
        else if(contains(fBeamPol, "H")) cross = G4ThreeVector(0,1,0);
        if (contains(fBeamPol, "-")) cross *= -1;
      }

      // Create new primary event
      fEvent = fEventGen->GenerateEvent();
      for (unsigned int pidx = 0; pidx < fEvent->fPartType.size(); pidx++) {

        double p = fEvent->fPartRealMom[pidx].mag();
        double m = fEvent->fPartType[pidx]->GetPDGMass();
        double kinE = sqrt(p*p + m*m) - m;

        fParticleGun->SetParticleDefinition(fEvent->fPartType[pidx]);
        fParticleGun->SetParticleEnergy(kinE);
        fParticleGun->SetParticlePosition(fEvent->fPartPos[pidx]);
        fParticleGun->SetParticleMomentumDirection(fEvent->fPartRealMom[pidx].unit());

        G4ThreeVector pol(0,0,0);
        if (pidx == 0) {
          if (cross.mag() !=0) {
            if (cross.mag() == 1) //transverse polarization
              pol = G4ThreeVector( (fEvent->fPartRealMom[0].unit()).cross(cross));
            else if (contains(fBeamPol, "+") ) //positive helicity
              pol = fEvent->fPartRealMom[0].unit();
            else //negative helicity
              pol = - fEvent->fPartRealMom[0].unit();
          }
        }
        fParticleGun->SetParticlePolarization(pol);

        fParticleGun->GeneratePrimaryVertex(anEvent);
      }
    }

    // Finally set the cross section and rate

    // Get number of thrown events
    G4double nthrown = remollRun::GetRunData()->GetNthrown();


    // Calculate rate
    SamplingType_t sampling_type = fEventGen->GetSamplingType();
    if (fEvent->fRate == 0) { // If the rate is set to 0 then calculate it using the cross section
        fEvent->fRate  = fEvent->fEffXs * fBeamTarg.GetEffLumin(sampling_type) / nthrown;

    } else if(!fRateCopy){ // For LUND - calculate rate and cross section
        fEvent->fEffXs = fEvent->fRate * nthrown / fBeamTarg.GetEffLumin(sampling_type);
        fEvent->fRate  = fEvent->fRate / nthrown;
    }

}
