#include "remollPrimaryGeneratorAction.hh"

#include "G4Event.hh"
#include "G4ParticleGun.hh"
#include "G4ParticleTable.hh"
#include "G4ParticleDefinition.hh"
#include "G4GenericMessenger.hh"

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
#include "remollGenTF1.hh"
#include "remollGen12CElastic.hh"
#include "remollGenFlat.hh"
#include "remollGenExternal.hh"
#include "remollGenAl.hh"
#include "remollGenLUND.hh"
#include "remollGenHyperon.hh"

remollPrimaryGeneratorAction::remollPrimaryGeneratorAction()
  : fEventGen(0),fPriGen(0),fParticleGun(0),fBeamTarg(0),fEvent(0),fMessenger(0),fRateCopy(0),fEffCrossSection(0)
{
    static bool has_been_warned = false;
    if (! has_been_warned) {
      G4cout << "remoll: All possible event generators are instantiated every time." << G4endl;
      G4cout << "remoll: This means some will not find necessary input files or" << G4endl;
      G4cout << "remoll: print other information in the next few lines." << G4endl;
      has_been_warned = true;
    }

    // Populate map with all possible event generators
    fEvGenMap["moller"] = new remollGenMoller();
    fEvGenMap["elastic"] = new remollGenpElastic();
    fEvGenMap["inelastic"] = new remollGenpInelastic();
    fEvGenMap["pion"] = new remollGenPion();
    fEvGenMap["beam"] = new remollGenBeam();
    fEvGenMap["flat"] = new remollGenFlat();
    fEvGenMap["TF1"] = new remollGenTF1();
    fEvGenMap["elasticAl"] = new remollGenAl(0);
    fEvGenMap["quasielasticAl"] = new remollGenAl(1);
    fEvGenMap["inelasticAl"] = new remollGenAl(2);
    fEvGenMap["external"] = new remollGenExternal();
    fEvGenMap["pion_LUND"] = new remollGenLUND();
    fEvGenMap["carbon"] = new remollGen12CElastic();
    fEvGenMap["hyperon"] = new remollGenHyperon();

    // Populate map with all possible primary generators
    fPriGenMap["particlegun"] = new G4ParticleGun();
    fPriGenMap["HEPEvt"] = new remollHEPEvtInterface();
    #ifdef G4LIB_USE_HEPMC
    fPriGenMap["hepmcAscii"] = new HepMCG4AsciiInterface();
    #ifdef G4LIB_USE_PYTHIA
    fPriGenMap["hepmcPythia"] = new HepMCG4PythiaInterface()
    #endif
    #endif

    // Create beam target
    fBeamTarg = new remollBeamTarget();

    // Default generator
    G4String default_generator = "moller";
    SetGenerator(default_generator);

    // Create event generator messenger
    fEvGenMessenger = new G4GenericMessenger(this,"/remoll/evgen/","Remoll event generator properties");
    fEvGenMessenger->DeclareMethod("set",&remollPrimaryGeneratorAction::SetGenerator,"Select physics generator");
    fEvGenMessenger->DeclarePropertyWithUnit("sigma","picobarn",fEffCrossSection,"Set effective cross section");
    fEvGenMessenger->DeclareProperty("copyRate",fRateCopy,"ExtGen: copy rate from previous sim");
}

remollPrimaryGeneratorAction::~remollPrimaryGeneratorAction()
{
    fEvGenMap.clear();
    fPriGenMap.clear();
    if (fEvGenMessenger) delete fEvGenMessenger;
    if (fMessenger) delete fMessenger;
    if (fBeamTarg)  delete fBeamTarg;
    if (fEventGen)  delete fEventGen;
}

void remollPrimaryGeneratorAction::SetGenerator(G4String& genname)
{
    // Set generator to null
    fEventGen = 0;
    fPriGen = 0;

    // Find event generator
    std::map<G4String,remollVEventGen*>::iterator evgen = fEvGenMap.find(genname);
    if (evgen != fEvGenMap.end()) {
      G4cout << "Setting generator to " << genname << G4endl;
      fPriGen = 0;
      fPriGenName = "";
      fEventGen = evgen->second;
      fEventGenName = evgen->first;
      fParticleGun = fEventGen->GetParticleGun();
    }

    // Find primary generator
    std::map<G4String,G4VPrimaryGenerator*>::iterator prigen = fPriGenMap.find(genname);
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
      fEventGen->SetBeamTarget(fBeamTarg);
    }

    remollRun::GetRunData()->SetGenName(genname.data());
}

void remollPrimaryGeneratorAction::GeneratePrimaries(G4Event* anEvent)
{
    if (!fEventGen && !fPriGen) {
      G4cerr << __FILE__ << " line " << __LINE__ << " - No event generator found." << G4endl;
      exit(1);
    }

    // Delete old primary event
    if (fEvent) {
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

      // Set beam polarization
      const G4String fBeamPol = fEventGen->GetBeamPolarization();
      G4ThreeVector cross(0,0,2);
      if (fBeamPol == "0") cross = G4ThreeVector(0,0,0);
      else {
        if (fBeamPol.contains('V')) cross = G4ThreeVector(1,0,0);
        else if(fBeamPol.contains('H')) cross = G4ThreeVector(0,1,0);
        if (fBeamPol.contains('-')) cross *= -1;
      }

      // Create new primary event
      fEvent = fEventGen->GenerateEvent();
      for (unsigned int pidx = 0; pidx < fEvent->fPartType.size(); pidx++) {

        double p = fEvent->fPartMom[pidx].mag();
        double m = fEvent->fPartType[pidx]->GetPDGMass();
        double kinE = sqrt(p*p + m*m) - m;

        fParticleGun->SetParticleDefinition(fEvent->fPartType[pidx]);
        fParticleGun->SetParticleEnergy(kinE);
        fParticleGun->SetParticlePosition(fEvent->fPartPos[pidx]);
        fParticleGun->SetParticleMomentumDirection(fEvent->fPartMom[pidx].unit());

        G4ThreeVector pol(0,0,0);
        if (pidx == 0) {
          if (cross.mag() !=0) {
            if (cross.mag() == 1) //transverse polarization
              pol = G4ThreeVector( (fEvent->fPartMom[0].unit()).cross(cross));
            else if (fBeamPol.contains("+") ) //positive helicity
              pol = fEvent->fPartMom[0].unit();
            else //negative helicity
              pol = - fEvent->fPartMom[0].unit();
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
    if (fEvent->fRate == 0) { // If the rate is set to 0 then calculate it using the cross section
        fEvent->fRate  = fEvent->fEffXs * fBeamTarg->GetEffLumin() / nthrown;

    } else if(!fRateCopy){ // For LUND - calculate rate and cross section
        fEvent->fEffXs = fEvent->fRate * nthrown / fBeamTarg->GetEffLumin();
        fEvent->fRate  = fEvent->fRate / nthrown;
    }

}
