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

#include "remollIO.hh"
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
#include "remollGenFlat.hh"
#include "remollGenExternal.hh"
#include "remollGenAl.hh"
#include "remollGenLUND.hh"

remollPrimaryGeneratorAction::remollPrimaryGeneratorAction()
: fEventGen(0),fPriGen(0),fParticleGun(0),fBeamTarg(0),fEvent(0),fMessenger(0)
{
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
}

remollPrimaryGeneratorAction::~remollPrimaryGeneratorAction()
{
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

    if (!fEventGen && fPriGen) {
      fPriGen->GeneratePrimaryVertex(anEvent);
      return;
    }

    if (fEventGen) {

      // Delete old primary event
      if (fEvent) {
        delete fEvent;
        fEvent = 0;
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
	G4ThreeVector pol = fEvent->fPartSpin[pidx];
	if (pol.getR()>0.01)
	  fParticleGun->SetParticlePolarization(pol);

        fParticleGun->GeneratePrimaryVertex(anEvent);
      }
    }
}
