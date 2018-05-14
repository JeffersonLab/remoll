#include "remollEvent.hh"
#include "remolltypes.hh"

#include <math.h>

#include "G4ParticleTable.hh"
#include "G4SystemOfUnits.hh"

remollEvent::remollEvent()
: fBeamTarget(0) {
    Reset();
}

remollEvent::~remollEvent(){
}

/**
 * Fill the event particle structure for writing to IO.
 */
std::vector<remollEventParticle_t> remollEvent::GetEventParticleIO() const {
  std::vector<remollEventParticle_t> parts;
  for (size_t idx = 0; idx < fPartType.size(); idx++) {
    remollEventParticle_t part;
    part.pid = fPartType[idx]->GetPDGEncoding();
    part.sx = fPartSpin[idx].x();
    part.sy = fPartSpin[idx].y();
    part.sz = fPartSpin[idx].z();
    part.vx = fPartPos[idx].x();
    part.vy = fPartPos[idx].y();
    part.vz = fPartPos[idx].z();
    part.px = fPartRealMom[idx].x();
    part.py = fPartRealMom[idx].y();
    part.pz = fPartRealMom[idx].z();
    part.th = fPartRealMom[idx].theta();
    part.ph = fPartRealMom[idx].phi();
    part.p  = fPartRealMom[idx].mag();
    part.tpx = fPartMom[idx].x();
    part.tpy = fPartMom[idx].y();
    part.tpz = fPartMom[idx].z();
    parts.push_back(part);
  }
  return parts;
}

/**
 * Fill the event structure for writing to IO.
 */
remollEvent_t remollEvent::GetEventIO() const {
  remollEvent_t ev;
  ev.xs = fEffXs/microbarn;
  ev.A  = fAsym/1e-9;
  ev.Am = fmAsym/1e-9;
  ev.Q2 = fQ2;
  ev.W2 = fW2;
  ev.thcom = fThCoM;
  ev.beamp = fBeamMomentum.mag();
  return ev;
}

void remollEvent::ProduceNewParticle( G4ThreeVector pos, G4ThreeVector mom, G4String name, G4ThreeVector spin ){
    fPartPos.push_back(pos);
    fPartMom.push_back(mom);
    fPartSpin.push_back(spin);
    fPartRealMom.push_back(mom);

    G4ParticleTable* particleTable = G4ParticleTable::GetParticleTable();
    G4ParticleDefinition* particle = particleTable->FindParticle(name);

    fPartType.push_back(particle);
}

void remollEvent::Reset(){
    fPartPos.clear();
    fPartMom.clear();
    fPartSpin.clear();
    fPartRealMom.clear();
    fPartType.clear();

    fBeamMomentum = G4ThreeVector(-1e9, -1e9, -1e9);
    fVertexPos    = G4ThreeVector(-1e9, -1e9, -1e9);

    fRate  = 0.0/s;
    fEffXs = -1e9*nanobarn;
    fAsym  = -1e9;

    fQ2    = -1e9*GeV*GeV;

    // Only care about for certain processes
    fW2    = -1e9*GeV*GeV;
    fThCoM = -1e9;
}

void remollEvent::UndoLastParticle(){
    fPartPos.pop_back();
    fPartMom.pop_back();
    fPartSpin.pop_back();
    fPartRealMom.pop_back();
    fPartType.pop_back();
}

G4bool remollEvent::EventIsSane(){
    // Here we check all the variables and make sure there is nothing 
    // kinematically wrong and there aren't stuff like nans and infs

    if( std::isnan(fEffXs) || std::isinf(fEffXs) || fEffXs < 0.0 ) return false;
    if( std::isnan(fAsym) || std::isinf(fAsym) || fAsym < -1.0 || fAsym > 1.0 ) return false;
    if( std::isnan(fThCoM) || std::isinf(fThCoM) ) return false;
    if( std::isnan(fQ2) || std::isinf(fQ2) ) return false;
    if( std::isnan(fW2) || std::isinf(fW2) ) return false;

    if( fPartPos.size() < 1 && fEffXs > 0.0 ){ 
	return false;
    }

    for(unsigned int i = 0; i < fPartPos.size(); i++ ){
	if( !fPartType[i] ){ return false; }

	if( std::isnan(fPartPos[i].x()) || std::isinf(fPartPos[i].x()) ) return false;
	if( std::isnan(fPartPos[i].y()) || std::isinf(fPartPos[i].y()) ) return false;
	if( std::isnan(fPartPos[i].z()) || std::isinf(fPartPos[i].z()) ) return false;

	if( std::isnan(fPartMom[i].x()) || std::isinf(fPartMom[i].x()) ) return false;
	if( std::isnan(fPartMom[i].y()) || std::isinf(fPartMom[i].y()) ) return false;
	if( std::isnan(fPartMom[i].z()) || std::isinf(fPartMom[i].z()) ) return false;
    }

    return true;
}


void remollEvent::Print(){
    G4cout << "Event " << this << " dump" << G4endl;
    G4cout << "\t" << fEffXs/nanobarn << " nb effective cross section " << G4endl;
    G4cout << "\t" << fAsym*1e6 << " ppm asymmetry" << G4endl;
    G4cout << "\t" << "Q2 = " << fQ2/GeV/GeV << " GeV2" << G4endl;
    G4cout << "\t" << "W2 = " << fW2/GeV/GeV << " GeV2" << G4endl;
    G4cout << "\t" << "th_com = " << fThCoM/deg << " deg" << G4endl;

    G4cout << "\t" << fPartPos.size() << " particles generated" << G4endl;

    unsigned int i;

    for( i = 0; i < fPartPos.size(); i++ ){
	if( !fPartType[i] ){
	    G4cout << "\tParticle type for " << i << " not defined" << G4endl;
	} else {
	    G4cout << "\t" << fPartType[i]->GetParticleName() << ":" << G4endl;
	    G4cout << "\t\tat x = (" << fPartPos[i].x()/m << ", " << fPartPos[i].y()/m << ", " << fPartPos[i].z()/m  << ") m" << G4endl;
	    G4cout << "\t\tof p = (" << fPartMom[i].x()/GeV << ", " << fPartMom[i].y()/GeV << ", " << fPartMom[i].z()/GeV  << ") GeV" << G4endl;
	    G4cout << "\t\tand s = (" << fPartSpin[i].x() << ", " << fPartSpin[i].y() << ", " << fPartSpin[i].z()  << ")" << G4endl;
	}
    }
}













