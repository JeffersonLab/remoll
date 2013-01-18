#include "remollEvent.hh"

#include "G4ParticleTable.hh"

remollEvent::remollEvent(){
    Reset();
}

remollEvent::~remollEvent(){
}

void remollEvent::ProduceNewParticle( G4ThreeVector pos, G4ThreeVector mom, G4String name ){
    fPartPos.push_back(pos);
    fPartMom.push_back(mom);
    fPartRealMom.push_back(mom);

    G4ParticleTable* particleTable = G4ParticleTable::GetParticleTable();
    G4ParticleDefinition* particle = particleTable->FindParticle(name);

    fPartType.push_back(particle);

    return;
}


void remollEvent::Reset(){
    fPartPos.clear();
    fPartMom.clear();
    fPartRealMom.clear();
    fPartType.clear();

    fBeamMomentum = G4ThreeVector(-1e9, -1e9, -1e9);
    fVertexPos    = G4ThreeVector(-1e9, -1e9, -1e9);

    fRate  = -1e9;
    fEffXs = -1e9;
    fAsym  = -1e9;
}

void remollEvent::UndoLastParticle(){
    fPartPos.pop_back();
    fPartMom.pop_back();
    fPartRealMom.pop_back();
    fPartType.pop_back();
}
