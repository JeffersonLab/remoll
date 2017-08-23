#include "remollVertex.hh"

#include "G4Material.hh"
#include "G4SystemOfUnits.hh"

remollVertex::remollVertex(){
    // Some default material
    fMaterial = NULL;
    fBeamEnergy = 0.0*GeV;
    fRadiationLength = 0.0;
}

remollVertex::~remollVertex(){
}
