#include "remollVertex.hh"

#include "G4Material.hh"
#include "G4SystemOfUnits.hh"

remollVertex::remollVertex(){
    // Some default material
    fMaterial = NULL;
    fBeamE = 0.0*GeV;
    fRadLen = 0.0;
}

remollVertex::~remollVertex(){
}
