#include "G4VUserTrackInformation.hh"
#include "remollVUserTrackInformation.hh"
#include "G4ios.hh"

// Guide obtained from http://geant4.slac.stanford.edu/Tips/event/1.html

G4Allocator<remollVUserTrackInformation> aTrackInformationAllocator;

remollVUserTrackInformation::remollVUserTrackInformation() {
  LastSigVertdE = 0.0;      // Initialize the initial Last Significant Parameters to zero, so that 
  LastSigVertdEDep = 0.0;   // They can be vetted against later
  LastSigVertdTh = 0.0;
  LastSigVertPos = G4ThreeVector(0.0, 0.0, 0.0);
}

remollVUserTrackInformation::remollVUserTrackInformation(const G4Track* aTrack) {
  LastSigVertdE = GetLastSigVertdE();      // Initialize the initial Last Significant Parameters to zero, so that 
  LastSigVertdEDep = GetLastSigVertdEDep();   // They can be vetted against later
  LastSigVertdTh = GetLastSigVertdTh();
  LastSigVertPos = GetLastSigVertPos();
}

remollVUserTrackInformation::remollVUserTrackInformation(const remollVUserTrackInformation* aTrackInfo) {
  LastSigVertdE = aTrackInfo->LastSigVertdE;      // Initialize the initial Last Significant Parameters to zero, so that 
  LastSigVertdEDep = aTrackInfo->LastSigVertdEDep;   // They can be vetted against later
  LastSigVertdTh = aTrackInfo->LastSigVertdTh;
  LastSigVertPos = aTrackInfo->LastSigVertPos;
}

remollVUserTrackInformation::~remollVUserTrackInformation() {;}

void remollVUserTrackInformation::Print() const
{ G4cout << "Last Significant Vertex's d Energy = " << LastSigVertdE << G4endl;
  G4cout << "Last Significant Vertex's delta Energy Deposited = " << LastSigVertdEDep << G4endl;
  G4cout << "Last Significant Vertex's delta Theta = " << LastSigVertdTh << G4endl;
  G4cout << "Last Significant Vertex's Position = " << LastSigVertPos << G4endl;
}

void remollVUserTrackInformation::SetLastSigVert(G4double dE, G4double dEDep, G4double dTh, G4ThreeVector Pos){
  LastSigVertdE = dE;
  LastSigVertdEDep = dEDep;
  LastSigVertdTh = dTh;
  LastSigVertPos = Pos;
}

G4double remollVUserTrackInformation::GetLastSigVertdE() {
  return LastSigVertdE;
}

G4double remollVUserTrackInformation::GetLastSigVertdEDep() {
  return LastSigVertdEDep;
}

G4double remollVUserTrackInformation::GetLastSigVertdTh() {
  return LastSigVertdTh;
}

G4ThreeVector remollVUserTrackInformation::GetLastSigVertPos() {
  return LastSigVertPos;
}
