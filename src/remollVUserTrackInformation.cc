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
  G4cout << "VUserTrackInformaion test 1 " << G4endl;
  LastSigVertdE = dE;
  G4cout << "VUserTrackInformaion test 2 " << G4endl;
  LastSigVertdEDep = dEDep;
  G4cout << "VUserTrackInformaion test 3 " << G4endl;
  LastSigVertdTh = dTh;
  G4cout << "VUserTrackInformaion test 4 " << G4endl;
  LastSigVertPos = Pos;
  G4cout << "VUserTrackInformaion test 5 " << G4endl;
}

G4double remollVUserTrackInformation::GetLastSigVertdE() {
  G4cout << "VUserTrackInformaion test 6 " << G4endl;
  return LastSigVertdE;
}

G4double remollVUserTrackInformation::GetLastSigVertdEDep() {
  G4cout << "VUserTrackInformaion test 7 " << G4endl;
  return LastSigVertdEDep;
}

G4double remollVUserTrackInformation::GetLastSigVertdTh() {
  G4cout << "VUserTrackInformaion test 8 " << G4endl;
  return LastSigVertdTh;
}

G4ThreeVector remollVUserTrackInformation::GetLastSigVertPos() {
  G4cout << "VUserTrackInformaion test 9 " << G4endl;
  return LastSigVertPos;
  G4cout << "VUserTrackInformaion test 10 " << G4endl;
}
