#include <list>
#include <fstream>

#include "G4ThreeVector.hh"

class remollFileEvent
{
  public:
    remollFileEvent();
    remollFileEvent(G4ThreeVector r, G4ThreeVector p, G4double w)
    : fR(r),fP(p),fW(w) { }
    ~remollFileEvent() { }

    G4ThreeVector GetR() const { return fR; }
    G4ThreeVector GetP() const { return fP; }
    G4double GetW() const { return fW; }

  private:
    G4ThreeVector fR, fP;
    G4double fW;
};

class remollFileReader
{
  public:
    remollFileReader(G4String filename, G4int skip = 0);
    ~remollFileReader();

    remollFileEvent GetAnEvent();

  private:
    std::ifstream fInputFile;
    std::list<remollFileEvent> fEventList;
};
