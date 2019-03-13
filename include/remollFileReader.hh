#include <list>
#include <fstream>

#include "G4ThreeVector.hh"

class remollFileEvent
{
  public:
    remollFileEvent(G4ThreeVector r = {0,0,0}, G4ThreeVector p = {0,0,0}, G4double w = 0)
    : fR(r),fP(p),fW(w) { }
    ~remollFileEvent() { }

    G4ThreeVector r() const { return fR; }
    G4ThreeVector p() const { return fP; }
    G4double w() const { return fW; }

  private:
    G4ThreeVector fR, fP;
    G4double fW;
};

class remollFileReader
{
  public:
    remollFileReader(G4String filename, G4int skip = 0, G4int debuglevel = 0);
    ~remollFileReader();

    remollFileEvent GetAnEvent();

    void SetDebugLevel(int level) { fDebugLevel = level; }
    int  GetDebugLevel() const { return fDebugLevel; }

  private:
    int fDebugLevel;
    std::ifstream fInputFile;
    std::list<remollFileEvent> fEventList;
};
