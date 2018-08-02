#ifndef __REMOLLPARALLELCONSTRUCTION_HH
#define __REMOLLPARALLELCONSTRUCTION_HH 1

#include <G4VUserParallelWorld.hh>

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4GDMLParser;

//
/// A parallel world construction class
///
/// - void Construct()
///     creates a parallel world in the mass world and parameterized volumes
//
class remollParallelConstruction : public G4VUserParallelWorld
{
  public:
    remollParallelConstruction();
    virtual ~remollParallelConstruction();

  public:
    virtual void Construct();

    void SetParallelWorldGeomFile(const G4String& filename) {
      fFileName = filename;
    };

  private:
    G4bool fConstructed;
    G4String fFileName;
};

#endif // __REMOLLPARALLELCONSTRUCTION_HH
