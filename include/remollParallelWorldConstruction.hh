#ifndef __REMOLLPARALLELWORLDCONSTRUCTION_HH
#define __REMOLLPARALLELWORLDCONSTRUCTION_HH 1

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
class remollParallelWorldConstruction : public G4VUserParallelWorld
{
  public:
    remollParallelWorldConstruction(G4String& parallelWorldName);
    virtual ~remollParallelWorldConstruction();

  public:
    virtual void Construct();
    
    void SetParallelWorldGeomFile(const G4String& filename) {
      fFileName = filename;
    };

  private:
    G4bool fConstructed;
    G4String fFileName;
};

#endif // __REMOLLPARALLELWORLDCONSTRUCTION_HH
