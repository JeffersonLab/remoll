#ifndef __REMOLLPARALLELCONSTRUCTION_HH
#define __REMOLLPARALLELCONSTRUCTION_HH 1

#include <G4VUserParallelWorld.hh>

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4GDMLParser;
class G4GenericMessenger;

//
/// A parallel world construction class
///
/// - void Construct()
///     creates a parallel world in the mass world and parameterized volumes
//
class remollParallelConstruction : public G4VUserParallelWorld
{
  public:
    remollParallelConstruction(const G4String& name, const G4String& gdmlfile);
    virtual ~remollParallelConstruction();

  public:
    virtual void Construct();
    virtual void ConstructSD();

    void ParseAuxiliaryVisibilityInfo();

    void SetParallelGeomFile(const G4String& gdmlfile) {
      fGDMLFile = gdmlfile;
    };

  private:
    G4bool fConstructed;
    G4String fGDMLFile;
    G4int fVerboseLevel;
    G4bool fGDMLValidate;
    G4bool fGDMLOverlapCheck;

    G4GenericMessenger* fParallelMessenger;

    G4GDMLParser* fGDMLParser;
};

#endif // __REMOLLPARALLELCONSTRUCTION_HH
