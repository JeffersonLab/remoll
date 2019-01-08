#ifndef __REMOLLGENBEAM_HH 
#define __REMOLLGENBEAM_HH 
/*!
 * Boring beam event generator
 *
 * Seamus Riordan
 * July 9, 2013
 *
*/

#include "remollVEventGen.hh"

class remollGenBeam : public remollVEventGen {
  public:
    remollGenBeam();
    virtual ~remollGenBeam();

    void SetOriginX(double x);
    void SetOriginY(double y);
    void SetOriginZ(double z);

    void SetRasterX(double RASx);
    void SetRasterY(double RASy);

    void SetDirection(G4ThreeVector direction);
    void SetDirectionX(double dx);
    void SetDirectionY(double dy);
    void SetDirectionZ(double dz);
    void SetDirectionPh(double ph);
    void SetDirectionTh(double th);

    void SetPolarization(G4ThreeVector polarization);
    void SetPolarizationX(double sx);
    void SetPolarizationY(double sy);
    void SetPolarizationZ(double sz);

    void SetPartName(G4String& name);

  private:
    void SamplePhysics(remollVertex *, remollEvent *);

    G4ThreeVector fOrigin;
    G4ThreeVector fDirection;
    G4ThreeVector fPolarization;

    double fXras;
    double fYras;

    G4String fParticleName;
};

#endif//__REMOLLGENBEAM_HH 
