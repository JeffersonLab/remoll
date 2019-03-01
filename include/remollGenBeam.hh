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

    void SetOriginXMean(double x);
    void SetOriginYMean(double y);
    void SetOriginZMean(double z);

    void SetOriginXSpread(double x);
    void SetOriginYSpread(double y);
    void SetOriginZSpread(double z);

    enum EOriginModel {
      kOriginModelFlat,
      kOriginModelGauss
    };

    EOriginModel GetOriginModelFromString(G4String model) const;
    void SetOriginXModel(G4String x);
    void SetOriginYModel(G4String y);
    void SetOriginZModel(G4String z);

    void SetRasterX(double x);
    void SetRasterY(double y);
    void SetRasterRefZ(double z);

    G4double GetSpread(G4double spread, EOriginModel model);
    G4ThreeVector GetSpread(G4ThreeVector spread,
      EOriginModel x = kOriginModelFlat,
      EOriginModel y = kOriginModelFlat,
      EOriginModel z = kOriginModelFlat);

    void SetDirectionX(double dx);
    void SetDirectionY(double dy);
    void SetDirectionZ(double dz);
    void SetDirectionPh(double ph);
    void SetDirectionTh(double th);

    void SetCorrelationX(double cx);
    void SetCorrelationY(double cy);

    void SetPolarizationX(double sx);
    void SetPolarizationY(double sy);
    void SetPolarizationZ(double sz);

    void SetPartName(G4String& name);

  private:
    void SamplePhysics(remollVertex *, remollEvent *);

    G4ThreeVector fOriginMean;
    G4ThreeVector fOriginSpread;
    EOriginModel  fOriginModelX, fOriginModelY, fOriginModelZ;
    G4ThreeVector fDirection;
    G4ThreeVector fCorrelation;
    G4ThreeVector fPolarization;

    G4ThreeVector fRaster;
    Double_t fRasterRefZ;

    G4String fParticleName;
};

#endif//__REMOLLGENBEAM_HH 
