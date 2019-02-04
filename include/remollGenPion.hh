#ifndef __REMOLLGENPION_HH
#define __REMOLLGENPION_HH
/*!
 * Pion event generator
 * from Wiser parameterization
 *
 * Seamus Riordan
 * August 16, 2013
 *
*/

#include "remollVEventGen.hh"

class remollGenPion : public remollVEventGen {
  public:
    remollGenPion();
    virtual ~remollGenPion();

    enum Pion_t {kPiPlus, kPiMinus, kPi0};

    void SetPionTypeByString_Deprecated(G4String& t) {
      G4cerr << "The command `/remoll/piontype` is deprecated." << G4endl;
      G4cerr << "Use instead `/remoll/evgen/pion/settype`." << G4endl;
      SetPionTypeByString(t);
    }
    void SetPionTypeByString(G4String& t) {
      if (t == "pi-") SetPionType(kPiMinus);
      else if (t == "pi+") SetPionType(kPiPlus);
      else if (t == "pi0") SetPionType(kPi0);
      else G4cerr << "Recognized pion types: pi-, pi+ and pi0." << G4endl;
    }
    void SetPionType(Pion_t t) { fPionType = t; }

  protected:
    Pion_t fPionType;

  private:
    void SamplePhysics(remollVertex *, remollEvent *);

    double wiser_sigma(double Ebeam, double pf, double thf, double rad_len, int type);
    static double wiserfit(double *x, double *par);
};

#endif //__REMOLLGENPION_HH
