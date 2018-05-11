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

#define __WISER_EPS 1e-4
#define __WISER_N_LEG_PTS 100


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
  
  void SetEmin( double emin ){ fE_min=emin; }
  void SetEmax( double emax ){ fE_max=emax; }

  //static routine below is used as a root TF1 fit function
  static Double_t wiser_all_fit(Double_t *x, Double_t *par){
    // Primary variable x[0] is photon energy in [GeV]
    
    // Parameters are:
    // par[0]    Beam energy [GeV]
    //Double_t Ebeam = par[0];
    // par[1]    Final particle momentum [GeV/c]
    Double_t pf    = par[1];
    // par[2]    Final particle angle [rad]
    Double_t thf   = par[2];
    // par[3]    Type (as defined in wiser_all_sig)
    Int_t type  = (Int_t) par[3];
    // par[4]    Minimum invariant mass of the residual system [GeV]
    Double_t M_X   = par[4];


    const Double_t mass_p  = 0.9383;
    const Double_t mass_p2 = mass_p*mass_p;
    const Double_t mass_pi = 0.1396;
    const Double_t mass_K  = 0.4973;

    Double_t mass[] = {mass_pi, mass_pi, mass_K, mass_K, mass_p, mass_p};

    Double_t E_gamma = x[0];

    double s = mass_p2 + 2.0*E_gamma*mass_p;

    /*  Wiser's fit    pi+     pi-    k+     k-     p+       p-  */
    Double_t A1[] =  {566.,  486.,   368., 18.2,  1.33E5,  1.63E3 };
    Double_t A2[] =  {829.,  115.,   1.91, 307.,  5.69E4, -4.30E3};
    Double_t A3[] =  {1.79,  1.77,   1.91, 0.98,  1.41,    1.79 };
    Double_t A4[] =  {2.10,  2.18,   1.15, 1.83,   .72,    2.24 };
    Double_t A6 =  1.90;
    Double_t A7 = -.0117;


    // Boost to CoM
    double beta_cm = E_gamma/(E_gamma+mass_p);
    double gamma_cm = 1.0/sqrt(1.0 - beta_cm*beta_cm);

    double p_cm_z = -gamma_cm*beta_cm*sqrt(pf*pf+mass[type]*mass[type])
	            +gamma_cm*pf*cos(thf);

    double pT   = pf*sin(thf);
    double p_cm = sqrt( pT*pT + p_cm_z*p_cm_z );
    double Ef = sqrt( p_cm*p_cm + mass[type]*mass[type] );

    double p_cm_max = sqrt(s +pow(M_X*M_X - mass[type]*mass[type],2.0)/s -
	   2.0*(M_X*M_X + mass[type]*mass[type]) )/2.0;

    double X_R = p_cm/p_cm_max;

    if( X_R > 1.0 ){ return 0.0; } // Kinematically forbidden


    if( type != 4 ){ // Everything but proton
	return ( A1[type] + A2[type]/sqrt(s) )*
	    pow(1.0 - X_R + A3[type]*A3[type]/s, A4[type])/E_gamma;
    } else {
	Double_t U_MAN = fabs(2.0*mass_p2 - 2.0*mass_p*Ef);

	return ( A1[type] + A2[type]/sqrt(s) )*
	    pow(1.0 - X_R + A3[type]*A3[type]/s, A4[type])/pow(1.0 + U_MAN,A6+A7*s)/E_gamma;
    }

    return 0.0;
};

static Double_t wiser_tf3(Double_t *x, Double_t *par){
  remollGenPion pionObj;
  double Ebeam  = par[0];
  double radint = par[1];
  double radext = par[2];
  int type = (int) par[3];
  
  double pf     = x[0];
  double th     = acos(x[1]);
  double radlen = x[2]*radext + radint;
  
  return pionObj.wiser_sigma(Ebeam, pf, th, radlen, type);
}
  
protected:  
  Double_t wiser_sigma(Double_t Ebeam, Double_t pf, Double_t thf, Double_t rad_len, Int_t type);
  Double_t wiser_total_sigma(Double_t Ebeam, Double_t intrad, Double_t extrad, Int_t type);
  Pion_t fPionType;
  
private:
  void SamplePhysics(remollVertex *, remollEvent *);
  
};

#endif//__REMOLLGENPION_HH 
