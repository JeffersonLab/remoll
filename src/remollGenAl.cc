#include "remollGenAl.hh"

#include "Randomize.hh"

#include <iomanip>
#include <iostream>

#include "remollEvent.hh"
#include "remollVertex.hh"
#include "remolltypes.hh"

#include "G4Material.hh"
#include "G4SystemOfUnits.hh"
#include "G4PhysicalConstants.hh"

extern G4int F1F2IN09(G4int Z, G4int IA, G4double qsq,
		      G4double wsq, G4double &F1, G4double &F2);

extern G4int F1F2QE09(G4int Z, G4int IA, G4double qsq,
		      G4double wsq, G4double &F1, G4double &F2);

std::map<G4int,G4String> remollGenAl::fNamesMap = remollGenAl::CreateNamesMap();

std::map<G4int,G4String> remollGenAl::CreateNamesMap() {
  std::map<G4int,G4String> names;
  names[0] = "elasticAl";
  names[1] = "quasielasticAl";
  names[2] = "inelasticAl";
  return names;
}

remollGenAl::remollGenAl(G4int physicsType)
: remollVEventGen(fNamesMap[physicsType]) {
    type = physicsType;

    fTh_min =     0.1*deg;
    fTh_max =     5.0*deg;
    fApplyMultScatt = true;
}

remollGenAl::~remollGenAl() {
}

void remollGenAl::SamplePhysics(remollVertex *vert, remollEvent *evt) {

    G4double beamE = vert->GetBeamEnergy(); // in MeV (it can be modified by beam loss)
    G4double th = acos(G4RandFlat::shoot(cos(fTh_max), cos(fTh_min))); // radians

    /////////////////////////////////////////
    // sample with 1.0/(1-cos)^2
    ////////////////////////////////////////
/*
    double cthmin = cos(fTh_min);
    double cthmax = cos(fTh_max);

    double icth_b = 1.0/(1.0-cthmax);
    double icth_a = 1.0/(1.0-cthmin);

    double sampv = 1.0/G4RandFlat::shoot(icth_b, icth_a);

    assert( -1.0 < sampv && sampv < 1.0 );

    double th = acos(1.0-sampv);
    // Value to reweight cross section by to account for non-uniform
    // sampling
    double samp_fact = sampv*sampv*(icth_a-icth_b)/(cthmin-cthmax);

    G4double phaseSpaceFactor = 2.0*pi*(cos(fTh_min) - cos(fTh_max))*samp_fact;
*/

    G4double phaseSpaceFactor = (fPh_max - fPh_min) * (cos(fTh_min) - cos(fTh_max));
    G4double ph = G4RandFlat::shoot(fPh_min, fPh_max);
    G4double eOut=0;
    G4double fWeight=0;
    G4double Q2=0;
    G4double effectiveXsection=0;
    G4double W2=0;
    G4double asym=0;

    switch (type) {
    case 0:
      GenElastic(beamE,th,Q2,W2,effectiveXsection,fWeight,eOut,asym);
      break;
    case 1:
      GenQuasiElastic(beamE,th,Q2,W2,effectiveXsection,fWeight,eOut,asym);
      break;
    case 2:
      GenInelastic(beamE,th,Q2,W2,effectiveXsection,fWeight,eOut,asym);
      break;
    default:
      G4cerr<<"Unknown aluminum event type"<<G4endl;
      exit(1);
    }
    
    if( vert->GetMaterial()->GetNumberOfElements() != 1 ) {
        G4cerr << __FILE__ << " line " << __LINE__ <<
               "  : Error!  Some lazy programmer didn't account for complex materials in the moller process!" << G4endl;
        exit(1);
    }

    evt->SetThCoM(th);
    evt->SetEffCrossSection(phaseSpaceFactor*effectiveXsection*microbarn);
    evt->SetQ2( Q2 ); //MeV^2
    evt->SetAsymmetry(asym);
    evt->SetW2( W2 );
    evt->ProduceNewParticle( G4ThreeVector(0.0, 0.0, 0.0),
                             G4ThreeVector(eOut*sin(th)*cos(ph), eOut*sin(th)*sin(ph), eOut*cos(th) ),
                             "e-" );

    return;
}

void remollGenAl::GenInelastic(G4double beamE,G4double theta,
                               G4double &Q2,G4double &W2,G4double &effectiveXsection,
                               G4double &fWeight,G4double &eOut, G4double &asym) {

    G4double F1 = 0.0;
    G4double F2 = 0.0;
    G4double W1 = 0.0;
    G4double xsect = 0.0;

    G4double CTH,STH,T2THE,Nu;
    
    do{
      eOut   =  electron_mass_c2 + G4UniformRand()*(beamE - electron_mass_c2);// Generate flat energy distribution of outgoing electron
      CTH    = cos(theta/2.0);
      STH    = sin(theta/2.0);
      T2THE  = STH*STH/CTH/CTH;
      Nu     = beamE - eOut;
      Q2     = 4.0*beamE*eOut*STH*STH;
      W2     = pow(proton_mass_c2,2) + 2.0*proton_mass_c2*Nu - Q2;
    }while(W2/GeV/GeV<0 || W2/GeV/GeV>9 || Q2/GeV/GeV<0.0 || Q2/GeV/GeV>10); //this is because F1F2IN09 won't work for W>3 and Q2>10
    
    // Mott scattering
    G4double MOTT = pow((0.00072/(beamE/GeV)*CTH/STH/STH),2);
    MOTT = MOTT*1.0E4; // Units: ub/sr/GeV

    G4int A=27;
    G4int Z=13;
    G4int bad=F1F2IN09(Z, A, Q2/GeV/GeV, W2/GeV/GeV, F1, F2);  
    if(bad){
      G4cerr << "ERROR: " << __FILE__ << " line " << __LINE__ << G4endl;	
      G4cerr << "  result was (-1 = Q2,W2 out of range in resmodd | -2 = A<3 | 1 inf/nan F1/F2) : " << bad <<G4endl;
      G4cerr << "     Q2 W2 A "<<Q2/GeV/GeV<<" "<<W2/GeV/GeV<<" "<<A<< G4endl;
      G4cerr << "     STH eOut beamE "<<STH<<" "<<eOut<<" "<<beamE<<G4endl;
      asym=0.99999;
      effectiveXsection=0;
      exit(1);
    }
    W1 = F1/proton_mass_c2;
 
    xsect = MOTT*(F2/(Nu/GeV) + 2.0*T2THE*(W1*GeV))*(beamE/GeV - electron_mass_c2/GeV);

    if (xsect < 0 || std::isnan(xsect) ) {
        G4cerr << "ERROR: " << __FILE__ << " line " << __LINE__ << G4endl;	
        G4cerr<<"Inelatic xsection problem: "<<xsect<<" "<<F1<<" "<<F2
              <<" "<<theta<<" "<<Q2/GeV/GeV<<" "<<W2/GeV/GeV<<" "<<G4endl;
        asym=0.99999;
        effectiveXsection=0;
        exit(1);
    }
//---Yuxiang Zhao, R-L asymmetry is negative
    asym=-1.0*Q2*0.8e-4/GeV/GeV;
    fWeight = xsect*sin(theta);
    effectiveXsection=xsect;
}




void remollGenAl::GenQuasiElastic(G4double beamE,G4double theta,
				  G4double &Q2,G4double &W2,G4double &effectiveXsection,
				  G4double &fWeight,G4double &eOut,G4double &asym) {  
  G4double F1 = 0.0;
  G4double F2 = 0.0;
  G4double w1 = 0.0;
  G4double w2 = 0.0;  
  G4double xsect = 0.0;
  
  // Generate flat energy distribution of outgoing electron
  eOut =  electron_mass_c2 + G4UniformRand()*(beamE - electron_mass_c2);
  
  G4double CTH = cos(theta/2.0);
  G4double STH = sin(theta/2.0);
  G4double T2THE = STH*STH/CTH/CTH;
  G4double Nu = beamE - eOut;//[MeV]
  Q2 = 4.0*beamE*eOut*STH*STH;
  W2 = proton_mass_c2*proton_mass_c2 + 2.0*proton_mass_c2*Nu - Q2;
  
  // Mott scattering
  G4double MOTT = pow((0.00072/(beamE/GeV)*CTH/STH/STH),2);
  MOTT = MOTT*1.0E4; // Units: ub/sr/GeV
  
  G4int A=27;
  G4int Z=13;
  F1F2QE09(Z, A, Q2/GeV/GeV, W2/GeV/GeV, F1, F2);
  
  w1 = F1/(proton_mass_c2/GeV);
  w2 = F2/(Nu/GeV);
  
  xsect = MOTT*(w2 + 2.0*T2THE*w1)*(beamE/GeV - electron_mass_c2/GeV);
  
  // In some cases a negative F2 is returned giving a negative cross section
  if (xsect <= 0) xsect = 0.0;

  fWeight = xsect*sin(theta);
  effectiveXsection = xsect;

  ///~~~ Aymmetry calculation
  asym= -GF/(4.*pi*fine_structure_const*sqrt(2.)) * Q2 * (QWp);
}

void remollGenAl::GenElastic(G4double beamE,G4double theta,
			     G4double &Q2,G4double &W2,G4double &effectiveXsection,
			     G4double &fWeight,G4double &eOut,G4double &asym) {
  ///~~~~ X-section calculation
  const G4double Z = 13.0;
  const G4double A = 27.0;
  const G4double M = proton_mass_c2*A;
  const G4double CTH = cos(theta/2.0);
  const G4double STH = sin(theta/2.0);
  const G4double ETA = 1.0+2.0*beamE*STH*STH/M;
  
  eOut = beamE/ETA;   
  Q2 = 4*beamE*eOut*STH*STH;//[MeV^2]
  W2 = M*M;//[MeV^2]
  
  //harmonic oscillator well parameter a0 ~1.76 fm 
  const G4double a = 2.98; //[fm]
  const G4double ap = sqrt(0.427);//[fm] 
  const G4double a0 = sqrt((a*a-1.5*ap*ap)/(3.5-10/Z-1.5/A)); 
  const G4double q2 = Q2/GeV/GeV*(1.0/0.197)*(1.0/0.197);//convert MeV^2 into fm^(-2)
  const G4double x = (1.0/4.0)*q2*a0*a0;
  
  const G4double F0 = (1.0/Z)*( Z-4.0/3.0*(Z-5.0)*x+4.0/15.0*(Z-8.0)*x*x)*exp(-x);
  const G4double F2 = (1.0-2.0/7.0*x)*exp(-x);
  const G4double Q = 14.6;  //[fm^(-2)]
  G4double Fe = sqrt( F0*F0+(7.0/450.0)*q2*q2*(Q*Q/Z/Z)*F2*F2 );
  Fe=Fe*exp(-0.25*q2*ap*ap); //correction for finite proton size
  Fe=Fe*exp(x/A); //correction for center-of-well motion
  const G4double F_2 = Fe*Fe;

  G4double SigmaMott = pow(((0.72/beamE)*CTH/(STH*STH)),2)/(1+2*beamE/M*STH*STH)*10000 ;
  SigmaMott *= (Z*Z);
  effectiveXsection = SigmaMott*F_2;

  G4double functionOfTheta = log (STH*STH) * log (CTH*CTH);
  G4double deltaSchwinger = (-2.0*fine_structure_const/pi)*
    ((log(beamE/15.0) - 13.0/12.0) * (log(Q2/(electron_mass_c2*electron_mass_c2)) - 1.0) + 17.0/36.0 + functionOfTheta/2.0);
  effectiveXsection *=(1. + deltaSchwinger);
  
  fWeight = effectiveXsection*sin(theta);  
  
  ///~~~ Aymmetry calculation
  asym= -GF/(4.*pi*fine_structure_const*sqrt(2.)) * Q2 * (QWp + QWn*(A-Z)/Z);
}
