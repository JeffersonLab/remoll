#include "remollGenAl.hh"
#include "iomanip"
#include "iostream"
#include "CLHEP/Random/RandFlat.h"
#include "remollEvent.hh"
#include "remollVertex.hh"
#include "G4Material.hh"
#include "remolltypes.hh"
#include "G4SystemOfUnits.hh"
#include "G4PhysicalConstants.hh"

extern G4int F1F2IN09(G4int Z, G4int IA, G4double qsq,
		      G4double wsq, G4double &F1, G4double &F2);

extern G4int F1F2QE09(G4int Z, G4int IA, G4double qsq,
		      G4double wsq, G4double &F1, G4double &F2);

remollGenAl::remollGenAl(G4int physicsType) {
    type=physicsType;
    fTh_min =     0.1*deg;
    fTh_max =     5.0*deg;
    fApplyMultScatt = true;
}

remollGenAl::~remollGenAl() {
}

void remollGenAl::SamplePhysics(remollVertex *vert, remollEvent *evt) {

    G4double beamE = vert->GetBeamE(); // in MeV (it can be modified by beam loss)
    G4double th = acos(CLHEP::RandFlat::shoot(cos(fTh_max), cos(fTh_min))); // radians
    G4double ph = CLHEP::RandFlat::shoot(0.0, 2.0*pi);
    G4double eOut=0;
    G4double fWeight=0;
    G4double Q2=0;
    G4double effectiveXsection=0;
    G4double W2=0;
    G4double asym=0;

    switch (type) {
    case 0:
        G4cout<<"You should really implement the elastic Al first"<<G4endl;
// 	GenElastic(beamE);//implement something
        break;
    case 1:
	GenQuasiElastic(beamE,th, Q2, W2, effectiveXsection, fWeight, eOut);
	//FIXME do i need to put in something for the asymmetry?
        break;
    case 2:
        GenInelastic(beamE,th,Q2,W2,effectiveXsection,fWeight,eOut,asym);
        break;
    default:
        G4cerr<<"Unknown aluminum event type"<<G4endl;
    }

    if( vert->GetMaterial()->GetNumberOfElements() != 1 ) {
        G4cerr << __FILE__ << " line " << __LINE__ <<
               "  : Error!  Some lazy programmer didn't account for complex materials in the moller process!" << G4endl;
        exit(1);
    }

    evt->SetThCoM(th);  //wasn't in the GenpInelastic ... should it be there FIXME
    evt->SetEffCrossSection(effectiveXsection);
    evt->SetQ2( Q2 );
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

    asym=Q2*0.8e-4/GeV/GeV;
    fWeight = xsect*sin(theta);
    effectiveXsection=xsect;
}




void remollGenAl::GenQuasiElastic(G4double beamE,G4double theta,
				  G4double &Q2,G4double &W2,G4double &effectiveXsection,
				  G4double &fWeight,G4double &eOut) {
  
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
  G4double Nu = beamE - eOut;
  Q2 = 4.0*beamE*eOut*STH*STH;
  W2 = proton_mass_c2*proton_mass_c2 + 2.0*proton_mass_c2*Nu - Q2;
  
  // Mott scattering
  G4double MOTT = pow((0.00072/(beamE/GeV)*CTH/STH/STH),2);
  MOTT = MOTT*1.0E4; // Units: ub/sr/GeV
  
  G4int A=27;
  G4int Z=13;
  F1F2QE09(Z, A, Q2/GeV/GeV, W2/GeV/GeV, F1, F2);
  
  w1 = F1/proton_mass_c2;
  w2 = F2/Nu;
  
  xsect = MOTT*(w2 + 2.0*T2THE*w1)*(beamE/GeV - electron_mass_c2/GeV);
  
  // In some cases a negative F2 is returned giving a negative cross section
  if (xsect <= 0) xsect = 0.0;
  
  fWeight = xsect*sin(theta);
  effectiveXsection = xsect;
}
