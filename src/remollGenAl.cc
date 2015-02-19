#include "remollGenAl.hh"
#include "iomanip"
#include "iostream"
#include "CLHEP/Random/RandFlat.h"
#include "remollEvent.hh"
#include "remollVertex.hh"
#include "G4Material.hh"
#include "remolltypes.hh"

extern int F1F2IN09(int Z, int IA, double qsq,
                     double wsq, double &F1, double &F2);

remollGenAl::remollGenAl(int physicsType) {
    type=physicsType;
    fTh_min =     0.1*deg;
    fTh_max =     5.0*deg;
    fApplyMultScatt = true;
}

remollGenAl::~remollGenAl() {
}

void remollGenAl::SamplePhysics(remollVertex *vert, remollEvent *evt) {


    double beamE = vert->GetBeamE(); // in GeV
    double th = acos(CLHEP::RandFlat::shoot(cos(fTh_max), cos(fTh_min))); // radians
    double ph = CLHEP::RandFlat::shoot(0.0, 2.0*pi);
    double eOut=0;
    double fWeight=0;
    double Q2=0;
    double effectiveXsection=0;
    double W2=0;
    double asym=0;

    switch (type) {
    case 0:
        G4cout<<"You should really do something about the elastic"<<G4endl;
// 	GenElastic(beamE);//implement something
        break;
    case 1:
        G4cout<<"You should really do something about the quasi elastic"<<G4endl;
// 	GenQuasiElastic(beamE);//implement something
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

    evt->SetEffCrossSection(effectiveXsection);
    evt->SetQ2( Q2 );
    evt->SetAsymmetry(asym);
    evt->SetW2( W2 );
    evt->ProduceNewParticle( G4ThreeVector(0.0, 0.0, 0.0),
                             G4ThreeVector(eOut*sin(th)*cos(ph), eOut*sin(th)*sin(ph), eOut*cos(th) ),
                             "e-" );

    return;
}

void remollGenAl::GenInelastic(double beamE,double th,
                               double &Q2,double &W2,double &effectiveXsection,
                               double &fWeight,double &eOut, double &asym) {

    double F1 = 0.0;
    double F2 = 0.0;
    double W1 = 0.0;
    double xsect = 0.0;

    double CTH,STH,T2THE,Nu;
    
    do{
      eOut   =  electron_mass_c2 + G4UniformRand()*(beamE - electron_mass_c2);// Generate flat energy distribution of outgoing electron
      CTH    = cos(th/2.0);
      STH    = sin(th/2.0);
      T2THE  = STH*STH/CTH/CTH;
      Nu     = beamE - eOut;
      Q2     = 4.0*beamE*eOut*STH*STH;
      W2     = pow(proton_mass_c2,2) + 2.0*proton_mass_c2*Nu - Q2;
    }while(W2/GeV/GeV<0 || W2/GeV/GeV>9 || Q2/GeV/GeV<0 || Q2/GeV/GeV>10); //this is because F1F2IN09 won't work for W>3 and Q2>10
    
    // Mott scattering
    double MOTT = pow((0.00072/beamE*CTH/STH/STH),2);
    MOTT = MOTT*1.0E4; // Units: ub/sr/GeV

    int A=27;
    int Z=13;
    int bad=F1F2IN09(Z, A, Q2/GeV/GeV, W2/GeV/GeV, F1, F2);  
    if(bad){
      G4cerr << "ERROR: " << __FILE__ << " line " << __LINE__ << G4endl;	
      G4cerr << "  result was (-1 = Q2,W2 out of range in resmodd | -2 = A<3) : " << bad <<G4endl;
      G4cerr << "     Q2 W2 A "<<Q2/GeV/GeV<<" "<<W2/GeV/GeV<<" "<<A<< G4endl;
      asym=0.99999;
      effectiveXsection=0;
      return;
    }
    W1 = F1/proton_mass_c2;
 
    xsect = MOTT*(F2/(Nu/GeV) + 2.0*T2THE*(W1*GeV))*(beamE/GeV - electron_mass_c2/GeV);

    if (xsect < 0 || std::isnan(xsect) ) {
        G4cerr << "ERROR: " << __FILE__ << " line " << __LINE__ << G4endl;	
        G4cerr<<"Inelatic xsection problem: "<<xsect<<" "<<F1<<" "<<F2
              <<" "<<th<<" "<<Q2/GeV/GeV<<" "<<W2/GeV/GeV<<" "<<G4endl;
        asym=0.99999;
        effectiveXsection=0;
        exit(1);
    }

    asym=Q2*0.8e-4/GeV/GeV;
    fWeight = xsect*sin(th);
    effectiveXsection=xsect;
}
