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
               ": Error!  Some lazy programmer didn't account for complex materials in the moller process!" << G4endl;
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

    // Generate flat energy distribution of outgoing electron
    eOut =  electron_mass_c2 + CLHEP::RandFlat::shoot(electron_mass_c2, beamE);

    double CTH = cos(th/2.0);
    double STH = sin(th/2.0);
    double T2THE = STH*STH/CTH/CTH;
    double Nu = beamE - eOut;
    Q2 = 4.0*beamE*eOut*STH*STH;
    W2 = pow(proton_mass_c2,2) + 2.0*proton_mass_c2*Nu - Q2;// FIXME labeled Wsq

    // Mott scattering
    double MOTT = pow((0.00072/beamE*CTH/STH/STH),2);
    MOTT = MOTT*1.0E4; // Units: ub/sr/GeV

    int bad=F1F2IN09(13, 27, Q2/GeV/GeV, W2/GeV/GeV, F1, F2);  
    if(bad){
      G4cerr << "ERROR: " << __FILE__ << " line " << __LINE__ << G4endl;	
      G4cerr << "-1 = Q2,W2 out of range in resmodd | -2 = A<3" << bad << G4endl;
      asym=0.99999;
      effectiveXsection=0;
      return;
    }
    W1 = F1/proton_mass_c2;
 
    xsect = MOTT*(F2/(Nu/GeV) + 2.0*T2THE*(W1*GeV))*(beamE/GeV - electron_mass_c2/GeV);
    std::cout<<beamE<<" "<<eOut<<std::endl;
//     std::cout<<"Q2, Nu, W2, F1, F2, xsec "
    std::cout<<std::setw(10)<<Q2/GeV/GeV<<" "<<std::setw(10)<<Nu/GeV<<" "<<std::setw(10)<<W2/GeV/GeV<<" "<<std::setw(10)<<F1<<" "<<std::setw(10)<<F2<<" "<<std::setw(10)<<xsect<<std::endl;
    // In some cases a negative F2 is returned giving a negative cross section
    if (xsect <= 0) {
        G4cerr << "ERROR: " << __FILE__ << " line " << __LINE__ << G4endl;	
        G4cerr<<"Inelatic xsection < 0 "<<xsect<<" "<<F2
              <<" "<<th<<" "<<Q2<<" "<<W2<<" "<<G4endl;
        asym=0.99999;
        effectiveXsection=0;
        return;
    }
    asym=Q2*0.8e-4/GeV/GeV;
    fWeight = xsect*sin(th);
    effectiveXsection=xsect;
}
