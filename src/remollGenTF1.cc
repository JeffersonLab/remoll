/* remollGenTF1.cc
 *
 * Updated June 2018
 *
 * Reads functions of radius from external root file and generates particles according to those functions.
 * Default functions are inside remollGenFunctions.root, and include moller, elastic, and inelastic scattering
 * for the whole array as well as for sectors 1, 2, and 3.
 *
 */
#include "remollGenTF1.hh"

#include "remollEvent.hh"
#include "remollVertex.hh"
#include "remollBeamTarget.hh"

#include "G4GenericMessenger.hh"
#include "G4Material.hh"
#include "G4VPhysicalVolume.hh"
#include "G4LogicalVolume.hh"
#include "G4PhysicalConstants.hh"

#include "remolltypes.hh"


#include "CLHEP/Random/RandFlat.h"
#include "CLHEP/Random/RandGauss.h"
#include <math.h>
#include <iostream>
#include <string>
#include <vector>
// For the TF1s
#include "TROOT.h"
#include "TFile.h"
#include "TTree.h"
#include "TH1F.h"
#include "TF1.h"
#include "TMath.h"


G4Mutex inFileMutex2 = G4MUTEX_INITIALIZER; //files are being read so mutex is needed

remollGenTF1::remollGenTF1()
: remollVEventGen("TF1"),
    fFile(0),
    fFunc(0),fMollerFunc(0),
    fElasticFunc(0),
    fInelasticFunc(0),
    fType(),
    fRing(1), fSector(0)
{
    fApplyMultScatt = true;
    r_t = G4RandFlat::shoot(600,1200);
    fZpos = (28.5*m - 0.52*m);
    fThisGenMessenger->DeclarePropertyWithUnit("rmax","mm",fR_max,"Maximum generation radial hit position (mm) for Remoll generator");
    fThisGenMessenger->DeclarePropertyWithUnit("rmin","mm",fR_min,"Minimum generation radial hit position (mm) for Remoll generator");
    fThisGenMessenger->DeclarePropertyWithUnit("deltaphmax","deg",fDeltaPh_max,"Upward Phi spread limit");
    fThisGenMessenger->DeclarePropertyWithUnit("deltaphmin","deg",fDeltaPh_min,"Downward Phi spread limit");
    fThisGenMessenger->DeclareProperty("BoffsetR",fBoffsetR,"Boolean for offsetting detector to the side (and flat radial distribution if R_max =/= 0)");
    fThisGenMessenger->DeclareProperty("sector",fSector,"Integer sector number for Remoll generator");
    fThisGenMessenger->DeclareProperty("ring",fRing,"Integer ring number for Remoll generator");
    fThisGenMessenger->DeclareMethod("setFileFunction",&remollGenTF1::SetGenFunctionFile,"ROOT filename:function name");
    fThisGenMessenger->DeclareMethod("scattType",&remollGenTF1::SetScatteringType,"Scattering type: moller, elastic, inelastic, or all");
    fThisGenMessenger->DeclareMethod("sector",&remollGenTF1::SetSector,"Sector number: 1,2,or 3, or 0 for all");
    fThisGenMessenger->DeclareMethod("radOffset",&remollGenTF1::SetRadOffset,"Radial offset to center detectors: boolean");
    fThisGenMessenger->DeclareMethod("ring",&remollGenTF1::SetRing,"Detector ring number (1-6)");
    SetScatteringType("all");
    SetSector(0);
    SetRadOffset(false);
    SetGenFunctionFile("remollGenFunctions.root:elastic_0");
}

remollGenTF1::~remollGenTF1() {
    if (fFile){
        fFile->Close();
    }
    fFunc = 0;
    fMollerFunc = 0;
    fElasticFunc = 0;
    fInelasticFunc = 0;
}

void remollGenTF1::PrintEventGen()
{
  remollVEventGen::PrintEventGen();
  G4cout << "r hits (Remoll generator) =[" << fR_min/mm << "," << fR_max/mm << "] mm" << G4endl;
  G4cout << "phi spread (remoll generator) =  [" << fDeltaPh_min/deg << "," << fDeltaPh_max/deg << "] deg" << G4endl;
}

void remollGenTF1::SetRing(G4int num){ fRing = num; }

void remollGenTF1::SetRadOffset(G4bool offset){ fBoffsetR = offset; }

void remollGenTF1::SetScatteringType(G4String input){ fType = input; }

void remollGenTF1::SetSector(const G4int secnum){ fSector = secnum; }

void remollGenTF1::SetGenFunctionFile(G4String input) {
    if(input == "genDefault"){ //input for generating input files using moller, elastic, and inelastic generators and then performing fits to use as TF1 input
        G4cerr << "Reading generated default output files." << G4endl;
        distAnalysis();
        return;
    }

    if (!input.contains(":")){
        G4cerr << "Improper formatting for user input. Please ensure your macro commands include '/remoll/evgen/TF1/setFileFunction <file name>:<function name>'" << G4endl;
        return;
    }

    std::stringstream ss;
    ss << input;

    G4String filename;
    G4String funcname;

    std::getline(ss, filename, ':');
    std::getline(ss, funcname, ':');

    G4cout << "File name read as " << filename << G4endl;
    G4cout << "Function name read as " << funcname << G4endl;

    G4AutoLock inFileLock(&inFileMutex2);

    G4cout << "Setting the external file to " << filename << G4endl;

    if (fFile) {
        fFile->Close();
        fFunc  = 0;
    }

    fFile = new TFile(filename);
    if (! fFile){
        G4cerr << "Could not open function file " << filename << G4endl;
        return;
    }

    fFile->GetObject(funcname,fFunc);
    if (! fFunc){
        G4cerr << "could not find function in file " << filename << G4endl;
        return;
    }
    inFileLock.unlock();
}

void remollGenTF1::SamplePhysics(remollVertex * /*vert*/, remollEvent *evt)
{
    if (!fFunc){
        G4cerr << "No Function set before SamplePhysics was called" << G4endl;
        return;
    }
  double xPos, yPos, zPos, zOffset;
  fE_min = 2.0*GeV;
  fE_max = 11.0*GeV;

  fTh_min = -0*deg;//FIXME Needs justification, what about the angle about the z axis that this points as well, is that phi, or is phi the start position about z?
  fTh_max = 2.5*deg;

  fR_min = 0.0*mm;
  fR_max = 0.0*mm;

  fPh_min = 0.0*deg;
  fPh_max = 360.0*deg;

  fDeltaPh_min = -2.0*deg;
  fDeltaPh_max = 2.0*deg;

//  fRing = 5;
//  fSector = 0; //FIXME what are the sector options? What do they refer to?
//  fBoffsetR = false;

//  zPos = fZ;

  // Get initial Remoll energy instead of using other sampling
  double E = G4RandFlat::shoot( fE_min, fE_max );
  double mass = electron_mass_c2;
  double p = sqrt(E*E - mass*mass);

  double pX, pY, pZ;
  double randTheta, randDeltaPhi, randPhi;

  randTheta = G4RandFlat::shoot( fTh_min, fTh_max );
  randPhi = G4RandFlat::shoot(fPh_min,fPh_max);
  randDeltaPhi = G4RandFlat::shoot(fDeltaPh_min,fDeltaPh_max); // FIXME define min/max angle spread in phi direction
  pX = cos(randPhi)*sin(randTheta)*p + sin(randPhi)*sin(randDeltaPhi)*p;
  pY = sin(randPhi)*sin(randTheta)*p - cos(randPhi)*sin(randDeltaPhi)*p;
  pZ = cos(randDeltaPhi)*cos(randTheta)*p;
//get radial distribution from remoll, radius is along x-axis
  double radialOffset[6][3] = {{710,710,710},
    {755,755,755},
    {817.5,817.5,817.5},
    {892.5,892.5,892.5},
    {1017.5,987.5,1030},
    {1150,1150,1150}};
  double rad = RadSpectrum();
  zOffset = -1*500; //FIXME arbitrary z offset for Moller distribution propagation - affects air showering noise
  zPos = (28500 + zOffset); //FIXME arbitrary z offset for Moller distribution propagation - affects air showering noise
  double xHitPos = (rad*cos(randPhi)- 1*((fBoffsetR)? radialOffset[fRing][fSector] : 0.0)); // Putting the offset here means that the detector and distribution will still make circles, just where the edge of the circle now passes the origin
  double yHitPos = rad*sin(randPhi);
  xPos = xHitPos - (-1*zOffset)*sin(randTheta)*cos(randPhi) - (-1*zOffset)*sin(randPhi)*sin(randDeltaPhi);
  yPos = yHitPos - (-1*zOffset)*sin(randTheta)*sin(randPhi) + (-1*zOffset)*cos(randPhi)*sin(randDeltaPhi);


  assert( E > 0.0 );
  assert( E > mass );

  evt->ProduceNewParticle( G4ThreeVector(xPos, yPos, zPos), 
    G4ThreeVector(pX, pY, pZ ), 
	  "e-" );
  evt->fBeamE = E;
  evt->fBeamMomentum = evt->fBeamMomentum.unit()*p;
  // Override Beam Target creating a spread in z vertex, by overwriting the remollVEventGen.cc functionality
  // where it asks the remollBeamTarget to treat the vertex as beam target spread. Adds (+=) the vector fVertexPos
  // to the particle position vector defined when particle is defined, happening in PolishEvent() method
  evt->fVertexPos.setZ( 0.0 );
  
  evt->SetEffCrossSection(0.0);
  evt->SetAsymmetry(0.0);
  evt->SetQ2(0.0);
  evt->SetW2(0.0);

}

double remollGenTF1::RadSpectrum(){
    //generate radius from TF1, radius in millimeters!
    if (fR_max>0.0 && fBoffsetR==true) {
        double flatRad = G4RandFlat::shoot(fR_min,fR_max);
        return flatRad;
    }
  
  //Randomly generate the distribution using the Metropolis algorithm
    double r, a, u; 
  //r_t is the previous hit, r is the proposed new hit, a is their relative
  //probabilities, and u is the deciding probability.
    r = G4RandGauss::shoot(r_t,100); //generate proposed r with gaussian around previous
    
    if (fType == "all" && !fFile){
        a = (fMollerFunc->Eval(r) + fElasticFunc->Eval(r) + fInelasticFunc->Eval(r)) / (fMollerFunc->Eval(r_t) + fElasticFunc->Eval(r_t) + fInelasticFunc->Eval(r_t));
    }else{
        a = fFunc->Eval(r) / fFunc->Eval(r_t);
    }
    u = G4RandFlat::shoot(0.0,1.0);
    if (u <= a)
        r_t = r;
    return  r_t;
}

void remollGenTF1::distAnalysis(){
    //takes input of remoll output files using moller, elastic, and inelastic generators
    //Pulls hit radius data from the root files and puts it into histograms, and then fits 
    //those histograms with parameterized functions that are output into remollGenFunctions.root
    if (fType == "moller" || fType == "all"){
        G4String fname("remollout_moller.root");
        getHist(fname);
        fitHist("moller");
        if(fType == "moller")
            fFunc = fMollerFunc;
    }if (fType == "elastic" || fType == "all"){
        getHist("remollout_elastic.root");
        fitHist("elastic");
        if (fType == "elastic")
            fFunc = fElasticFunc;
    }if (fType == "inelastic" || fType == "all"){
        getHist("remollout_inelastic.root");
        fitHist("inelastic");
        if (fType == "inelastic")
            fFunc = fInelasticFunc;
    }
}


void remollGenTF1::getHist(G4String fname){
    G4AutoLock inFileLock(&inFileMutex2);
    
    G4cout << "Opening file " << fname << G4endl;
    TFile *file = new TFile(fname);
    if (!file){
        G4cerr << "File not found." << G4endl;
        exit(1);
    }
    TTree *T = (TTree*)(file->Get("T"));
    if (!T){
        G4cerr << "TTree T not found" << G4endl;
        exit(1);
    }
    TH1F* rad = new TH1F("rad","hit.r",120,600,1200);
    
    rad->GetXaxis()->SetTitle("Radius (mm)");
    rad->GetYaxis()->SetTitle("Rate (GeV/5 mm)");

    int entries = T->GetEntries();
    double rate;
    std::vector<remollGenericDetectorHit_t>* Hit = new std::vector<remollGenericDetectorHit_t>;

    if (T->GetBranch("hit")){
        T->SetBranchAddress("hit", &Hit);
    }else{
        G4cerr << "Could not find branch hit in input file" << G4endl;
    }
    if (T->GetBranch("rate")){ 
        T->SetBranchAddress("rate",&rate);
    }else{
        G4cerr << "Could not find branch rate in input file" << G4endl;
    }
    std::cerr << "Total entries to read: " << entries << std::endl;
    for(int i = 0; i < entries; i++){
        T->GetEntry(i);
        for (size_t j = 0; j <  Hit->size(); j++){
            remollGenericDetectorHit_t hit = Hit->at(j);//create local copy of hit
            if (hit.det == 28 && hit.r > 600 && hit.r < 1200 && (hit.pid == 11 || hit.pid == -11)){
                if (fSector == 0){
                    rad->Fill(hit.r,rate);
                    continue;
                }
                 
                //determine sector number (conditions provided by Seamus)
                 double secphi;
                 double eigth = 360.0/56.0;
                 if (atan2(hit.y,hit.x)>0){
                     secphi = fmod(atan2(hit.y,hit.x),2.0*3.14159/7.0)*180/3.14159;
                }
                 else{
                     secphi = fmod(atan2(hit.y,hit.x)+2.0*3.14159,2.0*3.14159/7.0)*180/3.14159;
                 }

                 if((secphi < eigth || secphi > 7*eigth) && fSector == 1)
                     rad->Fill(hit.r,rate);
                 if (((eigth < secphi && secphi < 3.0*eigth) || (5.0*eigth < secphi && secphi < 7.0*eigth)) && fSector == 2)
                     rad->Fill(hit.r,rate);
                 if ((3.0*eigth < secphi && secphi < 5.0*eigth) && fSector == 3)
                     rad->Fill(hit.r,rate);
            }
        }
    } 
    rad->Scale(1e7/entries);
    rad->SetDirectory(0);    
    file->Close();
    inFileLock.unlock();
    if (fname == "remollout_moller.root"){
        fMollerHist = rad;
    }else if (fname == "remollout_elastic.root"){
        fElasticHist = rad;
    }else if ( fname == "remollout_inelastic.root"){
        fInelasticHist = rad;
    }
    return;
    
}

void remollGenTF1::fitHist(G4String type){
    TF1* fit;
     
    //fit histograms of all sectors
        if (type == "moller"){
            TH1F* rad = fMollerHist;
            G4cerr << "Fitting moller sector " << fSector << G4endl;
            fit = new TF1("mol","gaus",600,1200);
            rad->GetXaxis()->SetRange(rad->FindBin(900),rad->FindBin(1100));
            double max = rad->GetMaximum();
            double mean = rad->GetBinCenter(rad->GetMaximumBin());
            double FWHM = rad->GetBinCenter(rad->FindLastBinAbove(max/2.0)) - rad->GetBinCenter(rad->FindFirstBinAbove(max/2.0));
            fit->SetParameters(max,FWHM/2,mean);
            rad->Fit("mol","NM");
            fMollerFunc = fit;
            return;
        }

         else if (type == "elastic"){
            TH1F* rad = fElasticHist;
             G4cerr << "Fitting elastic sector " << fSector << G4endl;
           
            fit = new TF1("el", remollGenTF1::elasticFit, 600,1200,6);
            
            rad->GetXaxis()->SetRange(rad->FindBin(700),rad->FindBin(800));  
            
            double max = rad->GetMaximum();
            double mean = rad->GetBinCenter(rad->GetMaximumBin());
            double FWHM = rad->GetBinCenter(rad->FindLastBinAbove(max/2.0)) - rad->GetBinCenter(rad->FindFirstBinAbove(max/2.0));
            fit->SetParameter(0,max);
            fit->SetParameter(2,mean);
            fit->SetParameter(1,FWHM/2);

            //Adjust range to only quadratic range
            rad->GetXaxis()->SetRange(rad->FindBin(800),rad->FindBin(1100));
            
            //fit->SetParameter(3,8e6);
            double b = rad->GetBinCenter(rad->GetMinimumBin());
            double c = rad->GetMinimum();
            double a = rad->GetBinContent(rad->FindBin(b+110))/12100;
            
            fit->SetParameter(3, a);
            fit->SetParameter(4, c);
            fit->SetParameter(5, b);

            
            rad->GetXaxis()->SetRange(rad->FindBin(600),rad->FindBin(1200));
            rad->Fit(fit,"QMN");
            fit->SetParameters(fit->GetParameters());
            rad->Fit(fit,"MN");
            fElasticFunc = fit;
            return;
         }
    
        else if (type == "inelastic"){
            G4cerr << "Fitting inelastic sector "<< fSector << G4endl;
            TH1F* rad = fInelasticHist;
            fit = new TF1("in",remollGenTF1::inelasticFit,600,1200,5);
            
            
            rad->GetXaxis()->SetRange(0,rad->FindBin(800));
            double max = rad->GetMaximum();
            double mean = rad->GetBinCenter(rad->GetMaximumBin());
            double HWHM = abs(mean - rad->GetBinCenter(rad->FindFirstBinAbove(max/2.0)));
            double stddev = 2*HWHM / (2*sqrt(2*log(2)));
            fit->SetParameter(0,max);
            fit->SetParameter(1,mean);
            fit->SetParameter(2,stddev);
            
            //a*x^-b
            rad->GetXaxis()->SetRange(rad->FindBin(mean),rad->FindBin(1200));
            double x1 = 950.0;
            double x2 = 1100.0;
            double y1 = rad->GetBinContent(rad->FindBin(x1));
            double y2 = rad->GetBinContent(rad->FindBin(x2));
            double b = log(y1/y2) / log(x2/x1);
            double a = y1 / pow(x1,-1*b);
            fit->SetParameter(3,a);
            fit->SetParameter(4,b);

            rad->GetXaxis()->SetRange(0,120);

            rad->Fit(fit,"NMWP");
            fInelasticFunc = fit;
            return;
        }
    return;
}
double remollGenTF1::lorentzFit(double *x, double *par){
    return (0.5*par[0]*par[1]/TMath::Pi())/TMath::Max(1.0e-10,(x[0]-par[2])*(x[0]-par[2]) + 0.25*par[1]*par[1]);
}

double remollGenTF1::elasticFit(double *x, double *par){
    if (x[0] > 1200 || x[0] < 600)
        return 0.0;
    double q = 0.0;
    double l = lorentzFit(x,par);

    q =  (TMath::Abs(par[3])*(x[0]-par[5])*(x[0]-par[5])-par[4]);
    if (l < q && x[0] > par[2]){
        return q;
    }else{
        return l;
    }
    return 0.0;
}

double remollGenTF1::inelasticFit(double *x, double *par){
    double arg = (fabs(par[2])>1e-6)? (x[0] - (par[1]))/par[2] : 0.0;
    double g = par[0]*exp(-0.5*arg*arg)/(par[2]*sqrt(2.0*TMath::Pi()));
    double e = 0;
    if (x[0] > (par[1]))
        e = par[3]/TMath::Power(x[0],par[4]);/* + par[5];*/
    if (g > e){
        return g;
    }
    else{
        return e;
    }
    return 0.0;
}
