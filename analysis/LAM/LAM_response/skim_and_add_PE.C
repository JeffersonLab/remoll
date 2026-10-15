#include <string>
#include <iostream>
#include <cmath>
#include <map>
#include "TROOT.h"
#include "TStyle.h"
#include "TGaxis.h"
#include "TFile.h"
#include "TH1I.h"
#include "TH2.h"
#include "TTree.h"
#include "TCanvas.h"
#include "TPaveStats.h"
#include "TSystem.h"
#include "TRandom3.h"


'''
Macro to skim remoll outputs with active LAM detectors, 
then create a new rootfile which contains only LAM events
adds a branch for the response of a single LAM PMT.

Author: Daniel Valmassei, dvalmassei@vt.edu
Date: 10-2-2026

Usage: 
    Lines to edit:
        282: list of good rootfiles
        283: location of rootfiles
        284: output location
        322: output file name
        354: name of electron lookup table
        355: name of gamma lookup table

    Location of files:
        Electron and gamma lookup tables should be loacted
        in the same directory as this macro.
'''

using namespace std;
const double pi = std::atan(1.0) * 4.0;

struct BinData {
    double meanPEs;
    double stdDev;
};

class PEMap {

   private:

      // Grid information
      double xMin;
      double yMin;
      double binSize;

      int nX;
      int nY;

      // 2D lookup table
      std::vector<std::vector<BinData>> lookup;

      TRandom3 rng;

   public:

    // --------------------------------------------------------
    // Constructor
    // --------------------------------------------------------

    PEMap(const char* filename, double gridSpacing = 10.0)
        : binSize(gridSpacing), rng(0)
    {
        std::ifstream file(filename);

        if (!file.is_open()) {
            std::cerr << "Error opening " << filename << std::endl;
            return;
        }

        std::string line;

        // Skip header
        std::getline(file, line);

        // Temporary storage
        struct CSVRow {
            double x;
            double y;
            double mean;
            double sigma;
        };

        std::vector<CSVRow> rows;

        double xmin = 1e30;
        double xmax = -1e30;
        double ymin = 1e30;
        double ymax = -1e30;

        // ----------------------------------------------------
        // Read CSV
        // ----------------------------------------------------

        while (std::getline(file, line)) {

            if (line.empty())
                continue;

            std::stringstream ss(line);
            std::string value;

            CSVRow row;

            std::getline(ss, value, ',');
            row.x = std::stod(value);

            std::getline(ss, value, ',');
            row.y = std::stod(value);

            std::getline(ss, value, ',');
            row.mean = std::stod(value);

            std::getline(ss, value, ',');
            row.sigma = std::stod(value);

            rows.push_back(row);

            xmin = std::min(xmin, row.x);
            xmax = std::max(xmax, row.x);

            ymin = std::min(ymin, row.y);
            ymax = std::max(ymax, row.y);
        }

        file.close();

        // ----------------------------------------------------
        // Determine grid dimensions
        // ----------------------------------------------------

        xMin = xmin;
        yMin = ymin;

        nX = static_cast<int>(
            std::round((xmax - xmin) / binSize)
        ) + 1;

        nY = static_cast<int>(
            std::round((ymax - ymin) / binSize)
        ) + 1;


        // Allocate lookup table

        lookup.resize(nX);

        for (int ix = 0; ix < nX; ix++)
            lookup[ix].resize(nY);


        // ----------------------------------------------------
        // Fill lookup table
        // ----------------------------------------------------

        for (const auto& row : rows) {

            int ix = static_cast<int>(
                std::round((row.x - xMin) / binSize)
            );

            int iy = static_cast<int>(
                std::round((row.y - yMin) / binSize)
            );

            if (ix >= 0 && ix < nX &&
                iy >= 0 && iy < nY) {

                lookup[ix][iy].meanPEs = row.mean;
                lookup[ix][iy].stdDev = row.sigma;
            }
        }


        std::cout << "Loaded PE map\n";
        std::cout << "X bins: " << nX << "\n";
        std::cout << "Y bins: " << nY << "\n";
        std::cout << "Grid spacing: " << binSize << " mm\n";
    }


    // --------------------------------------------------------
    // Get randomly sampled PE value
    // --------------------------------------------------------

    double GetPE(double x, double y)
    {
        // Convert position directly to grid indices

        int ix = static_cast<int>(
            std::floor((x - xMin) / binSize + 0.5)
        );

        int iy = static_cast<int>(
            std::floor((y - yMin) / binSize + 0.5)
        );


        // Check bounds

        if (ix < 0 || ix >= nX ||
            iy < 0 || iy >= nY) {

            std::cerr << "Warning: point ("
                      << x << ", " << y
                      << ") is outside PE map."
                      << std::endl;

            return 0.0;
        }


        // Lookup mean and sigma

        double mean =
            lookup[ix][iy].meanPEs;

        double sigma =
            lookup[ix][iy].stdDev;


        // Generate random value

        return rng.Gaus(mean, sigma);
    }


    // --------------------------------------------------------
    // Get mean PE without random sampling
    // --------------------------------------------------------

    double GetMeanPE(double x, double y)
    {
        int ix = static_cast<int>(
            std::floor((x - xMin) / binSize + 0.5)
        );

        int iy = static_cast<int>(
            std::floor((y - yMin) / binSize + 0.5)
        );

        if (ix < 0 || ix >= nX ||
            iy < 0 || iy >= nY)
            return 0.0;

        return lookup[ix][iy].meanPEs;
    }


    // --------------------------------------------------------
    // Get standard deviation
    // --------------------------------------------------------

    double GetStdDev(double x, double y)
    {
        int ix = static_cast<int>(
            std::floor((x - xMin) / binSize + 0.5)
        );

        int iy = static_cast<int>(
            std::floor((y - yMin) / binSize + 0.5)
        );

        if (ix < 0 || ix >= nX ||
            iy < 0 || iy >= nY)
            return 0.0;

        return lookup[ix][iy].stdDev;
    }
};


void skim_and_add_PE() {

   //Set globle styles
   gROOT->Reset();

   //define variables
   Int_t fileSplit = 0;

   Int_t maxFileCount = 1000;
   TString goodInRootFiles = "/lustre24/expphy/volatile/halla/moller12gev/nshankma/decon_moller.list";
   TString inRootFileDir = "/lustre24/expphy/volatile/halla/moller12gev/nshankma/";
   TString outRootFileDir = "/lustre24/expphy/volatile/halla/moller12gev/dvalmass/remolloutput/";

   //point to rootfile directory
   gSystem->Exec("mkdir -p "+outRootFileDir);
    


   // 2. Create a new file
   TString outRootFile;
   TFile *newFile;
   TTree *newTree;
   Double_t x(-1.0e-12), y(-1.0e-12), phi(-1.0e-12), thcom(-1.0e-12), 
            px(-1.0e-12), py(-1.0e-12), pz(-1.0e-12), p(-1.0e-12), energy(-1.0e-12),
            rate(-1.0e-12), A(-1.0e-12), Am(-1.0e-12), PE(-1.0e-12);
   Int_t pid(0), det(0), trid(0);

   //Process rootfiles
   TChain* T = new TChain("T");
   // Loop over all the root files in the directory
   std::ifstream list(goodInRootFiles);
   std::string filename;

   while(std::getline(list, filename)){
      cout << "Trying to access: " << filename << endl;
      TString fnm = filename;
      // Check if the file exists (and it assumed that the zombie files are not present)
      if (gSystem->AccessPathName(inRootFileDir + fnm)) continue;
      T->Add(inRootFileDir + fnm);
      fileSplit++;
   }
   cout << "Total number of files: " << fileSplit << endl;
   Long64_t nEntries = T->GetEntries();

   
   outRootFile = outRootFileDir + "new_file.root";
   newFile = new TFile(outRootFile, "RECREATE");
   newTree = new TTree("T", "qsim tree");

   //Set branches for out root file
   newTree->Branch("x", &x, "x/D");
   newTree->Branch("y", &y, "y/D");
   newTree->Branch("thcom", &thcom, "thcom/D");
   newTree->Branch("px", &px, "px/D");
   newTree->Branch("py", &py, "py/D");
   newTree->Branch("pz", &pz, "pz/D");
   newTree->Branch("p", &p, "p/D");
   newTree->Branch("energy", &energy, "energy/D");
   newTree->Branch("pid", &pid, "pid/I");
   newTree->Branch("det", &det, "det/I");
   newTree->Branch("trid", &trid, "trid/I");
   newTree->Branch("rate", &rate, "rate/D");
   newTree->Branch("A", &A, "A/D");
   newTree->Branch("Am", &Am, "Am/D");
   newTree->Branch("PE", &PE, "PE/D");



   // 3. Copy only events that hit the detectors
   vector<remollGenericDetectorHit_t> *fHit = 0;
   remollEvent_t *fEvent=0;
   Double_t fRate = 0;

   T->SetBranchAddress("hit", &fHit);
   T->SetBranchAddress("ev", &fEvent);
   T->SetBranchAddress("rate",&fRate);

   PEMap electronPeMap("electron_CM_10mm_lookupTable.csv", 10.0);
   PEMap gammaPeMap("gamma_CM_10mm_lookupTable.csv", 10.0);

   //loop over all events
   for (int i=0; i<nEntries; i++){
      T->GetEntry(i);

      //loop over all hits in the event
      for (Int_t iHit = 0; iHit < fHit->size(); iHit++){
         det = fHit->at(iHit).det;
         pid = fHit->at(iHit).pid;
         trid = fHit->at(iHit).trid;
         energy = fHit->at(iHit).e;
         x = fHit->at(iHit).x;
         y = fHit->at(iHit).y;
         px = fHit->at(iHit).px;
         py = fHit->at(iHit).py;
         pz = fHit->at(iHit).pz;
         p = fHit->at(iHit).p;
         thcom = fEvent->thcom;
         A = fEvent->A;
         Am = fEvent->Am;
         rate = fRate; //Rate is in Hz/65uA
         
         //Transform coordinates and momentum
          if (det>80000 && det<80099){
            double_t xtemp = x;
            double_t ytemp = y;
            double_t pxtemp = px;
            double_t pytemp = py;
            double_t rot_angle = -6*pi/7;
            x = xtemp*cos(rot_angle) - ytemp*sin(rot_angle);
            y = xtemp*sin(rot_angle) + ytemp*cos(rot_angle);
            px = pxtemp*cos(rot_angle) - pytemp*sin(rot_angle);
            py = pxtemp*sin(rot_angle) + pytemp*cos(rot_angle);
            pz = pz;
         } else if (det>80100 && det<80199){
            double_t xtemp = x;
            double_t ytemp = y;
            double_t pxtemp = px;
            double_t pytemp = py;
            double_t rot_angle = -4*pi/7;
            x = xtemp*cos(rot_angle) - ytemp*sin(rot_angle);
            y = xtemp*sin(rot_angle) + ytemp*cos(rot_angle);
            px = pxtemp*cos(rot_angle) - pytemp*sin(rot_angle);
            py = pxtemp*sin(rot_angle) + pytemp*cos(rot_angle);
            pz = pz;
         } else if (det>80200 && det<80299){
            double_t xtemp = x;
            double_t ytemp = y;
            double_t pxtemp = px;
            double_t pytemp = py;
            double_t rot_angle = -2*pi/7;
            x = xtemp*cos(rot_angle) - ytemp*sin(rot_angle);
            y = xtemp*sin(rot_angle) + ytemp*cos(rot_angle);
            px = pxtemp*cos(rot_angle) - pytemp*sin(rot_angle);
            py = pxtemp*sin(rot_angle) + pytemp*cos(rot_angle);
            pz = pz;
         } else if (det>80300 && det<80399){
            double_t xtemp = x;
            double_t ytemp = y;
            double_t pxtemp = px;
            double_t pytemp = py;
            double_t rot_angle = 0*pi/7;
            x = xtemp*cos(rot_angle) - ytemp*sin(rot_angle);
            y = xtemp*sin(rot_angle) + ytemp*cos(rot_angle);
            px = pxtemp*cos(rot_angle) - pytemp*sin(rot_angle);
            py = pxtemp*sin(rot_angle) + pytemp*cos(rot_angle);
            pz = pz;
         } else if (det>80400 && det<80499){
            double_t xtemp = x;
            double_t ytemp = y;
            double_t pxtemp = px;
            double_t pytemp = py;
            double_t rot_angle = -12*pi/7;
            x = xtemp*cos(rot_angle) - ytemp*sin(rot_angle);
            y = xtemp*sin(rot_angle) + ytemp*cos(rot_angle);
            px = pxtemp*cos(rot_angle) - pytemp*sin(rot_angle);
            py = pxtemp*sin(rot_angle) + pytemp*cos(rot_angle);
            pz = pz;
         } else if (det>80500 && det<80599){
            double_t xtemp = x;
            double_t ytemp = y;
            double_t pxtemp = px;
            double_t pytemp = py;
            double_t rot_angle = -10*pi/7;
            x = xtemp*cos(rot_angle) - ytemp*sin(rot_angle);
            y = xtemp*sin(rot_angle) + ytemp*cos(rot_angle);
            px = pxtemp*cos(rot_angle) - pytemp*sin(rot_angle);
            py = pxtemp*sin(rot_angle) + pytemp*cos(rot_angle);
            pz = pz;
         } else if (det>80600 && det<80699){
            double_t xtemp = x;
            double_t ytemp = y;
            double_t pxtemp = px;
            double_t pytemp = py;
            double_t rot_angle = -8*pi/7;
            x = xtemp*cos(rot_angle) - ytemp*sin(rot_angle);
            y = xtemp*sin(rot_angle) + ytemp*cos(rot_angle);
            px = pxtemp*cos(rot_angle) - pytemp*sin(rot_angle);
            py = pxtemp*sin(rot_angle) + pytemp*cos(rot_angle);
            pz = pz;
         }

         if (det>80000 && det<90000 && pz>0) {
            if (pid == 11){
               PE = electronPeMap.GetMeanPE(x,y);
            } else if (pid == 22){
               PE = gammaPeMap.GetMeanPE(x,y);
            } else{
               PE = NAN;
            }
            
            newTree->Fill();
         } 
         //if (det==detNum[argSector] && (pid==11 || pid==22) && pz>0) newTree->Fill(); 
      }
      if (i%1000==0) cout << "Processed " << i << " events." << endl;
   }
 
   // 4. Save to the new file and close
   newTree->Write();
   delete newFile;
}