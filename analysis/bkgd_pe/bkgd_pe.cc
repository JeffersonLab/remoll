#include <sstream>
#include <string>
#include <iostream>
#include <fstream>
#include <vector> 

#include <TApplication.h>
#include <TRint.h>
#include <TSystem.h>

#include <TStyle.h>
#include <TPaveStats.h>
#include <TCanvas.h>
#include <TGraph.h>
#include <TMultiGraph.h>
#include <TLegend.h>
#include <TGraphErrors.h>
#include <TFrame.h>
#include <TObjArray.h>
#include <TVector2.h>
#include <TLatex.h>

#include <TFile.h>
#include <TChain.h>
#include <TH2F.h>
#include <TH2D.h>

#include "TTree.h"
#include "TFile.h"
#include "remolltypes.hh"
#include "bkgd_petypes.hh"
#include "bkgd_pe.hh"

#define pi 3.141592653589793238462643383279502884L

class bkgd {
    public:
        int do_pe(int, char**);
        void set_plot_style();
        int get_lookuptable();
        void bkgd_pe_ana();
        void bkgd_pePlots(int, char **);
    private:
        std::string fileString = "tracking.root";
        std::string detName = "R5o";
        std::string anaStr = "backgrounds";
        double current = 65.0;
        double degeneracy = 50.0;
        double user_angle = 0.0;
        double user_x_pos = 0.0;
        double user_reflectivity = 0.7;
        double user_cerenkov = 1.0;
        double user_scintillation = 1.0;
        double user_z_pos = 0.0;
        double N_entries = 0;
        //std::string varString = "reflectorAngle";
        //std::string unitString = "deg";
        bool reana = true;
        int detid=540210;

        TH2D* lookupTable;
        TH2D* lookupTableReference;
        TH2D* lookupTableLog;
        TH2D* lookupTableFlat;
};

int bkgd::get_lookuptable()
{
    TFile* old_file;
    TTree* oldtree;
    /*double oldx_pos = 0.0;
    double oldangle = 0.0;
    double oldavg     = 0.0;
    double oldavg_err = 0.0;
    double oldrms     = 0.0;
    double oldrms_err = 0.0;
    double oldres     = 0.0;
    double oldN_en    = 0.0;
    double oldreflectivity = 0.0;
    double oldcerenkov = 0.0;
    double oldscintillation = 0.0;
    double oldz_pos = 0.0;*/
    // FIXME
    if (!gSystem->AccessPathName(Form("scans_%s.root",detName.c_str()))) {
        // Old file exists, read it and add new entries
        old_file = TFile::Open(Form("scans_%s.root",detName.c_str()));
        old_file->GetObject("scans", oldtree);
        if (!oldtree) {
            std::cout << "ERROR: Dead scans tree" ;
            return 0;
        }
        int nent = oldtree->GetEntries();
    }
    else {
        Printf("Error, no scans_%s.root file available, please place the look-up-table data there",detName.c_str());
        return 0;
    }
    // Draw a TGraph, GOFF mode, obtain the output vectors and loop over them, filling in a low/up bin edge array with the means of neighboring bin centers, if it's an edge then calculate distance from opposite side and add that off to the side.
    // Include in cut avg_pes != 0 (i.e. not some glitch)
    // Make a new TH2 hist with Nentries from tgraph and bin low/highs in angle and x_pos from above exercise

    TH1* tmpHist;
    //TGraph* tmpGraph;
    //
    const Int_t refnPoints = oldtree->Draw("angle:10*abs(x_pos):avg_pes","avg_pes!=0","goff");
    //const Int_t nPoints = oldtree->Draw("angle:10*(x_pos)-960.0-215.0:avg_pes","angle>-23 && angle<23 && avg_pes!=0 && scintillation==1 && cerenkov==1 && reflectivity==0.7","goff");
    Double_t* ref_local_angle_points  = oldtree->GetV1();
    Double_t* ref_local_x_pos_points  = oldtree->GetV2();
    Double_t ref_x_pos = ref_local_x_pos_points[(int) refnPoints/2];   // Just take the first entry and hope it is good enough :/
    Double_t ref_angle = ref_local_angle_points[(int) refnPoints/2];
    std::cout<< "Using reference X = " << ref_x_pos << ", and reference angle = " << ref_angle << " to determine lookup table binning info" << std::endl;

    const Int_t nAngles = oldtree->Draw("angle",Form("10*abs(x_pos)==%f",ref_x_pos),"goff");
    //const Int_t nAngles = oldtree->Draw("angle","angle>-23 && angle<23 && x_pos==1.0 && avg_pes!=0 && scintillation==1 && cerenkov==1 && reflectivity==0.7","goff");
    Double_t* angle_points1 = oldtree->GetV1();
    Double_t* angle_points = new Double_t[nAngles+1];
    Int_t* angle_indices = new Int_t[nAngles];
    TMath::Sort(nAngles,angle_points1,angle_indices,kFALSE);
    for (Int_t j = 0 ; j < nAngles; j++) {
        angle_points[j] = angle_points1[angle_indices[j]];
        std::cout<< "Bin angle point = " << angle_points[j] << std::endl;
    }

    const Int_t nXpos   = oldtree->Draw("10*abs(x_pos)",Form("angle==%f",ref_angle),"goff");
    //const Int_t nXpos   = oldtree->Draw("10*(x_pos)-960.0-215.0","angle==0.0 && avg_pes!=0 && scintillation==1 && cerenkov==1 && reflectivity==0.7","goff");
    Double_t* x_pos_points1 = oldtree->GetV1();
    Double_t* x_pos_points = new Double_t[nXpos+1];
    Int_t* x_pos_indices = new Int_t[nXpos];
    TMath::Sort(nXpos,x_pos_points1,x_pos_indices,kFALSE);
    for (Int_t j = 0 ; j < nXpos; j++) {
        x_pos_points[j] = x_pos_points1[x_pos_indices[j]];
        std::cout<< "Bin x point = " << x_pos_points[j] << std::endl;
    }
    x_pos_points[nXpos]   = x_pos_points[nXpos-1]   + ( x_pos_points[nXpos-1]   - x_pos_points[nXpos-2]   );
    angle_points[nAngles] = angle_points[nAngles-1] + ( angle_points[nAngles-1] - angle_points[nAngles-2] );
    std::cout<< "Last bin x point = " << x_pos_points[nXpos] << std::endl;
    std::cout<< "Last bin angle point = " << angle_points[nAngles] << std::endl;

    lookupTable = new TH2D("lookupTable","lookupTable",nAngles,angle_points,nXpos,x_pos_points);
    lookupTableReference = new TH2D("lookupTableReference","lookupTableReference",nAngles,angle_points,nXpos,x_pos_points);
    lookupTableLog = new TH2D("lookupTableLog","lookupTableLog",nAngles,angle_points,nXpos,x_pos_points);
    lookupTableFlat = new TH2D("lookupTableFlat","lookupTableFlat",nAngles,angle_points,nXpos,x_pos_points);

    const Int_t nPoints = oldtree->Draw("angle:10*abs(x_pos):avg_pes","avg_pes!=0","goff");
    Double_t* local_angle_points  = oldtree->GetV1();
    Double_t* local_x_pos_points  = oldtree->GetV2();
    Double_t* avg_pes_points      = oldtree->GetV3();

    for (Int_t i = 0 ; i < nPoints ; i++ ) {
        std::cout<< "Filling lookup table with point: 10*abs(x_pos) = " << local_x_pos_points[i] << ", x bin " << lookupTable->GetYaxis()->FindBin(local_x_pos_points[i]) << ", angle = " << local_angle_points[i] << ", angle bin " << lookupTable->GetXaxis()->FindBin(local_angle_points[i]) << ", pe value = " << avg_pes_points[i] << std::endl;
        lookupTable->Fill(local_angle_points[i],local_x_pos_points[i],avg_pes_points[i]);
        lookupTableReference->Fill(local_angle_points[i],local_x_pos_points[i],avg_pes_points[i]);
        lookupTableLog->Fill(local_angle_points[i],local_x_pos_points[i],TMath::Log(avg_pes_points[i]));
        // Unweighted, so the weighted one can have its bins re-normalized if any are filled with more than one point (I know, it is cheating the definition of a histogram, but lookup tables are hard in ROOT).
        lookupTableFlat->Fill(local_angle_points[i],local_x_pos_points[i],1); 
    }
    Double_t localBinContent = 0.0;
    Double_t localFlatBinContent = 0.0;

    for (Int_t i = 0 ; i < nPoints ; i++ ) {
        //std::cout << "Angle bin = " << lookupTableReference->GetXaxis()->FindBin(local_angle_points[i]) << " x_pos bin = " << lookupTableReference->GetYaxis()->FindBin(local_x_pos_points[i]) << " of x = " << local_x_pos_points[i] << std::endl;
        localBinContent = (Double_t)lookupTableReference->GetBinContent(lookupTableReference->GetXaxis()->FindBin(local_angle_points[i]),lookupTableReference->GetYaxis()->FindBin(local_x_pos_points[i]));
        localFlatBinContent = (Double_t)lookupTableFlat->GetBinContent(lookupTableFlat->GetXaxis()->FindBin(local_angle_points[i]),lookupTableFlat->GetYaxis()->FindBin(local_x_pos_points[i]));
        if (localFlatBinContent > 1) {
            std::cout<< "Checking double-filling: was " << localBinContent << ", with " << localFlatBinContent << " points at 10*(x_pos) = " << local_x_pos_points[i] << ", angle = " << local_angle_points[i] << ", pe value = " << avg_pes_points[i] << std::endl;
            // Normalize each bin the be the true average of whatever was thrown into it.
            //lookupTable->SetBinContent(lookupTable->GetBin(lookupTable->GetXaxis()->FindBin(local_angle_points[i]),lookupTable->GetYaxis()->FindBin(local_x_pos_points[i])),avg_pes_points[i]); 
            lookupTable->SetBinContent(lookupTable->GetBin(lookupTable->GetXaxis()->FindBin(local_angle_points[i]),lookupTable->GetYaxis()->FindBin(local_x_pos_points[i])),localBinContent/localFlatBinContent); 
            //lookupTable->SetBinContent(local_angle_points[i],local_x_pos_points[i],localBinContent/localFlatBinContent); 
            std::cout << "now = " << (Double_t)lookupTable->GetBinContent(lookupTable->GetXaxis()->FindBin(local_angle_points[i]),lookupTable->GetYaxis()->FindBin(local_x_pos_points[i])) << std::endl ;
        }
    }
    TCanvas * c1 = new TCanvas();
    c1->cd();
    lookupTableLog->SetName(Form("Look Up Table - Log(Avg PEs)"));
    lookupTableLog->SetTitle(Form("Look Up Table - Log(Avg PEs)"));
    lookupTableLog->SetXTitle(Form("Angle (degrees)"));
    lookupTableLog->SetYTitle(Form("Radial hit position (mm)"));
    lookupTableLog->Draw("COLZ");
    c1->SaveAs(Form("lookupTable_%s.pdf(",detName.c_str()));
    TCanvas * c2 = new TCanvas();
    c2->cd();
    lookupTable->SetName(Form("Look Up Table - Avg PEs"));
    lookupTable->SetTitle(Form("Look Up Table - Avg PEs"));
    lookupTable->SetXTitle(Form("Angle (degrees)"));
    lookupTable->SetYTitle(Form("Radial hit position (mm)"));
    lookupTable->Draw("COLZ");
    c2->SaveAs(Form("lookupTable_%s.pdf)",detName.c_str()));
    delete c1;
    delete c2;
    return 1;
}

void bkgd::set_plot_style()
{
    const Int_t NRGBs = 5;
    const Int_t NCont = 255;
    // See class TColor documentation and SetPalette() command
    Double_t stops[NRGBs] = { 0.00, 0.34, 0.61, 0.84, 1.00 };
    Double_t red[NRGBs]   = { 0.00, 0.00, 0.87, 1.00, 0.51 };
    Double_t green[NRGBs] = { 0.00, 0.81, 1.00, 0.20, 0.00 };
    Double_t blue[NRGBs]  = { 0.51, 1.00, 0.12, 0.00, 0.00 };
    TColor::CreateGradientColorTable(NRGBs, stops, red, green, blue, NCont);
    gStyle->SetNumberContours(NCont);
}


void bkgd::bkgd_pe_ana()
{
    TTree::SetMaxTreeSize(Long64_t(1024)*1024*1024*200); //200 GB tree
    Double_t asymmetry = 0.0;
    Double_t rate = 0.0;
    remollEvent_t *fEvent = 0;
          /*double A, Am;
          double xs;
          double Q2, W2;
          double thcom;
          double beamp;*/
    std::vector < remollGenericDetectorHit_t > *fHit = 0;
    std::vector < remollEventParticle_t > *fPart = 0;
    int dotPos = fileString.rfind(".");   
    std::ostringstream os;
    os << anaStr << "_" << fileString.substr(0, dotPos) << "_PEs_"<<"det_"<<detid<<".root";
    std::string fileName = os.str();
    TFile *old = new TFile(fileString.c_str());
    TTree *oldTree = (TTree*)old->Get("T");
    TFile *newFile = new TFile(fileName.c_str(),"RECREATE", "", 1);

    TTree* newTree = new TTree("T", "Tree of detector hits");
    oldTree->SetBranchAddress("ev", &fEvent); 
    oldTree->SetBranchAddress("rate", &rate); 
    oldTree->SetBranchAddress("hit", &fHit); 
    oldTree->SetBranchAddress("part", &fPart); 
    std::vector < simPEs_t > *simPEs = new std::vector < simPEs_t > ;
    std::vector < simPEs_t > *simRPEs = new std::vector < simPEs_t > ;
    std::vector < simPEs_t > *simAPEs = new std::vector < simPEs_t > ;
    std::vector < catPEs_t > *catPEs = new std::vector < catPEs_t > ;
    std::vector < Q_t > *Q = new std::vector < Q_t > ;
    std::vector < Ref_t > *Ref = new std::vector < Ref_t > ;
    std::vector < RefX_t > *RefX = new std::vector < RefX_t > ;
    std::vector < Refair_t > *Refair = new std::vector < Refair_t > ;
    std::vector < LG_t > *LG = new std::vector < LG_t > ;
    std::vector < LGair_t > *LGair = new std::vector < LGair_t > ;
    std::vector < PMTcat_t > *PMTcat = new std::vector < PMTcat_t > ;
    std::vector < PMTbulk_t > *PMTbulk = new std::vector < PMTbulk_t > ;
    std::vector < elseX_t > *elseX = new std::vector < elseX_t > ;
    std::vector<int> eTRID;  
    std::vector<int> DETID;
    std::vector<int> PID;
    std::vector<int> MTRID;
    std::vector<int> peTRID;
    double peNumber;
    double peRateWeighting;
    double peAsymmetryWeighting;
                
    int Qcounted = 0;
    int Refaircounted = 0;
    int LGaircounted = 0;
    int PMTbulkcounted = 0;
    int detSourcedPEs = 0;
	double peN_entries = 0;

    //TODO reading data into envelopes downstream could be sped up
    //by storing the data by Z instead of by hit
    
    newTree->Branch("nentries", &peN_entries);
    newTree->Branch("simpes", &simPEs);
    newTree->Branch("simRpes", &simRPEs);
    newTree->Branch("simApes", &simAPEs);
    newTree->Branch("catpes", &catPEs);
    newTree->Branch("q", &Q);
    newTree->Branch("ref", &Ref);
    newTree->Branch("refx", &RefX);
    newTree->Branch("refair", &Refair);
    newTree->Branch("lg", &LG);
    newTree->Branch("lgair", &LGair);
    newTree->Branch("pmtcat", &PMTcat);
    newTree->Branch("pmtbulk", &PMTbulk);
    newTree->Branch("else", &elseX);

    Double_t lookup = 0.0;
    Int_t lookupAngle = 0;
    Int_t lookupXpos = 0;

    for (size_t j = 0; j < oldTree->GetEntries(); j++)
    {
        if (j%10000 == 0) 
        {
            std::cerr << "\r" <<  j << "/" << oldTree->GetEntries() << " - " << (j*1.0)/oldTree->GetEntries() * 100 << "%";
        }
     
        oldTree->GetEntry(j);
        remollEvent_t event = *fEvent;
        //std::cout << "Hits: " << fHit->size() << std::endl;
        //std::cout << "Parts: " << fPart->size() << std::endl;
        //std::vector<double> x, y, z, px, py, pz, p, e, m, vx, vy, vz;
        std::vector<double> cathitx, cathity, cathitz;
        // Log which electron track IDs (and other info) hit which detectors, and which PE IDs hit the cathode and where
        // Attribute all npe counts and cathode hit positions to all DETID branches that are hit during that event (in lieu of getting photon birth vertex properly down)
        // int npe = 0; // do this as peTRID->Size();

        for (size_t i = 0; i < fHit->size(); i++)
        {
            remollGenericDetectorHit_t hit = fHit->at(i); 
            //Count each optical photon hit in an entry, iff the primary electron hits a detector, also sum accidentals
            // Make a root tree out of the amounts (and locations) of photon hits on the various detectors
            // Each branch is a different detector that the electron can hit (quartz, wall, air, cathode, PMT, etc.)
            //  Store the electron hit location and energy, etc. for that hit
            //  Store the total number of photons that hit the cathode after that electron hit, and their cathode hit info
            
            // FIXME remove this condition to get results for ALL SIGNAL of any form, not just parent electron hits
            if (anaStr == "backgrounds" && (hit.det==detid+4 || hit.det == detid+5) && hit.pid == 11 && hit.mtrid == 0){ // Then this is our primary signal of interest
                // if you do mtrid == 1 then you get the delta rays! About a 1% contribution 
                eTRID.push_back(hit.trid);
                DETID.push_back(hit.det);
                // FIXME Assumes the detector is alligned along the x axis and so are all particles!! Fails for backscattered particles
                lookupAngle = lookupTable->GetXaxis()->FindBin((180.0/TMath::Pi())*asin(((hit.px*abs(hit.x) + hit.py*abs(hit.y))/hit.r)/hit.p));  // Maintain sign convention on angles here... was just hit.px/hit.p before...
                lookupXpos = lookupTable->GetYaxis()->FindBin(hit.r);
                lookup = lookupTable->GetBinContent(lookupAngle,lookupXpos);
                peNumber += lookup;
                peRateWeighting += rate*lookup/(current*degeneracy);
                peAsymmetryWeighting += event.A*rate*lookup/(current*degeneracy);
                if ( lookup == 0.0 ) {
                    std::cout << "0 PEs" << std::endl;
                }
                std::cout << anaStr << " PE Hit, rate = " << rate/(current*degeneracy) << ", Asymmetry = " << event.A << ", lookup pe value = " << lookup << ", bin number " << lookupTable->GetBin(lookupAngle,lookupXpos) << ", " << lookupAngle << ", " << lookupXpos << ", angle = " << (180.0/TMath::Pi())*asin(((hit.px*abs(hit.x) + hit.py*abs(hit.y))/hit.r)/hit.p) << ", abs(x_pos) = " << hit.r << ", which yields: " << peRateWeighting << std::endl;
            }
            if (anaStr == "signals" && (hit.det==detid+1) && hit.pid == 11 && hit.mtrid == 0){ // Then this is our primary signal of interest
                // if you do mtrid == 1 then you get the delta rays! About a 1% contribution 
                eTRID.push_back(hit.trid);
                DETID.push_back(hit.det);
                lookup = 15.0;
                if (detName == "R5o") lookup = 25.0;
                peNumber += lookup;
                peRateWeighting += rate*lookup/(current*degeneracy);
                peAsymmetryWeighting += event.A*rate*lookup/(current*degeneracy);
            }
            if (hit.det == detid+1) {
                // If any particle hits the quartz detector then tell the particle ID and mother ID (so we can keep track of deltas) 
                PID.push_back(hit.pid);
                MTRID.push_back(hit.mtrid);
            }
            if (hit.pid == 0 && hit.det == detid){
                // This is an optical photon and we want to count ++ if it hits the PMT cathode
                peTRID.push_back(hit.trid);
                cathitx.push_back(hit.x);
                cathity.push_back(hit.y);
                cathitz.push_back(hit.z);
            }
        }

        for (size_t i = 0; i < fHit->size();i++)
        {
            remollGenericDetectorHit_t hit = fHit->at(i);
            for (size_t k = 0; k < eTRID.size(); k++)
            {
                if (hit.trid == eTRID.at(k)) { // Then this electron hit a detector we should know about and store all of the info and cathode hits too
                    //std::cout << "electron TRID " << hit.trid << std::endl;
                    // Make the electron hit info go into the appropriate detector branch
                    // quartz
                    if (hit.det == detid+1 && Qcounted==0) {     
                        Q->push_back(QTrim(hit,peTRID.size(),cathitx,cathity,cathitz)); 
                        Qcounted++;
                    }
                    // reflector air
                    if (hit.det == detid+2 && Refaircounted==0) { 
                        Refair->push_back(RefairTrim(hit,peTRID.size(),cathitx,cathity,cathitz)); 
                        Refaircounted++;
                    }
                    // reflector skin
                    if (hit.det == detid+3) { 
                        Ref->push_back(RefTrim(hit,peTRID.size(),cathitx,cathity,cathitz)); 
                    }
                    // reflector volume
                    if (hit.det == detid+4) { 
                        RefX->push_back(RefXTrim(hit,peTRID.size(),cathitx,cathity,cathitz));  
                    }
                    // light guide air
                    if (hit.det == detid+6 && LGaircounted==0) { 
                        LGair->push_back(LGairTrim(hit,peTRID.size(),cathitx,cathity,cathitz));   
                        LGaircounted++;
                    }
                    // light guide skin
                    if (hit.det == detid+5) { 
                        LG->push_back(LGTrim(hit,peTRID.size(),cathitx,cathity,cathitz));   
                    }
                    // PMT bulk
                    if (hit.det == detid+7 && PMTbulkcounted==0) {
                        PMTbulk->push_back(PMTbulkTrim(hit,peTRID.size(),cathitx,cathity,cathitz));   
                        PMTbulkcounted++;
                    }
                    // PMT cathode
                    if (hit.det == detid) { 
                        PMTcat->push_back(PMTcatTrim(hit,peTRID.size(),cathitx,cathity,cathitz));   
                    }
                    //break; //how is this useful?? I want to muliple count
                }
            }   
        }
        simPEs->push_back(simPEsTrim(DETID,PID,MTRID,peNumber,cathitx,cathity,cathitz)); 
        simRPEs->push_back(simPEsTrim(DETID,PID,MTRID,peRateWeighting,cathitx,cathity,cathitz)); 
        simAPEs->push_back(simPEsTrim(DETID,PID,MTRID,peAsymmetryWeighting,cathitx,cathity,cathitz)); 
        catPEs->push_back(catPEsTrim(DETID,PID,MTRID,(int)peTRID.size(),cathitx,cathity,cathitz)); 
        detSourcedPEs=(int)peTRID.size();
        elseX->push_back(elseXTrim(((int)peTRID.size()-detSourcedPEs),cathitx,cathity,cathitz));
        peN_entries = oldTree->GetEntries();
        // FIXME old logic requires hits on the cathode... new flat_parallel detecor just wants any hit
        // if (catPEs->size() > 0){}
        if (simPEs->size() > 0){
            newTree->Fill();
	    }
        peNumber=0.0;
        peRateWeighting=0.0;
        peAsymmetryWeighting=0.0;
        Qcounted=0;
        Refaircounted=0;
        LGaircounted=0;
        PMTbulkcounted=0;
        detSourcedPEs=0;
        eTRID.clear();
        catPEs->clear();
        simPEs->clear();
        simRPEs->clear();
        simAPEs->clear();
        elseX->clear();
        Q->clear();
        Refair->clear();
        Ref->clear();
        RefX->clear();
        LGair->clear();
        LG->clear();
        PMTbulk->clear();
        PMTcat->clear();
        cathitx.clear();
        cathity.clear();
        cathitz.clear();
        DETID.clear();
        PID.clear();
        MTRID.clear();
        peTRID.clear();
    }
    newFile = newTree->GetCurrentFile();
    newTree->Write("", TObject::kOverwrite);
    newTree->Print();
    old->Close();
    newFile->Close();
}

//void pePlots(int argcC, char **argvC, std::string fileString="tracking.root", int detid=540210, std::string variable="reflectorAngle", double varVal=11.5, std::string unit="deg")
void bkgd::bkgd_pePlots(int argcC, char **argvC)
{ 
    //TApplication theApp("App",&argcC,argvC);

    int dotpos = fileString.rfind(".root");   
    std::ostringstream os;
    std::ostringstream os2;
    os << anaStr << "_" << fileString.substr(0, dotpos) << "_PEs_"<<"det_"<<detid<<".root";
    os2 << anaStr << "_" << fileString.substr(0, dotpos) << "_PEs_"<<"det_"<<detid<<"_plots.root";
    std::string fileName = os.str();
    std::string fileNamePlots = os2.str();

    TChain * Tmol =new TChain("T");
    TString ttemp = TString(fileName);
    Tmol->Add(ttemp);
    TFile *plotsFile = new TFile(fileNamePlots.c_str(),"RECREATE", "", 1);
    TTree* plotsTree = new TTree("T", "Tree of plots");
    plotsFile->cd(); // Is this necessary?
    //int nentries = (Int_t)newTree->GetEntries();
    //
    Tmol->SetBranchAddress("nentries", &N_entries);
    if (Tmol) {
        Tmol->GetEntry(0);
    }
    std::fstream file_out_rms;
    std::fstream file_out_integral;
    std::ofstream file_out_mean;
    std::ofstream file_out_res;
    file_out_integral.open("int.csv",std::ofstream::out | std::ofstream::app);
    file_out_rms.open("rms.csv",std::ofstream::out | std::ofstream::app);
    file_out_mean.open("mean.csv",std::ofstream::out | std::ofstream::app);
    file_out_res.open("res.csv",std::ofstream::out | std::ofstream::app);

    //gROOT->SetStyle("Plain");
    //gStyle->SetOptStat(0); 
    gStyle->SetOptStat("eMR");
    gStyle->SetNumberContours(255);

    set_plot_style();

    const int n_plots = 15;
    //TH1 *Histo[n_plots];
    double integral[n_plots];
    double RMS[n_plots];
    double RMSerror[n_plots];
    double Mean[n_plots];
    double Meanerror[n_plots];
    TCanvas * c1[n_plots];

    std::string names[n_plots]={
                            "Simulated Total Cathode Spectrum per event",
                            "Simulated Rate-weighted Total Cathode Spectrum per event",
                            "Simulated Asym-weighted Total Cathode Spectrum per event",
                            "Total Cathode Spectrum per event",
                            "Cathode Spectrum from primary signal quartz electrons only",
                            "Cathode Spectrum from quartz deltas",
                            "Cathode Spectrum from all non-primary quartz signals",
                            "e- hit radial hit spectrum on the reflector", // Consider adding photon xyz tracking too - would have to go beside the cathode xyz tracking feature
                            "e- hit radial hit spectrum on the quartz-reflector holder",
                            "Cathode Spectrum from the reflector air",
                            "e- hit radial spectrum on the light guide walls",
                            "Cathode Spectrum from the light guide air",
                            "e- hit radial spectrum in the PMT bulk",
                            "e- hit spectrum in the cathode",
                            "Cathode Spectrum from elsewhere"}; 
    std::string draws[n_plots]={
                            "simpes.npes",
                            "simRpes.npes",
                            "simApes.npes",
                            "catpes.npes",
                            "catpes.npes",
                            "catpes.npes",
                            "catpes.npes",
                            "ref.r",
                            "refx.r",
                            "refair.npes",
                            "lg.r",
                            "lgair.npes",
                            "pmtbulk.r",
                            "pmtcat.r",
                            "else.npes"};
    std::string cuts[n_plots]={
                            "simRpes.npes*(1==1)",
                            "",
                            "",
                            "",
                            Form("catpes.detids==%d && catpes.pids==11 && catpes.mtrids==0",detid),
                            Form("catpes.detids==%d && abs(catpes.pids)==11 && catpes.mtrids!=0",detid),
                            Form("catpes.detids==%d && catpes.mtrids!=0",detid),
                            "ref.npes*(ref.npes!=0)",
                            "refx.npes*(refx.npes!=0)",
                            "",
                            "lg.npes*(lg.npes!=0)",
                            "",
                            "",
                            "",
                            ""};
    std::string drawOpts[n_plots]={
                            "",
                            "",
                            "",
                            "",
                            "",
                            "",
                            "",
                            "",
                            "",
                            "",
                            "",
                            "",
                            "",
                            "",
                            ""};
    std::string xTitle[n_plots]={
                                 "Simulated PEs",
                                 "Simulated Rate-weighted PEs",
                                 "Simulated Asym-weighted PEs",
                                 "PEs",
                                 "PEs",
                                 "PEs",
                                 "PEs",
                                 "PMT spectrum vs. Radial Hit Position (mm)",
                                 "PMT spectrum vs. Radial Hit Position (mm)",
                                 "PEs",
                                 "PMT spectrum vs. Radial Hit Position (mm)",
                                 "PEs",
                                 "Radial Hit Position (mm)",
                                 "Radial Hit Position (mm)",
                                 "PEs"};
    std::string yTitle[n_plots]={
                                 "Spectrum",
                                 "Spectrum",
                                 "Spectrum",
                                 "Spectrum",
                                 "Spectrum",
                                 "Spectrum",
                                 "Spectrum",
                                 "Counts Spectrum",
                                 "Counts Spectrum",
                                 "Spectrum",
                                 "Counts Spectrum",
                                 "Spectrum",
                                 "Counts",
                                 "Counts",
                                 "Spectrum"};
    /*
    int nbins[n_plots]={
                        100,
                        10000,
                        10000,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100};
    double lowbin[n_plots]={
                        0.0,
                        0.0,
                        0.0,
                        0.0,
                        0.0,
                        0.0,
                        0.0,
                        900.0,
                        900.0,
                        0.0,
                        950.0,
                        0.0,
                        1050.0,
                        1050.0,
                        0.0};
    double highbin[n_plots]={
                        10.0,
                        1.0e9.
                        1.0e9,
                        100.0,
                        100.0,
                        100.0,
                        100.0,
                        1200.0,
                        1200.0,
                        100.0,
                        1200.0,
                        100.0,
                        1400.0,
                        1250.0,
                        100.0};
    */


    //for (int p=0;p<n_plots;p++)
    for (int p=0;p<4;p++){
        c1[p]=new TCanvas(Form("%s_c%02d",names[p].c_str(),p),Form("canvas_%s_%02d",names[p].c_str(),p),1500,1500);
        c1[p]->cd();
        if (p < 3) c1[p]->SetLogy();
        //Histo[p]=new TH1F(Form("Histo[%d]",p),Form("%s; %s; %s",names[p].c_str(),xTitle[p].c_str(),yTitle[p].c_str()),nbins[p],lowbin[p],highbin[p]);
        //Tmol->Draw(Form("%s>>Histo[%d]",draws[p].c_str(),p),Form("%s",cuts[p].c_str()));
        Tmol->Draw(Form("%s",draws[p].c_str(),p),Form("%s",cuts[p].c_str()),Form("%s",drawOpts[p].c_str()));
        TH1* htmp = (TH1*)gROOT->FindObject("htemp");

        htmp->SetName(Form("Histo[%d]",p));
        htmp->SetTitle(Form("%s",names[p].c_str()));
        htmp->SetXTitle(Form("%s",xTitle[p].c_str()));
        htmp->SetYTitle(Form("%s",yTitle[p].c_str()));

        htmp->SetStats(1221);
        integral[p] = 1.0*htmp->Integral();
        RMS[p] = 1.0*htmp->GetRMS();
        RMSerror[p] = 1.0*htmp->GetRMSError();
        Mean[p] = 1.0*htmp->GetMean();
        Meanerror[p] = 1.0*htmp->GetMeanError();

        //Plot_Name,x_axis_units,x_number,y_number,y_uncertainty
        // user_reflectivity = 0.9, double user_cerenkov = 1.0, double user_scintillation = 1.0, double user_z_pos = -11.0
        if (p==0){ //then we simulated PEs on the photocathode
            //file_out_rms<<names[p]<<anaStr<<","<<fileString<<" - RMS,"<<RMS[p]<<","<<RMSerror[p]<<std::endl;
            //std::cout<<names[p]<<anaStr<<","<<fileString<<" - Mean,"<<Mean[p]<<","<<Meanerror[p]<<std::endl;
            //file_out_mean<<names[p]<<anaStr<<","<<fileString<<" - Mean,"<<Mean[p]<<","<<Meanerror[p]<<std::endl;
            //file_out_res<<names[p]<<anaStr<<","<<fileString<<" - Resolution = RMS/Mean,"<<(RMS[p]/Mean[p])<<","<<(RMS[p]/Mean[p])*sqrt((RMSerror[p]*RMSerror[p])+(Meanerror[p]*Meanerror[p]))<<std::endl;


            double ref_x_pos = user_x_pos;
            double ref_angle = user_angle;
            double ref_reflectivity = user_reflectivity;
            double ref_cerenkov = user_cerenkov;
            double ref_scintillation = user_scintillation;
            double ref_z_pos = user_z_pos;

            double oldx_pos = 0.0;
            double oldangle = 0.0;
            double oldavg     = 0.0;
            double oldavg_err = 0.0;
            double oldrms     = 0.0;
            double oldrms_err = 0.0;
            double oldres     = 0.0;
            double oldN_en    = 0.0;
            double oldreflectivity = 0.0;
            double oldcerenkov = 0.0;
            double oldscintillation = 0.0;
            double oldz_pos = 0.0;

            double x_pos = 0.0;
            double angle = 0.0;
            double avg     = 0.0;
            double avg_err = 0.0;
            double rms     = 0.0;
            double rms_err = 0.0;
            double res     = 0.0;
            double N_en    = 0.0;
            double reflectivity = 0.0;
            double cerenkov = 0.0;
            double scintillation = 0.0;
            double z_pos = 0.0;

            std::cout << "X = " << ref_x_pos << ", angle = " << ref_angle <<std::endl;

            TFile new_file(Form("localTmp.root"),"recreate");
            new_file.cd();
            TTree* newtree;

            TFile* old_file;
            TTree* oldtree;
            if (!gSystem->AccessPathName("scans.root")) {
                // Old file exists, read it and add new entries
                old_file = TFile::Open("scans.root");
                old_file->GetObject("scans", oldtree);
                new_file.cd();
                if (!oldtree) {
                    std::cout << "ERROR: Dead scans tree" ;
                    return;
                }
                newtree = oldtree->CloneTree(0);
                int nent = oldtree->GetEntries();

                //        TLeaf* angleL = oldtree->GetLeaf("angle");
                //        TLeaf* x_posL = oldtree->GetLeaf("x_pos");

                // Clear out prior instance if exists
                bool prior = true;
                if (oldtree->GetBranch("reflectivity")) {
                    oldtree->SetBranchAddress("reflectivity",&oldreflectivity);
                    newtree->SetBranchAddress("reflectivity",&reflectivity);
                }
                else {
                    newtree->Branch("reflectivity",&reflectivity);
                    prior = false;
                }
                if (oldtree->GetBranch("cerenkov")) {
                    oldtree->SetBranchAddress("cerenkov",&oldcerenkov);
                    newtree->SetBranchAddress("cerenkov",&cerenkov);
                }
                else {
                    newtree->Branch("cerenkov",&cerenkov);
                    prior = false;
                }
                if (oldtree->GetBranch("scintillation")) {
                    oldtree->SetBranchAddress("scintillation",&oldscintillation);
                    newtree->SetBranchAddress("scintillation",&scintillation);
                }
                else {
                    newtree->Branch("scintillation",&scintillation);
                    prior = false;
                }
                if (oldtree->GetBranch("z_pos")) {
                    oldtree->SetBranchAddress("z_pos",&oldz_pos);
                    newtree->SetBranchAddress("z_pos",&z_pos);
                }
                else {
                    newtree->Branch("z_pos",&z_pos);
                    prior = false;
                }
                oldtree->SetBranchAddress("angle",&oldangle);
                oldtree->SetBranchAddress("x_pos",&oldx_pos);
                oldtree->SetBranchAddress("avg_pes",&oldavg);
                oldtree->SetBranchAddress("avg_pes_err",&oldavg_err);
                oldtree->SetBranchAddress("rms_pes",&oldrms);
                oldtree->SetBranchAddress("rms_pes_err",&oldrms_err);
                oldtree->SetBranchAddress("res",&oldres);
                oldtree->SetBranchAddress("nentries",&oldN_en);
                newtree->SetBranchAddress("angle",&angle);
                newtree->SetBranchAddress("x_pos",&x_pos);
                newtree->SetBranchAddress("avg_pes",&avg);
                newtree->SetBranchAddress("avg_pes_err",&avg_err);
                newtree->SetBranchAddress("rms_pes",&rms);
                newtree->SetBranchAddress("rms_pes_err",&rms_err);
                newtree->SetBranchAddress("res",&res);
                newtree->SetBranchAddress("nentries",&N_en);
                for (int j = 0 ; j < nent ; j++ ) {
                    //            x_posL->GetBranch()->GetEntry(j);
                    //            angleL->GetBranch()->GetEntry(j);
                    oldtree->GetEntry(j);

                    if (ref_x_pos == oldx_pos && ref_angle == oldangle && (!prior || (ref_reflectivity == oldreflectivity && ref_cerenkov == oldcerenkov && ref_scintillation == oldscintillation && ref_z_pos == oldz_pos))) {
                        //if (ref_x_pos == x_posL->GetValue() && ref_angle == angleL->GetValue()) 
                        std::cout << "TEST 1" << std::endl;
                        continue;
                    }
                    x_pos   = oldx_pos;
                    angle   = oldangle;
                    avg     = oldavg;
                    avg_err = oldavg_err;
                    rms     = oldrms;
                    rms_err = oldrms_err;
                    res     = oldres;
                    N_en    = oldN_en;
                    reflectivity = oldreflectivity;
                    cerenkov = oldcerenkov;
                    scintillation = oldscintillation;
                    z_pos = oldz_pos;
                    if (!oldtree->GetBranch("reflectivity")) {
                        reflectivity = 0.9;
                    }
                    if (!oldtree->GetBranch("cerenkov")) {
                        cerenkov = 1.0;
                    }
                    if (!oldtree->GetBranch("scintillation")) {
                        scintillation = 1.0;
                    }
                    if (!oldtree->GetBranch("z_pos")) {
                        z_pos = -11.0;
                    }
                    newtree->Fill();
                }

                // Append current run to end
                old_file->Close();
                gSystem->Exec("rm scans.root");
                delete old_file;
            }
            else {
                // Old file doesn't exist, make a new one
                new_file.cd();
                newtree = new TTree("scans","scans");

                // Write new tree
                newtree->Branch("angle",&angle);
                newtree->Branch("x_pos",&x_pos);
                newtree->Branch("avg_pes",&avg);
                newtree->Branch("avg_pes_err",&avg_err);
                newtree->Branch("rms_pes",&rms);
                newtree->Branch("rms_pes_err",&rms_err);
                newtree->Branch("res",&res);
                newtree->Branch("nentries",&N_en);
                newtree->Branch("reflectivity",&reflectivity);
                newtree->Branch("cerenkov",&cerenkov);
                newtree->Branch("scintillation",&scintillation);
                newtree->Branch("z_pos",&z_pos);
            }

            newtree->SetBranchAddress("angle",&angle);
            newtree->SetBranchAddress("x_pos",&x_pos);
            newtree->SetBranchAddress("avg_pes",&avg);
            newtree->SetBranchAddress("avg_pes_err",&avg_err);
            newtree->SetBranchAddress("rms_pes",&rms);
            newtree->SetBranchAddress("rms_pes_err",&rms_err);
            newtree->SetBranchAddress("res",&res);
            newtree->SetBranchAddress("nentries",&N_en);
            newtree->SetBranchAddress("reflectivity",&reflectivity);
            newtree->SetBranchAddress("cerenkov",&cerenkov);
            newtree->SetBranchAddress("scintillation",&scintillation);
            newtree->SetBranchAddress("z_pos",&z_pos);

            angle   = user_angle;
            x_pos   = user_x_pos;
            file_out_integral << names[p] << ", " << anaStr << ", " << fileString << ", " << detName << ", " << detid << ", Integral, "<<integral[p]<<",0"<<std::endl;
            file_out_rms << names[p] << ", " << anaStr << ", " << fileString << ", " << detName << ", " << detid << ", RMS, "<<RMS[p]<<","<<RMSerror[p]<<std::endl;
            std::cout << names[p] << ", " << anaStr << ", " << fileString << ", " << detName << ", " << detid << ", Mean, "<<Mean[p]<<","<<Meanerror[p]<<std::endl;
            file_out_mean << names[p] << ", " << anaStr << ", " << fileString << ", " << detName << ", " << detid << ", Mean, "<<Mean[p]<<","<<Meanerror[p]<<std::endl;
            file_out_res << names[p] << ", " << anaStr << ", " <<fileString << ", " << detName << ", " << detid << ", Resolution = RMS/Mean, "<<(RMS[p]/Mean[p])<<","<<(RMS[p]/Mean[p])*sqrt((RMSerror[p]*RMSerror[p])+(Meanerror[p]*Meanerror[p]))<<std::endl;

            avg     = Mean[p];
            avg_err = Meanerror[p];
            rms     = RMS[p];
            rms_err = RMSerror[p];
            res     = Mean[p]/RMS[p];
            N_en    = (double)N_entries;

            reflectivity = user_reflectivity;
            cerenkov = user_cerenkov;
            scintillation = user_scintillation;
            z_pos = user_z_pos;

            std::cout << "TEST 2 X = " << x_pos << ", angle = " << angle <<std::endl;
            new_file.cd();
            newtree->Fill();
            newtree->Write("scans",TObject::kOverwrite);
            new_file.Close();

            gSystem->Exec("mv localTmp.root scans.root");

        }
        plotsFile->cd();
        c1[p]->Write();
        c1[p]->SaveAs(Form("%s_%s_%d_%d.png",fileString.substr(0,fileString.find(".root")).c_str(),anaStr.c_str(),detid,p));
    }

    file_out_integral.close();
    file_out_rms.close();
    file_out_mean.close();
    file_out_res.close();

    plotsFile = plotsTree->GetCurrentFile();
    plotsTree->Write("", TObject::kOverwrite);
    plotsTree->Print();
    plotsFile->Close();
    //theApp.Run();
}

int bkgd::do_pe(int argc, char **argv)
{
    if (argc <= 1 || argc > 10)
    {
        std::cerr << "Usage: ./bkgd_pe char*:filename int:detNumber char*:detName char*:analysis (backgrounds or signals) double:reflectivity double:cerenkov double:scintillation double:beamCurrent double:degeneracy (Number of duplicate sample jobs)" << std::endl;
        //std::cerr << "Usage: ./pe char*:filename int:detid char*:manipulateVariable float:variableValue bool:reanalyze(y or n)" << std::endl;
        exit(0);
    }
    if (argc >= 2) 
    {
        std::string fileName(argv[1]); 
        fileString = fileName;
    }
    if (argc >= 3)
    {
        detid = atoi(argv[2]);    
    }
    if (argc >= 4)
    {
        std::string det_Name(argv[3]); 
        detName = det_Name;
    }
    if (argc >= 5)
    {
        std::string ana(argv[4]);
        anaStr = ana;
    }
    if (argc >= 6)
    {
        user_reflectivity = atof(argv[5]);    
    }
    if (argc >=7)
    {
        user_cerenkov = atof(argv[6]);    
    }
    if (argc >=8)
    {
        user_scintillation = atof(argv[7]);    
    }
    if (argc >=9)
    {
        current = atof(argv[8]);    
    }
    if (argc >=10)
    {
        degeneracy = atof(argv[9]);    
    }

    int success = get_lookuptable();
    if (!success){
        return 0;
    }
    if (reana == true) {
        std::cout << "Running with file=" << fileString << ", analysis=" << anaStr << ", detid=" << detid << std::endl; 
        bkgd_pe_ana();
    }
    if (argc >=1){
        std::cout << "Plotting previously analyzed file=" << fileString << " + " << anaStr << " + PEs_det_" << detid << ".root" << std::endl; 
        bkgd_pePlots(argc, argv);
    }
    return 1;
}

int main(int argc, char **argv)
{
    bkgd ana;
    return ana.do_pe(argc, argv);
}
