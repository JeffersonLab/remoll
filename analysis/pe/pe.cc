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
#include "petypes.hh"
#include "pe.hh"

#define pi 3.141592653589793238462643383279502884L

void set_plot_style()
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


void pe(std::string file="tracking.root", int detid=50001)
{
    TTree::SetMaxTreeSize(Long64_t(1024)*1024*1024*200); //200 GB tree
    std::vector < remollGenericDetectorHit_t > *fHit = 0;
    std::vector < remollEventParticle_t > *fPart = 0;
    int dotPos = file.rfind(".");   
    std::ostringstream os;
    os << file.substr(0, dotPos) << "_PEs_"<<"det_"<<detid<<".root";
    std::string fileName = os.str();
    TFile *old = new TFile(file.c_str());
    TTree *oldTree = (TTree*)old->Get("T");
    TFile *newFile = new TFile(fileName.c_str(),"RECREATE", "", 1);

    TTree* newTree = new TTree("T", "Tree of detector hits");
    oldTree->SetBranchAddress("hit", &fHit); 
    oldTree->SetBranchAddress("part", &fPart); 
    std::vector < catPEs_t > *catPEs = new std::vector < catPEs_t > ;
    std::vector < catPEs_t > *refPEs = new std::vector < catPEs_t > ;
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
    std::vector<int> refTRID;
    std::vector<int> DETID;
    std::vector<int> PID;
    std::vector<int> MTRID;
    std::vector<int> peTRID;
    int refHit = 0;
    int Qcounted = 0;
    int Refaircounted = 0;
    int LGaircounted = 0;
    int PMTbulkcounted = 0;
    int detSourcedPEs = 0;
	double N_entries = 0;

    //TODO reading data into envelopes downstream could be sped up
    //by storing the data by Z instead of by hit
    
    newTree->Branch("nentries", &N_entries);
    newTree->Branch("catpes", &catPEs);
    newTree->Branch("refpes", &refPEs);
    newTree->Branch("q", &Q);
    newTree->Branch("ref", &Ref);
    newTree->Branch("refx", &RefX);
    newTree->Branch("refair", &Refair);
    newTree->Branch("lg", &LG);
    newTree->Branch("lgair", &LGair);
    newTree->Branch("pmtcat", &PMTcat);
    newTree->Branch("pmtbulk", &PMTbulk);
    newTree->Branch("else", &elseX);
    for (size_t j = 0; j < oldTree->GetEntries(); j++)
    {
        if (j%10000 == 0) 
        {
            std::cerr << "\r" <<  j << "/" << oldTree->GetEntries() << " - " << (j*1.0)/oldTree->GetEntries() * 100 << "%";
        }
     
        refHit = 0;
        oldTree->GetEntry(j);
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
            
            if (hit.pid == 11 && hit.mtrid == 0){ // Then this is our primary signal of interest
            // if you do mtrid == 1 then you get the delta rays! About a 1% contribution 
                eTRID.push_back(hit.trid);
                DETID.push_back(hit.det);
                if (i<30 && hit.det == 50001){
                }
            }
            if (hit.det == detid+1) {
                // If any particle hits the quartz detector then tell the particle ID and mother ID (so we can keep track of deltas) 
                PID.push_back(hit.pid);
                MTRID.push_back(hit.mtrid);
            }
            if (hit.pid == 0 && hit.det == detid+4){
                refTRID.push_back(hit.trid);
            }
            if (hit.pid == 0 && hit.det == detid){
                // This is an optical photon and we want to count ++ if it hits the PMT cathode
                peTRID.push_back(hit.trid);
                cathitx.push_back(hit.x);
                cathity.push_back(hit.y);
                cathitz.push_back(hit.z);
            }
        }

        std::cout << "Event number " << j << std::endl;
        for (size_t i = 0; i < fHit->size();i++)
        {
            remollGenericDetectorHit_t hit = fHit->at(i);
            for (size_t k = 0; k < refTRID.size(); k++)
            {
                std::cout<< "Checking reflector hit track ID = " << refTRID.at(k) << " Against hit track ID = " << hit.trid << std::endl;
                if (hit.pid==0 && hit.trid == refTRID.at(k)) {
                    std::cout<< "Reflector hit - Match found" << std::endl;
                    for (size_t l = 0; l < peTRID.size(); l++)
                    {
                        std::cout<< "Checking cathode hit track ID = " << peTRID.at(l) << " Against hit track ID = " << hit.trid << std::endl;
                        if (hit.trid == peTRID.at(l)) {
                            std::cout<< "Cathode hit - Match found" << std::endl;
                            refHit++;
                        }
                    }
                }
            }
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
        if (refHit>=1) {
            refPEs->push_back(catPEsTrim(DETID,PID,MTRID,refHit,cathitx,cathity,cathitz)); 
        }
        else {
            refPEs->push_back(catPEsTrim(DETID,PID,MTRID,0,cathitx,cathity,cathitz)); 
        }
        refHit=0;
        catPEs->push_back(catPEsTrim(DETID,PID,MTRID,(int)peTRID.size(),cathitx,cathity,cathitz)); 
        detSourcedPEs=(int)peTRID.size();
        elseX->push_back(elseXTrim(((int)peTRID.size()-detSourcedPEs),cathitx,cathity,cathitz));
        N_entries = oldTree->GetEntries();
        if (catPEs->size() > 0){
            newTree->Fill();
	    }
        Qcounted=0;
        Refaircounted=0;
        LGaircounted=0;
        PMTbulkcounted=0;
        detSourcedPEs=0;
        eTRID.clear();
        refTRID.clear();
        refPEs->clear();
        catPEs->clear();
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

//void pePlots(int argcC, char **argvC, std::string fileP="tracking.root", int detid=50001, std::string variable="reflectorAngle", double varVal=11.5, std::string unit="deg")
void pePlots(int argcC, char **argvC, std::string fileP="tracking.root", int detid=50001, double user_angle = 0.0, double user_x_pos = 0.0, double user_reflectivity = 0.9, double user_cerenkov = 1.0, double user_scintillation = 1.0, double user_z_pos = -11.0)
{ 
    //TApplication theApp("App",&argcC,argvC);

    int dotpos = fileP.rfind(".root");   
    std::ostringstream os;
    std::ostringstream os2;
    os << fileP.substr(0, dotpos) << "_PEs_"<<"det_"<<detid<<".root";
    os2 << fileP.substr(0, dotpos) << "_PEs_"<<"det_"<<detid<<"_plots.root";
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
    double N_entries = 0;
    Tmol->SetBranchAddress("nentries", &N_entries);
    if (Tmol) {
        Tmol->GetEntry(0);
    }
    std::fstream file_out_ref_rms;
    std::ofstream file_out_ref_mean;
    std::ofstream file_out_ref_res;
    file_out_ref_rms.open("ref_rms.csv",std::ofstream::out | std::ofstream::app);
    file_out_ref_mean.open("ref_mean.csv",std::ofstream::out | std::ofstream::app);
    file_out_ref_res.open("ref_res.csv",std::ofstream::out | std::ofstream::app);
    std::fstream file_out_rms;
    std::ofstream file_out_mean;
    std::ofstream file_out_res;
    file_out_rms.open("rms.csv",std::ofstream::out | std::ofstream::app);
    file_out_mean.open("mean.csv",std::ofstream::out | std::ofstream::app);
    file_out_res.open("res.csv",std::ofstream::out | std::ofstream::app);

    //gROOT->SetStyle("Plain");
    //gStyle->SetOptStat(0); 
    gStyle->SetOptStat("eMR");
    gStyle->SetNumberContours(255);

    set_plot_style();

    const int n_plots = 13;
    TH1F *Histo[n_plots];
    double RMS[n_plots];
    double RMSerror[n_plots];
    double Mean[n_plots];
    double Meanerror[n_plots];
    TCanvas * c1[n_plots];

    std::string names[n_plots]={"Total Cathode Spectrum per event",
                            "Reflector bounces Cathode PEs",
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
    std::string draws[n_plots]={"catpes.npes",
                            "refpes.npes",
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
    std::string cuts[n_plots]={"",
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
    std::string xTitle[n_plots]={"PEs",
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
    std::string yTitle[n_plots]={"Spectrum",
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
    int nbins[n_plots]={100,
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
                        900.0,
                        900.0,
                        0.0,
                        950.0,
                        0.0,
                        1050.0,
                        1050.0,
                        0.0};
    double highbin[n_plots]={
                        100.0,
                        10.0,
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


    for (int p=0;p<n_plots;p++){
        c1[p]=new TCanvas(Form("%s_c%02d",names[p].c_str(),p),Form("canvas_%s_%02d",names[p].c_str(),p),1500,1500);
        c1[p]->cd();
        c1[p]->SetLogy();
        Histo[p]=new TH1F(Form("Histo[%d]",p),Form("%s; %s; %s",names[p].c_str(),xTitle[p].c_str(),yTitle[p].c_str()),nbins[p],lowbin[p],highbin[p]);
        //Histo[p]=new TH1F();
        //Histo[p]->SetName(Form("Histo[%d]",p));
        Tmol->Draw(Form("%s>>Histo[%d]",draws[p].c_str(),p),Form("%s",cuts[p].c_str()));

        //Histo[p]->SetTitle(Form("%s",names[p].c_str()));
        //Histo[p]->SetXTitle(Form("PEs/radius hit position; Spectrum"));
        //Histo[p]->SetYTitle(Form("Spectrum"));

        Histo[p]->SetStats(1111);
        RMS[p] = 1.0*Histo[p]->GetRMS();
        RMSerror[p] = 1.0*Histo[p]->GetRMSError();
        Mean[p] = 1.0*Histo[p]->GetMean();
        Meanerror[p] = 1.0*Histo[p]->GetMeanError();

        //Plot_Name,x_axis_units,x_number,y_number,y_uncertainty
        // user_reflectivity = 0.9, double user_cerenkov = 1.0, double user_scintillation = 1.0, double user_z_pos = -11.0
        if (p==0){ //then a primary electron hit the quartz and we want to see the spectrum
            //file_out_rms<<names[p]<<fileP<<" - RMS,"<<RMS[p]<<","<<RMSerror[p]<<std::endl;
            //std::cout<<names[p]<<fileP<<" - Mean,"<<Mean[p]<<","<<Meanerror[p]<<std::endl;
            //file_out_mean<<names[p]<<fileP<<" - Mean,"<<Mean[p]<<","<<Meanerror[p]<<std::endl;
            //file_out_res<<names[p]<<fileP<<" - Resolution = RMS/Mean,"<<(RMS[p]/Mean[p])<<","<<(RMS[p]/Mean[p])*sqrt((RMSerror[p]*RMSerror[p])+(Meanerror[p]*Meanerror[p]))<<std::endl;


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
            file_out_rms<<names[p]<< ", " << fileP<<", RMS,"<<RMS[p]<<","<<RMSerror[p]<<std::endl;
            std::cout<<names[p]<< ", " << fileP<<", Mean,"<<Mean[p]<<","<<Meanerror[p]<<std::endl;
            file_out_mean<<names[p]<< ", " << fileP<<", Mean,"<<Mean[p]<<","<<Meanerror[p]<<std::endl;
            file_out_res<<names[p]<< ", " <<fileP<<", Resolution = RMS/Mean,"<<(RMS[p]/Mean[p])<<","<<(RMS[p]/Mean[p])*sqrt((RMSerror[p]*RMSerror[p])+(Meanerror[p]*Meanerror[p]))<<std::endl;

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
        if (p ==1 ) {
            file_out_ref_rms<<names[p]<< ", " << fileP<<", Reflector RMS,"<<RMS[p]<<","<<RMSerror[p]<<std::endl;
            std::cout<<names[p]<< ", " << fileP<<", Reflector Mean,"<<Mean[p]<<","<<Meanerror[p]<<std::endl;
            file_out_ref_mean<<names[p]<< ", " << fileP<<", Reflector Mean,"<<Mean[p]<<","<<Meanerror[p]<<std::endl;
            file_out_ref_res<<names[p]<< ", " <<fileP<<", Reflector Resolution = RMS/Mean,"<<(RMS[p]/Mean[p])<<","<<(RMS[p]/Mean[p])*sqrt((RMSerror[p]*RMSerror[p])+(Meanerror[p]*Meanerror[p]))<<std::endl;
        }
        plotsFile->cd();
        c1[p]->Write();
        c1[p]->SaveAs(Form("%s_%d.png",fileP.substr(0,fileP.find(".root")).c_str(),p));
    }

    file_out_ref_rms.close();
    file_out_ref_mean.close();
    file_out_ref_res.close();
    file_out_rms.close();
    file_out_mean.close();
    file_out_res.close();

    plotsFile = plotsTree->GetCurrentFile();
    plotsTree->Write("", TObject::kOverwrite);
    plotsTree->Print();
    plotsFile->Close();
    //theApp.Run();
}

int main(int argc, char **argv)
{
    std::string fileString = "tracking.root";
    double user_angle = 0.0;
    double user_x_pos = 0.0;
    double user_reflectivity = 0.9;
    double user_cerenkov = 1.0;
    double user_scintillation = 1.0;
    double user_z_pos = -11.0;
    //std::string varString = "reflectorAngle";
    //std::string unitString = "deg";
    bool reana = true;
    int detid=50001;
    //double manipVar=11.5;
    if (argc <= 1 || argc > 9)
    {
        std::cerr << "Usage: ./pe char*:filename int:detNumber double:angle double:x_pos double:reflectivity double:cerenkov double:scintillation double:z_pos" << std::endl;
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
        user_angle = atof(argv[3]);    
    }
    if (argc >= 5)
    {
        user_x_pos = atof(argv[4]);    
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
        user_z_pos = atof(argv[8]);    
    }
    if (reana == true) {
        std::cout << "Running with file=" << fileString << ", detid=" << detid << std::endl; 
        pe(fileString, detid);
    }
    if (argc >=3){
        std::cout << "Plotting previously analyzed file=" << fileString << " + PEs_det_" << detid << ".root" << std::endl; 
        pePlots(argc, argv, fileString, detid, user_angle, user_x_pos, user_reflectivity, user_cerenkov, user_scintillation, user_z_pos);
    }
}

