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
    int Qcounted = 0;
    int Refaircounted = 0;
    int LGaircounted = 0;
    int PMTbulkcounted = 0;
    int detSourcedPEs = 0;

    //TODO reading data into envelopes downstream could be sped up
    //by storing the data by Z instead of by hit
    
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
    for (size_t j = 0; j < oldTree->GetEntries(); j++)
    {
        if (j%10000 == 0) 
        {
            std::cerr << "\r" <<  j << "/" << oldTree->GetEntries() << " - " << (j*1.0)/oldTree->GetEntries() * 100 << "%";
        }
     
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
            if (hit.det == detid) {
                // If any particle hits the quartz detector then tell the particle ID and mother ID (so we can keep track of deltas) 
                PID.push_back(hit.pid);
                MTRID.push_back(hit.mtrid);
            }
            if (hit.pid == 0 && hit.det == detid+700){
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
                    if (hit.det == detid && Qcounted==0) {     
                        Q->push_back(QTrim(hit,peTRID.size(),cathitx,cathity,cathitz)); 
                        Qcounted++;
                    }
                    // reflector air
                    if (hit.det == detid+100 && Refaircounted==0) { 
                        Refair->push_back(RefairTrim(hit,peTRID.size(),cathitx,cathity,cathitz)); 
                        Refaircounted++;
                    }
                    // reflector skin
                    if (hit.det == detid+200) { 
                        Ref->push_back(RefTrim(hit,peTRID.size(),cathitx,cathity,cathitz)); 
                    }
                    // reflector volume
                    if (hit.det == detid+300) { 
                        RefX->push_back(RefXTrim(hit,peTRID.size(),cathitx,cathity,cathitz));  
                    }
                    // light guide air
                    if (hit.det == detid+400 && LGaircounted==0) { 
                        LGair->push_back(LGairTrim(hit,peTRID.size(),cathitx,cathity,cathitz));   
                        LGaircounted++;
                    }
                    // light guide skin
                    if (hit.det == detid+500) { 
                        LG->push_back(LGTrim(hit,peTRID.size(),cathitx,cathity,cathitz));   
                    }
                    // PMT bulk
                    if (hit.det == detid+600 && PMTbulkcounted==0) {
                        PMTbulk->push_back(PMTbulkTrim(hit,peTRID.size(),cathitx,cathity,cathitz));   
                        PMTbulkcounted++;
                    }
                    // PMT cathode
                    if (hit.det == detid+700) { 
                        PMTcat->push_back(PMTcatTrim(hit,peTRID.size(),cathitx,cathity,cathitz));   
                    }
                    //break; //how is this useful?? I want to muliple count
                }
            }   
        }
        catPEs->push_back(catPEsTrim(DETID,PID,MTRID,(int)peTRID.size(),cathitx,cathity,cathitz)); 
        detSourcedPEs=(int)peTRID.size();
        elseX->push_back(elseXTrim(((int)peTRID.size()-detSourcedPEs),cathitx,cathity,cathitz));
        if (catPEs->size() > 0){
            newTree->Fill();
	    }
        Qcounted=0;
        Refaircounted=0;
        LGaircounted=0;
        PMTbulkcounted=0;
        detSourcedPEs=0;
        eTRID.clear();
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

void pePlots(int argcC, char **argvC, std::string fileP="tracking.root", int detid=50001, std::string variable="reflectorAngle", double varVal=11.5, std::string unit="deg")
{ 
    TApplication theApp("App",&argcC,argvC);

    int dotpos = fileP.rfind(".");   
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

    const int n_plots = 12;
    TH1F *Histo[n_plots];
    double RMS[n_plots];
    double RMSerror[n_plots];
    double Mean[n_plots];
    double Meanerror[n_plots];
    TCanvas * c1[n_plots];
    std::string names[n_plots]={"Total Cathode Spectrum per event",
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
                        100};
    double lowbin[n_plots]={
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
        if (p==1){ //then a primary electron hit the quartz and we want to see the spectrum
            file_out_rms<<names[p]<<" - Changed "<<variable<<" - RMS,"<<unit<<","<<varVal<<","<<RMS[p]<<","<<RMSerror[p]<<std::endl;
            file_out_mean<<names[p]<<" - Changed "<<variable<<" - Mean,"<<unit<<","<<varVal<<","<<Mean[p]<<","<<Meanerror[p]<<std::endl;
            file_out_res<<names[p]<<" - Changed "<<variable<<" - Resolution = RMS/Mean,"<<unit<<","<<varVal<<","<<(RMS[p]/Mean[p])<<","<<(RMS[p]/Mean[p])*sqrt((RMSerror[p]*RMSerror[p])+(Meanerror[p]*Meanerror[p]))<<std::endl;
        }

        c1[p]->Write();
        c1[p]->SaveAs(/*plotsFolder+*/Form("%s_%03.2f_%02d.png",variable.c_str(),varVal,p));
    }

    file_out_rms.close();
    file_out_mean.close();
    file_out_res.close();

    plotsFile = plotsTree->GetCurrentFile();
    plotsTree->Write("", TObject::kOverwrite);
    plotsTree->Print();
    plotsFile->Close();
    theApp.Run();
}

int main(int argc, char **argv)
{
    std::string fileString = "tracking.root";
    std::string varString = "reflectorAngle";
    std::string unitString = "deg";
    bool reana = true;
    int detid=50001;
    double manipVar=11.5;
    if (argc <= 1 || argc > 7)
    {
        std::cerr << "Usage: ./pe char*:filename int:detid char*:manipulateVariable float:variableValue bool:reanalyze(y or n)" << std::endl;
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
    if (argc >=4)
    {
        std::string varName(argv[3]); 
        varString = varName;
    }
    if (argc >=5)
    {
        manipVar = atof(argv[4]);
    }
    if (argc >=6)
    {
        std::string unitName(argv[5]); 
        unitString = unitName;
    }
    if (argc >=7)
    {
        if (argv[6][0]=='n'){
            reana=false;
        }
    }
    if (reana == true) {
        std::cout << "Running with file=" << fileString << ", detid=" << detid << std::endl; 
        pe(fileString, detid);
    }
    if (argc >=5){
        std::cout << "Plotting previously analyzed file=" << fileString << " + PEs_det_" << detid << ".root" << std::endl; 
        pePlots(argc, argv, fileString, detid, varString, manipVar, unitString);
    }
}

