/*
Rakitha Mon Aug 25 14:48:01 EDT 2014
Plot generated related to the radiation in the hall

Radiation plots
Vertex cuts are based on the shielding blocks

//////

Cameron Clarke Wednesday July 26 14:41:57 EDT 2017
Plots added for y vs z vertex distributions

Cameron Clarke Wednesday August 15 20:06:46 EDT 2018
- Including libremoll.so allows for remoll v2.0.0 data types
- Remodelled code to use T->Draw() for speed and portability
- Advise to use pruneTree first to reduce the size of files
- Modelled file list reading from pruneTree (Ciprian's code)

*/

#include <vector>
#include <string>
#include <sstream>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <new>
#include <cstdlib>
#include <math.h>

#include <TRandom.h>
#include <TRandom3.h>
#include <TApplication.h>
#include <TSystem.h>

#include <TH2F.h>
#include <TH2D.h>
#include <TTree.h>
#include <TF1.h>
#include <TProfile.h>
#include <Rtypes.h>
#include <TROOT.h>
#include <TFile.h>
#include <TChain.h>
#include <TString.h> 
#include <TDatime.h>
#include <TStopwatch.h>
#include <stdexcept>
#include <time.h>
#include <cstdio>
#include <map>
#include <cassert>

#include <TMath.h>
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

using namespace std;

#define __IO_MAXHIT 100000


//for generic hits
Double_t fEvRate;
Int_t fNGenDetHit;
Int_t fGenDetHit_det[__IO_MAXHIT];
Int_t fGenDetHit_trid[__IO_MAXHIT];
Int_t fGenDetHit_pid[__IO_MAXHIT];
Double_t fGenDetHit_P[__IO_MAXHIT];
Double_t fGenDetHit_X[__IO_MAXHIT];
Double_t fGenDetHit_Y[__IO_MAXHIT];
Double_t fGenDetHit_Z[__IO_MAXHIT];
Double_t fGenDetHit_VX[__IO_MAXHIT];
Double_t fGenDetHit_VY[__IO_MAXHIT];
Double_t fGenDetHit_VZ[__IO_MAXHIT];
Double_t fGenDetHit_Lx[__IO_MAXHIT];
Double_t fGenDetHit_Ly[__IO_MAXHIT];
Double_t fGenDetHit_Lz[__IO_MAXHIT];
Double_t fGenDetHit_LdTh[__IO_MAXHIT];
Double_t fGenDetHit_LdE[__IO_MAXHIT];
Double_t fGenDetHit_edep[__IO_MAXHIT];
//Double_t fGenDetHit_T[__IO_MAXHIT];

// List of sensitive detectors:
const int n_detectors= 16;
Int_t hall_det_cyl = 99;        // cylindrical det at the radius close to hall wall 
Int_t hall_det_top = 101;       // cylindrical det at the top of the hall
Int_t hall_det_bottom = 103;    // cylindrical det at the bottom of the hall
Int_t hall = 6000;              // Physical hall detectorized
Int_t lead_target_hut = 6003;   // Target hut lead
Int_t poly_target_hut = 6004;   // Target hut poly
Int_t lead_collar = 6007;       // Lead collar detectorized
Int_t poly_collar = 6008;       // Lead collar poly shielding detectorized
Int_t US_shield_block_1 = 6010; // Concrete shielding block 1, upstream of collimator 1
Int_t US_shield_block_2 = 6011; // Concrete shielding block 2, downstream of collimator 1
Int_t US_poly_shield = 6012;    // Poly shield around collimator 1
Int_t DS_shield_block_3 = 6020; // Concrete shielding block 3, upstream of collimator 3
Int_t DS_poly_shield = 6021;    // Poly shield upstream of collimator 3
//Int_t hybrid_hut = 6024;        // Hybrid toroid's phased out shielding hut
//Int_t hybrid_poly_hut = 6025;   // Hybrid toroid's phased out poly hut
Int_t hybrid_lead_roof = 6027;  // Hybrid toroid's lead roof
Int_t lead_shldtube = 6028;     // Lead shielding tube in front of collimator 4
Int_t AlCan = 6030;             // Aluminum can around hybrid magnet

const int n_shlds = 13;

Int_t SensVolume_v[n_detectors] = {99,101,103,6000,6003,6004,6007,6008,6010,6011,6012,6020,6021,6027,6028,6030};//Detector numbers (wall, ceiling, floor, hall, lead target hut, poly target hut, lead collar, poly collar, block 1, block 2, blocks 1 and 2 poly shield, block 3, block 3's poly shield, hybrid concrete hut, hybrid poly hut, hybrid lead roof)//look at everything going out to the hall

const int n_energy_ranges = 3;
const int n_particles = 3;

const int n_regions = 10; // originally 6, used for mapping localized radiation to the whole hall
const int n_sinks = 10;   // used for mapping radiation into any one spot
const int n_sources = 10; // used for mapping total radiation versus which sensitive detectors it originiated from, not including radiation into those regions themselves
////Flux and power parameters Full range //indices [region][detector][energy range][pid] = [8][16][3][3] - 3 energy ranges for e,gamma, n for three hall detectors. 
Double_t flux_local[n_regions][2][n_particles][n_energy_ranges]={{{{0}}}}; // The last index is for the shieldings: target, shielding blocks 1 to 4, and other vertices
Double_t power_local[n_regions][2][n_particles][n_energy_ranges]={{{{0}}}};
// NEW
Double_t shld_flux_local[n_shlds][n_particles][n_energy_ranges]={{{0}}};  // The last index is for mapping where the radiation lands specifically, xyz coordinates are the origin vertices.
Double_t shld_power_local[n_shlds][n_particles][n_energy_ranges]={{{0}}};

std::map<int,int> detectormap;
std::map<int,int> pidmap;
std::map<int,double> pidmass;

// FIXME get the hit_radius cuts right based on new beam pipes etc.
Double_t hit_radius_min[2] = {0.46038,0.46038}; //m inner radius of the beam pipe 45.72 cm and outer radius of the beam pipe 46.038 cm and radius of the detector plane is 1.9 m
Double_t hit_radius;
Double_t kineE;

//boolean switches

Bool_t earlybreak=kFALSE;              //exit early from the main tree entries loop

Bool_t kSaveRootFile=kTRUE;           //save histograms and canvases into a rootfile
Bool_t kShowGraphic=kTRUE;            //Show canvases and they will be saved the rootfile. Set this to false to show no graphics but save them to rootfile

//Boolean parameter to disable/enable saving histograms as png
Bool_t kVertices=kTRUE;// Governs the cut region plotting
//Negate below 3
Bool_t kShlds=kFALSE;                  // Governs the sensitive shielding region vertex position plotting
Bool_t kShldHits=kFALSE;               // Governs the sensitive shielding region hit position plotting
Bool_t k1D=kFALSE;                    // Governs 1D plottings

Bool_t kSave1DKEHisto=(k1D && kVertices);      // option to save Kinetic energy from cut regions histograms
Bool_t kShow1DKEPlots=(k1D && kVertices);      
Bool_t kSave1DShldKEHisto=(k1D && kShlds);     // option to save Kinetic energy going into the different shielding block destination regions
Bool_t kShow1DShldKEPlots=(k1D && kShlds);     

Bool_t kSave1DVrtxHisto=(k1D && kVertices);    // option to save the z vertex positions from the cut regions histograms
Bool_t kShow1DVrtxPlots=(k1D && kVertices);    
Bool_t kSave1DShldVrtxHisto=(k1D && kShlds);   // option to save the z vertex positions going into the different shielding block destination regions
Bool_t kShow1DShldVrtxPlots=(k1D && kShlds);
Bool_t kSave1DShldHitHisto=(k1D && kShldHits); // option to save the z hit positions going into the different shielding block destination regions 
Bool_t kShow1DShldHitPlots=(k1D && kShldHits);

Bool_t kSave2DRoofHisto=kVertices;    // option to save 2D hit distribution onto the roof from cut region histograms
Bool_t kShow2DRoofPlots=kVertices;     
Bool_t kSave2DCylHisto=kVertices;     // option to save 2D hit Cylindrical detector distribution histograms
Bool_t kShow2DCylPlots=kVertices;      
Bool_t kSave2DVertexHisto=kVertices;  // option to save 2D origin vertices for the different cut regions 
Bool_t kShow2DVertexPlots=kVertices;
Bool_t kSave2DShldVertexHisto=kShlds; // option to save 2D origin vertices for the different shielding block destination regions
Bool_t kShow2DShldVertexPlots=kShlds;
Bool_t kSave2DShldHitHisto=kShldHits; // option to save 2D hit position distributions for the different shielding block destination regions
Bool_t kShow2DShldHitPlots=kShldHits;



//end of boolean switches
void set_plot_style();

TFile * rootfile;
int main(Int_t argc,Char_t* argv[]) {
  TApplication theApp("App",&argc,argv);


  ofstream list_outputs;

  //to ignore high energy moller scattering at the detector plane use,
  //hit_radius_min[0] = 1.0;
  //hit_radius_min[1] = 1.0;

  //remoll Tree
  TChain * Tmol =new TChain("T");
  //Cameron Clarke runs:
  //input info:
  const int n_mills = 1;// FIXME number of million events

  Int_t n_events = n_mills*1e6;
  Int_t beamcurrent = 85;//uA
  //TString modifier="combined_half";
  //TString added_file="/home/cameronc/gitdir/remoll/output/"+modifier+".root";
  
  //TString added_file_array[n_mills] = new TString();
  //ostringstream added_file_array[n_mills]; // The last index is for the shieldings: target, shielding blocks 1 to 4, and other vertices
  TString added_file_array[n_mills]={""};//n_mills]={""}; // The last index is for the shieldings: target, shielding blocks 1 to 4, and other vertices
  for (int v=1 ; v <= n_mills ; v++){ 
    ostringstream temp_str_stream2;
    ostringstream temp_str_stream3;
    temp_str_stream2<<v;
    TString vS;
    vS=temp_str_stream2.str();
    temp_str_stream3<<"/home/cameronc/gitdir/remoll/output/"<<argv[1]<<"_"<<n_mills<<"M/out_"<<argv[1]<<vS<<"/remoll_"<<argv[1]<<"_1M.root";
    added_file_array[v]=temp_str_stream3.str();
    Tmol->Add(added_file_array[v]);
  }
  
  ostringstream temp_str_stream4;
  temp_str_stream4<<"/home/cameronc/gitdir/remoll/output/Plots_"<<argv[1]<<"_"<<n_mills<<"M/";//Name of folder for saving plots
  TString plotsFolder=temp_str_stream4.str();//Name of folder for saving plots
  //TString plotsFolder="/home/cameronc/gitdir/remoll/output/Plots_"+modifier+"_"+nmills+"/";//Name of folder for saving plots

  ostringstream temp_str_stream5;
  temp_str_stream5<<plotsFolder<<argv[1]<<"_"<<n_mills<<"M_plots.root";//name of the rootfile to save generated histograms
  TString rootfilename=temp_str_stream5.str();//name of the rootfile to save generated histograms
  //TString rootfilename=plotsFolder+modifier+"_"+nmills+"_plots.root";//name of the rootfile to save generated histograms

  ostringstream temp_str_stream6;
  temp_str_stream6<<plotsFolder<<"list_outputs_"<<argv[1]<<"_"<<n_mills<<"M.txt";
  TString textfilename=temp_str_stream6.str();
  list_outputs.open(textfilename);
  //list_outputs.open(plotsFolder+"list_outputs_"+modifier+"_"+nmills+".txt");
 

// FIXME broken?

/*
  TString modifier=argv[1];// name of file to read (+ "_n_mills")
  TString n_millsS;
  
  ostringstream temp_str_stream;
  temp_str_stream<<n_mills;
  
  n_millsS=temp_str_stream.str();

  Int_t n_events = n_mills*1e6;
  Int_t beamcurrent = 85;//uA
  //TString modifier="combined_half";
  //TString added_file="/home/cameronc/gitdir/remoll/output/"+modifier+".root";
  
  //TString added_file_array[n_mills] = new TString();
  //ostringstream added_file_array[n_mills];
  TString added_file_array[n_mills]={""};
  for (int v=1 ; v <= n_mills ; v++){ 
    ostringstream temp_str_stream2;
    ostringstream temp_str_stream3;
    ostringstream temp_str_stream4;
    temp_str_stream2<<v;
    TString vS;
    vS=temp_str_stream2.str();
    //temp_str_stream3<<"/home/cameronc/gitdir/remoll/output/"<<modifier<<"_"<<n_mills<<"M/out_"<<modifier<<vS<<"/remoll_"<<modifier<<"_1M.root";
    //added_file_array[v]=temp_str_stream3.str();
    added_file_array[v]="/home/cameronc/gitdir/remoll/output/"+modifier+"_"+n_millsS+"M/out_"+modifier+vS+"/remoll_"+modifier+"_1M.root";
    Tmol->Add(added_file_array[v]);
  }
  
  ostringstream temp_str_stream4;
  //temp_str_stream4<<"/home/cameronc/gitdir/remoll/output/Plots_"<<modifier<<"_"<<n_mills<<"M/";//Name of folder for saving plots
  //TString plotsFolder=temp_str_stream4.str();//Name of folder for saving plots
  TString plotsFolder="/home/cameronc/gitdir/remoll/output/Plots_"+modifier+"_"+n_millsS+"M/";//Name of folder for saving plots

  ostringstream temp_str_stream5;
  //temp_str_stream5<<plotsFolder<<modifier<<"_"<<n_mills<<"M_plots.root";//name of the rootfile to save generated histograms
  TString rootfilename=plotsFolder+modifier+"_"+n_millsS+"M_plots.root";//name of the rootfile to save generated histograms
  //TString rootfilename=temp_str_stream5.str();//name of the rootfile to save generated histograms

  ostringstream temp_str_stream6;
  //temp_str_stream6<<plotsFolder<<"list_outputs_"<<modifier<<"_"<<n_mills<<"M.txt";
  //TString textfilename=temp_str_stream6.str();
  TString textfilename=plotsFolder+"list_outputs_"+modifier+"_"+n_millsS+"M.txt";
  list_outputs.open(textfilename);
  */
  //Tmol->Add(added_file);
  list_outputs << "Contents of textout_flux and textout_power lists of strings" << std::endl;

  // Alright: Everything necessary exists for me to plot vertex vs. z plots, but the R vs. z plots are probably enough for now to be honest (need to be set to some absolute scale). It would be nice to have a radiation at top of hall plot vs. z (color mapped maybe) distribution as well though. I also want to generate plots of the last significant scattering vertex, not just the particle creation vertices, in case these are significantly different.



  //generic hit (for sens detectors)
  Tmol->SetBranchAddress("rate",&fEvRate);
//  Tmol->SetBranchAddress("hit.n",&fNGenDetHit);
  Tmol->SetBranchAddress("hit.det",&fGenDetHit_det);
  Tmol->SetBranchAddress("hit.pid",&fGenDetHit_pid);
  Tmol->SetBranchAddress("hit.trid",&fGenDetHit_trid);
  Tmol->SetBranchAddress("hit.p",&fGenDetHit_P);
  Tmol->SetBranchAddress("hit.x",&fGenDetHit_X);
  Tmol->SetBranchAddress("hit.y",&fGenDetHit_Y);
  Tmol->SetBranchAddress("hit.z",&fGenDetHit_Z);
  Tmol->SetBranchAddress("hit.vx",&fGenDetHit_VX);
  Tmol->SetBranchAddress("hit.vy",&fGenDetHit_VY);
  Tmol->SetBranchAddress("hit.vz",&fGenDetHit_VZ);

  Int_t nentries = (Int_t)Tmol->GetEntries();

  if (kSaveRootFile){
    TString rootfilestatus="RECREATE";
    rootfile = new TFile(rootfilename, rootfilestatus);
    rootfile->cd();
  }

  set_plot_style();

  gROOT->SetStyle("Plain");
  //gStyle->SetOptStat(0); 
  gStyle->SetOptStat("eMR");
  gStyle->SetNumberContours(255);

  //indices asigned to each detector
  detectormap[99]=0;    // Cyl det
  detectormap[101]=1;   // Roof
  detectormap[103]=2;   // Floor
  detectormap[6000]=3;  // Hall                                             // Region 1
  detectormap[6003]=4;  // Target hut
  detectormap[6004]=5;  // Target hut poly layer                            // Region 2
  detectormap[6007]=6;  // Lead collar
  detectormap[6008]=7;  // Lead collar poly layer                           // Region 3
  detectormap[6010]=8;  // Shielding block 1 (upstream of collimator 1)
  detectormap[6011]=9;  // Shielding block 2 (downstream of collimator 1)
  detectormap[6012]=10; // Shielding blocks 1 and 2 poly layer              // Region 4
  detectormap[6020]=11; // Shielding block 3 (upstream of collimator 4)
  detectormap[6021]=12; // Shielding block 3 poly layer                     // Region 5
  detectormap[6024]=13; // Hybrid shielding block 4             // Deactivated
  detectormap[6025]=14; // Hybrid shielding block 4 poly layer  // Deactivated
  detectormap[6027]=15; // Hybrid hut lead roof                             // Region 6
  detectormap[6028]=16; // Lead shielding block in front of col4
  detectormap[6030]=17; // Aluminum can around hybrid magnet

  //indices asigned to pid numbers
  pidmap[11]=0; //electron 
  pidmap[22]=1; //photon
  pidmap[2112]=2; //neutron

  pidmass[11]=0.511;//MeV
  pidmass[22]=0.0;
  pidmass[2112]=939.565;//MeV

  //hall radiation from all source, target, collimators separated for ep,gamma, and n for three energy ranges

  // FIXME add more histograms for other kinds of plots
  TH1F *Histo_kineE[n_regions][n_particles][n_energy_ranges]; // only one that was originally n_regions+1
  TH1F *Histo_vertex[n_regions][n_particles][n_energy_ranges];
  TH1F *Histo_vertex_noWeight[n_regions][n_particles][n_energy_ranges];

  TH1F *Histo_shld_kineE[n_shlds][n_particles][n_energy_ranges]; // only one that was originally n_regions+1
  TH1F *Histo_shld_vertex[n_shlds][n_particles][n_energy_ranges];
  TH1F *Histo_shld_vertex_noWeight[n_shlds][n_particles][n_energy_ranges];
  TH1F *Histo_shld_hit[n_shlds][n_particles][n_energy_ranges];
  TH1F *Histo_shld_hit_noWeight[n_shlds][n_particles][n_energy_ranges];

  //hall radiation 2D hit distribution for the cylinder using y vs phi and using x vs. y for disks
  //When using cylindrical coordinates, the hit distr from z>0 and z<0 are projected into the y vs phi plane where phi goes from -90 to 90
  //
  TH2D *Histo_RadDet[n_regions][n_particles][5];//n_regions vertex ranges for three particles species for three detectors (0-cylinder phi-y, 1 and 2 are top and bottom disk detectors, 3,4 is cylinder x-y forward backward)

  TH2D *HistoVertex_RadDet[n_regions][n_particles][n_energy_ranges][2];//n_regions vertex ranges for three particles species for three energy ranges for y:x, and y:z NEW //I may extend to add x:z, and y:z vertex distributions
  TH2D *HistoVertex_shld_RadDet[n_shlds][n_particles][n_energy_ranges][2];//n_shlds vertex ranges for three particles species for three energy ranges for y:x, and y:z NEW //I may extend to add x:z, and y:z vertex distributions
  TH2D *HistoHit_RadDet[n_regions][n_particles][n_energy_ranges][2];//n_regions vertex ranges for three particles species for three energy ranges for y:x, and y:z NEW //I may extend to add x:z, and y:z vertex distributions
  TH2D *HistoHit_shld_RadDet[n_shlds][n_particles][n_energy_ranges][2];//n_shlds vertex ranges for three particles species for three energy ranges for y:x, and y:z NEW //I may extend to add x:z, and y:z vertex distributions

  // FIXME consider reading these xyz cuts from an external file for ease of shielding and collimator modifications (extrude beamlines too?).
  // FIXME Add more cuts for different kinds of ranges
  //                                      { change the binning to reflect the opposite nature, good, all binned in one spot, decent-needs better boundaries->775?, 775?, decent, decent, seems to miss a whole lot }
  //                                      { hall**            ,  target       ,  collar        ,  coll1shld , +magnet    ,  coll4shld,  hybshld         ,  dump ,  other,  all  }; -> Hall is an inverted volume in x and z, not y.
  //                                was   {-315 tp 1781.837   , -315 to 315   ,  275.1 to 315.1,  397.45-775,  775.55-812,  812-992  ,  992-1821.837    ,  dump ,  other,  all  }; -> Hall is an inverted volume in x and z, not y.
  Double_t z_vertex_cuts_low[n_regions] = {-235.-80           , -235.-80.     ,  285.1+10.-20. ,  315.1     ,  775.05+0.5,  812.     ,  992.            ,  2800., -500. , -3000.}; //last index store vertices outside of other ranges 
  Double_t z_vertex_cuts_up[n_regions]  = { 1821.837          ,  235.+80.     ,  285.1+10.+20. ,  812.      ,  812.      ,  992.     ,  1821.837        ,  6000.,  2000.,  6000.};
  Double_t x_vertex_cuts_low[n_regions] = {-296.5             ,  91.5-296.-80., -18.5-20.      , -213.-1.   , -386.84-1. , -386.84-1.,  91.5-285.75-40. , -500. , -500. , -3000.};
  Double_t x_vertex_cuts_up[n_regions]  = { 296.5             ,  91.5+296.+80.,  18.5+20.      ,  213.+1.   ,  386.84+1. ,  386.84+1.,  91.5+285.75+40. ,  500. ,  500. ,  3000.};
  Double_t y_vertex_cuts_low[n_regions] = {-330.              , -40.-250.-40. , -18.5-20.      , -213.-1.   , -290.-1.   , -290.-1.  , -40.-250.-40.    , -500. , -500. , -1000.};
  Double_t y_vertex_cuts_up[n_regions]  = { 330.              , -40.+250.+40. ,  18.5+20.      ,  213.+1.   ,  290.+1.   ,  290.+1.  , -40.+250.+40.    ,  500. ,  500. ,  2500.};
  Double_t R_vertex_cuts_up[n_regions]  = { 5000.             ,  500.         ,  75.           ,  350.      ,  500.      ,  500.     ,  450.            ,  500. ,  500. ,  3500.};
  Int_t    x_vertex_bin_counts[n_regions]={ 1000, 300, 50, 300, 300, 300, 300, 1000, 1000, 1000}; // default, overridden below
  Int_t    y_vertex_bin_counts[n_regions]={ 1000, 300, 50, 300, 300, 300, 300, 1000, 1000, 1000};
  Int_t    z_vertex_bin_counts[n_regions]={ 1000, 300, 50, 300, 300, 300, 300, 1200, 1000, 1000};
  Int_t    R_vertex_bin_counts[n_regions]={ 1000, 300, 50, 300, 300, 300, 300, 500 , 1000, 1000};
  // FIXME Use a constant bin/area metric.
  Int_t    x_area_per_bin = 1; // 1 cm per bin
  Int_t    y_area_per_bin = 1; // 1 cm per bin
  Int_t    z_area_per_bin = 1; // 1 cm per bin
  Int_t    R_area_per_bin = 1; // 1 cm per bin
  for (int q=0;q<n_regions;q++){  
    z_vertex_bin_counts[q] = (z_vertex_cuts_up[q]-z_vertex_cuts_low[q])/z_area_per_bin;
    x_vertex_bin_counts[q] = (x_vertex_cuts_up[q]-x_vertex_cuts_low[q])/x_area_per_bin;
    y_vertex_bin_counts[q] = (y_vertex_cuts_up[q]-y_vertex_cuts_low[q])/y_area_per_bin;
    R_vertex_bin_counts[q] = (R_vertex_cuts_up[q]-0)/R_area_per_bin;
  }

  // If in classic mode these values will be used (smaller bin ranges):
  Double_t Hall_z_vertices_low = -350.;
  Double_t Hall_z_vertices_up  =  1800.;
  Double_t Hall_x_vertices_low = -500.;
  Double_t Hall_x_vertices_up  =  500.;
  Double_t Hall_y_vertices_low = -500.;
  Double_t Hall_y_vertices_up  =  500.;
  Double_t Hall_R_vertices_up  =  3500.;
  if(kVertices==kTRUE && kShlds==kFALSE){
    z_vertex_bin_counts[0] = 650;//0;
    x_vertex_bin_counts[0] = 600;//0;
    y_vertex_bin_counts[0] = 350;//0;
    R_vertex_bin_counts[0] = 300;//0;
    Double_t Hall_z_vertices_low = -300;//0.;
    Double_t Hall_z_vertices_up  =  350;//0.;
    Double_t Hall_x_vertices_low = -300;//0.;
    Double_t Hall_x_vertices_up  =  300;//0.;
    Double_t Hall_y_vertices_low = -100;//0.;
    Double_t Hall_y_vertices_up  =  250;//0.;
    Double_t Hall_R_vertices_up  =  350;//0.;
  }

  // OLD vertices
  /*
  Double_t z_vertex_cuts_low[n_regions] = {hall,-236.5,collar,275.1,483.,590.,852.,-600.};//last index store vertices outside of other ranges 
  Double_t z_vertex_cuts_up[n_regions] = {hall,236,collar,295.1,589.99,690.,952.,1700.};//the gap between shld-2 and shld-3 are closed now so changing 483 to 583 to 483 to 589.99
  Double_t x_vertex_cuts_low[n_regions] = {hall,-296.,collar,-35.,-113.5,-133.,-224.6,-500.};
  Double_t x_vertex_cuts_up[n_regions] = {hall,296.,collar,35.,113.5,133.,346.8,500.};
  Double_t y_vertex_cuts_low[n_regions] = {hall,-250.,collar,-35.,-113.5,-133.,-250.,-500.};
  Double_t y_vertex_cuts_up[n_regions] = {hall,250.,collar,35.,113.5,133.,250.,500.};
  Double_t R_vertex_cuts_up[n_regions] =  {hall,300. , collar,50.0, 160.0, 180.0, 450., 300.};
  */

  Double_t energy_cut_low[n_energy_ranges]={0,10,30};
  Double_t energy_cut_up[n_energy_ranges]={10,30,12000};
  // FIXME why these energy upper limits? Ask Rakitha?
  //Int_t bin_ranges[n_regions+1][n_particles][n_energy_ranges+1]={
  Int_t bin_ranges[n_regions][n_particles][n_energy_ranges+1]={
           {{0,10,30,12000},{0,10,30,4000},{0,10,30,4000}},    // Hall               (elec,gamma,neutron)
           {{0,10,30,12000},{0,10,30,4000},{0,10,30,4000}},    // target 
           {{0,10,30,12000},{0,10,30,4000},{0,10,30,4000}},    // lead collar 
			   // >>
           {{0,10,30,12000},{0,10,30,4000},{0,10,30,4000}},     // Shielding Block 1 and 2
			     {{0,10,30,12000},{0,10,30,4000},{0,10,30,4000}},     // Shielding Block 3
			     {{0,10,30,12000},{0,10,30,4000},{0,10,30,4000}},     // +Magnet region
           {{0,10,30,12000},{0,10,30,4000},{0,10,30,4000}},     // Shielding Block 4
			   // << these were only up to 1000 energy range, changed to match
			     {{0,10,30,12000},{0,10,30,4000},{0,10,30,4000}},    // dump
			     {{0,10,30,12000},{0,10,30,4000},{0,10,30,4000}},    // other
			     {{0,10,30,12000},{0,10,30,4000},{0,10,30,4000}}     // all
  };
  // FIXME add more, make more clear
  //                                       { hall**,  target , collar, coll1shld,+magnet ,coll4shld,dump,hybshld,other} -> Hall is an inverted volume in x and z, not y.
  Int_t vertex_bin_ranges_low[n_regions] = {-3000. , -235.-80., 295.1-10.-10., 403., 770., 812., 992.    ,  3200., -500. , -3000.};//last index store vertices outside of other ranges 
  Int_t vertex_bin_ranges_up[n_regions]  = { 3000. ,  235.+80., 295.1+10.+10., 770., 812., 992., 1821.837,  7000.,  2000.,  3000.};
  Int_t vertex_bin_counts[n_regions]     = { 2000  ,  500     , 50           , 500 , 500 , 500 , 500     ,  1900 ,  2500 ,  6000 };
  //Int_t vertex_bin_ranges_low[n_regions] = {-235.-80 , -235.-80., 295.1-10.-10., 403., 770., 812., 992.    , -600. };//last index store vertices outside of other ranges 
  //Int_t vertex_bin_ranges_up[n_regions]  = { 1781.837,  235.+80., 295.1+10.+10., 770., 812., 992., 1821.837,  1822.};
  TString ke_range[n_energy_ranges] = {"KE<10","10<KE<30","30<KE"};
  TString spid[n_particles]={"e-","#gamma","n0"};
  // The Hall, the target hut, the lead collar, the first and second shielding blocks around the 1st collimator, the shielding block in front of the hybrid and collimator 4 (and 3), the hybrid shielding hut or roof, everything else, everything.
  TString svertex[n_regions]={"Hall","ShTarget","LeadCollar","Coll1Shld","+Magnet","Coll4Shld","HybridShld","Dump","Other","All"};     
  // last index stores hits not within tgt or collimators
  //OLD: TString svertex_2D[n_regions]={"Hall","ShTarget","LeadCollar","Coll1Shld","+Magnet","Coll4Shld","HybridShld","Other","All"};            

  TString svertex_shld[n_shlds]={"Hall","TargetHut","TargetHutPoly","LeadCollar","LeadCollarPoly","Coll1ShldUS","Coll1ShldDS","Coll1ShldPoly","Coll4Shld","Coll4ShldPoly","HybridShld","LeadTube","AlCan"};

  // FIXME Define more/less histograms
  for(Int_t i=0;i<n_regions;i++){//vertices
    for(Int_t j=0;j<n_particles;j++){//pid
      for(Int_t k=0;k<n_energy_ranges;k++){//KE
	      //1D radiation histograms
        Histo_kineE[i][j][k]=new TH1F(Form("Histo_kineE_%d_%d_%d",i+1,j+1,k+1),Form("%s from %s Area in %s MeV Range; KineE (MeV)",spid[j].Data(),svertex[i].Data(),ke_range[k].Data()),100,bin_ranges[i][j][k],bin_ranges[i][j][k+1]);
	      Histo_vertex[i][j][k]=new TH1F(Form("Histo_vertex_%d_%d_%d",i+1,j+1,k+1),Form("%s Vertices from %s Area in %s MeV Range (KE weighted);Z vertex (cm);MeV",spid[j].Data(),svertex[i].Data(),ke_range[k].Data()),vertex_bin_counts[i],vertex_bin_ranges_low[i],vertex_bin_ranges_up[i]);
	      Histo_vertex_noWeight[i][j][k]=new TH1F(Form("Histo_vertex_noWeight_%d_%d_%d",i+1,j+1,k+1),Form("%s Vertices from %s Area in %s MeV Range ;Z vertex (cm);Counts, for %d events",spid[j].Data(),svertex[i].Data(),ke_range[k].Data(),n_events),vertex_bin_counts[i],vertex_bin_ranges_low[i],vertex_bin_ranges_up[i]);
	      //2D vertex distribution histograms
	      if (i>0){
          HistoVertex_RadDet[i][j][k][0]=new TH2D(Form("HistoVertex_RadDet_v%d_p%d_k%d_0",i+1,j+1,k+1),Form(" %s from %s Area in %s MeV Range; x (cm); y (cm); (MeV)",spid[j].Data(),svertex[i].Data(),ke_range[k].Data()),x_vertex_bin_counts[i],x_vertex_cuts_low[i]-1,x_vertex_cuts_up[i]+1,y_vertex_bin_counts[i],y_vertex_cuts_low[i]-1,y_vertex_cuts_up[i]+1);
	        //HistoVertex_RadDet[i][j][k][1]=new TH2D(Form("HistoVertex_RadDet_v%d_p%d_k%d_1",i+1,j+1,k+1),Form(" %s from %s Area in %s MeV Range; z (cm); R (cm); (MeV)",spid[j].Data(),svertex[i].Data(),ke_range[k].Data()),z_vertex_bin_counts[i],z_vertex_cuts_low[i]-1,z_vertex_cuts_up[i]+1,R_vertex_bin_counts[i],0,R_vertex_cuts_up[i]+1);
	        HistoVertex_RadDet[i][j][k][1]=new TH2D(Form("HistoVertex_RadDet_v%d_p%d_k%d_2",i+1,j+1,k+1),Form(" %s from %s Area in %s MeV Range; z (cm); y (cm); (MeV)",spid[j].Data(),svertex[i].Data(),ke_range[k].Data()),z_vertex_bin_counts[i],z_vertex_cuts_low[i]-1,z_vertex_cuts_up[i]+1,y_vertex_bin_counts[i],y_vertex_cuts_low[i]-1,y_vertex_cuts_up[i]+1);
        }
        else if (i==0){
          HistoVertex_RadDet[i][j][k][0]=new TH2D(Form("HistoVertex_RadDet_v%d_p%d_k%d_0",i+1,j+1,k+1),Form(" %s from %s Area in %s MeV Range; x (cm); y (cm); (MeV)",spid[j].Data(),svertex[i].Data(),ke_range[k].Data()),x_vertex_bin_counts[i],Hall_x_vertices_low-1,Hall_x_vertices_up+1,y_vertex_bin_counts[i],Hall_y_vertices_low-1,Hall_y_vertices_up+1);
	        //HistoVertex_RadDet[i][j][k][1]=new TH2D(Form("HistoVertex_RadDet_v%d_p%d_k%d_1",i+1,j+1,k+1),Form(" %s from %s Area in %s MeV Range; z (cm); R (cm); (MeV)",spid[j].Data(),svertex[i].Data(),ke_range[k].Data()),z_vertex_bin_counts[i],Hall_z_vertices_low-1,Hall_z_vertices_up+1,R_vertex_bin_counts[i],0,Hall_R_vertices_up+1);
	        HistoVertex_RadDet[i][j][k][1]=new TH2D(Form("HistoVertex_RadDet_v%d_p%d_k%d_2",i+1,j+1,k+1),Form(" %s from %s Area in %s MeV Range; z (cm); y (cm); (MeV)",spid[j].Data(),svertex[i].Data(),ke_range[k].Data()),z_vertex_bin_counts[i],Hall_z_vertices_low-1,Hall_z_vertices_up+1,y_vertex_bin_counts[i],Hall_y_vertices_low-1,Hall_y_vertices_up+1);
        }
      }
	    Histo_RadDet[i][j][0]=new TH2D(Form("Histo_RadDet_%d_%d_0",i+1,j+1),Form("Cyl. Det: %s from %s Area; #phi (Deg.); y (cm); (MeV)",spid[j].Data(),svertex[i].Data()),90,-90,90,100,-800,800);//default bin sizes 360 and 400
	    Histo_RadDet[i][j][1]=new TH2D(Form("Histo_RadDet_%d_%d_1",i+1,j+1),Form("Top Disk. Det: %s from %s Area; z (cm); x (cm); (MeV)",spid[j].Data(),svertex[i].Data()),100,-3000,3000,100,-3000,3000);//default bin sizes 300, 300
	    Histo_RadDet[i][j][2]=new TH2D(Form("Histo_RadDet_%d_%d_2",i+1,j+1),Form("Bottom Disk. Det: %s from %s Area; z (cm); x (cm); (MeV)",spid[j].Data(),svertex[i].Data()),100,-3000,3000,100,-3000,3000);//default bin sizes 300, 300
	    Histo_RadDet[i][j][3]=new TH2D(Form("Histo_RadDet_%d_%d_3f",i+1,j+1),Form("Cyl. Det: %s from %s Area : Forward; x (cm); y (cm); (MeV)",spid[j].Data(),svertex[i].Data()),260,-2600,2600,100,-800,800);//default bin sizes 520, 400
	    Histo_RadDet[i][j][4]=new TH2D(Form("Histo_RadDet_%d_%d_4b",i+1,j+1),Form("Cyl. Det: %s from %s Area : Backward; x (cm); y (cm); (MeV)",spid[j].Data(),svertex[i].Data()),260,-2600,2600,100,-800,800);//default bin sizes 520, 400
    }
  } 
  // FIXME New histograms and stuff
  for(Int_t i=0;i<n_shlds;i++){
    for(Int_t j=0;j<n_particles;j++){
      for(Int_t k=0;k<n_energy_ranges;k++){
        Histo_shld_kineE[i][j][k]=new TH1F(Form("Histo_shld_kineE_%d_%d_%d",i+1,j+1,k+1),Form("%s into %s Area in %s MeV Range; KineE (MeV)",spid[j].Data(),svertex_shld[i].Data(),ke_range[k].Data()),z_vertex_bin_counts[0],Hall_z_vertices_low-1,Hall_z_vertices_up+1);
	      Histo_shld_vertex[i][j][k]=new TH1F(Form("Histo_shld_vertex_%d_%d_%d",i+1,j+1,k+1),Form("%s Vertices into %s Area in %s MeV Range (KE weighted);Z vertex (cm);MeV",spid[j].Data(),svertex_shld[i].Data(),ke_range[k].Data()),z_vertex_bin_counts[0],Hall_z_vertices_low-1,Hall_z_vertices_up+1);
	      Histo_shld_vertex_noWeight[i][j][k]=new TH1F(Form("Histo_shld_vertex_noWeight_%d_%d_%d",i+1,j+1,k+1),Form("%s Vertices into %s Area in %s MeV Range ;Z vertex (cm);Counts, for %d events",spid[j].Data(),svertex_shld[i].Data(),ke_range[k].Data(),n_events),z_vertex_bin_counts[0],Hall_z_vertices_low-1,Hall_z_vertices_up+1);
        HistoVertex_shld_RadDet[i][j][k][0]=new TH2D(Form("HistoVertex_shld_RadDet_v%d_p%d_k%d_0",i+1,j+1,k+1),Form(" %s into %s Area in %s MeV Range; x (cm); y (cm); (MeV)",spid[j].Data(),svertex_shld[i].Data(),ke_range[k].Data()),x_vertex_bin_counts[0],Hall_x_vertices_low-1,Hall_x_vertices_up+1,y_vertex_bin_counts[0],Hall_y_vertices_low-1,Hall_y_vertices_up+1);
	      HistoVertex_shld_RadDet[i][j][k][1]=new TH2D(Form("HistoVertex_shld_RadDet_v%d_p%d_k%d_2",i+1,j+1,k+1),Form(" %s into %s Area in %s MeV Range; z (cm); y (cm); (MeV)",spid[j].Data(),svertex_shld[i].Data(),ke_range[k].Data()),z_vertex_bin_counts[0],Hall_z_vertices_low-1,Hall_z_vertices_up+1,y_vertex_bin_counts[0],Hall_y_vertices_low-1,Hall_y_vertices_up+1);
	      
        
        Histo_shld_hit[i][j][k]=new TH1F(Form("Histo_shld_hit_%d_%d_%d",i+1,j+1,k+1),Form("%s Hits into %s Area in %s MeV Range (KE weighted);Z vertex (cm);MeV",spid[j].Data(),svertex_shld[i].Data(),ke_range[k].Data()),z_vertex_bin_counts[0],Hall_z_vertices_low-1,Hall_z_vertices_up+1);
	      Histo_shld_hit_noWeight[i][j][k]=new TH1F(Form("Histo_shld_hit_noWeight_%d_%d_%d",i+1,j+1,k+1),Form("%s Hits into %s Area in %s MeV Range ;Z vertex (cm);Counts, for %d events",spid[j].Data(),svertex_shld[i].Data(),ke_range[k].Data(),n_events),z_vertex_bin_counts[0],Hall_z_vertices_low-1,Hall_z_vertices_up+1);
        HistoHit_shld_RadDet[i][j][k][0]=new TH2D(Form("HistoHit_shld_RadDet_v%d_p%d_k%d_0",i+1,j+1,k+1),Form(" %s Hits into %s Area in %s MeV Range; x (cm); y (cm); (MeV)",spid[j].Data(),svertex_shld[i].Data(),ke_range[k].Data()),x_vertex_bin_counts[0],Hall_x_vertices_low-1,Hall_x_vertices_up+1,y_vertex_bin_counts[0],Hall_y_vertices_low-1,Hall_y_vertices_up+1);
	      HistoHit_shld_RadDet[i][j][k][1]=new TH2D(Form("HistoHitx_shld_RadDet_v%d_p%d_k%d_2",i+1,j+1,k+1),Form(" %s Hits into %s Area in %s MeV Range; z (cm); y (cm); (MeV)",spid[j].Data(),svertex_shld[i].Data(),ke_range[k].Data()),z_vertex_bin_counts[0],Hall_z_vertices_low-1,Hall_z_vertices_up+1,y_vertex_bin_counts[0],Hall_y_vertices_low-1,Hall_y_vertices_up+1);
      }
    }
  }

  int detid    = -1;
  int pid      = -1;
  Int_t vrtx   = -1; //index for vertex range
  Int_t keid   = -1;
  Int_t vrtx_z = -1;

  Double_t rho;
  Double_t phi;
  printf("Normalized to %d events \n",n_events);
  for (int i=0; i<nentries ; i++) {
    for (int unused_iterator = 0; unused_iterator < __IO_MAXHIT; unused_iterator++)
      fGenDetHit_pid[unused_iterator] = 0;    
    Tmol->GetEntry(i);
    for (int u = 0; u < __IO_MAXHIT; u++)
    {  if(fGenDetHit_P[u] == 0)
      {  
        fNGenDetHit = u;
        break;
      }
    }
    for (int j = 0; j<fNGenDetHit; j++){

      //for rate weighted simulations
      //if(fEvRate<0)
      //break;
      
      //for e-beam on target
      //record power and flux by the hall detectors only for pid==11,22,2112 particles
      // FIXME edit the detector definitions
      if(kVertices && (fGenDetHit_det[j]==SensVolume_v[0] || fGenDetHit_det[j]==SensVolume_v[1] || fGenDetHit_det[j]==SensVolume_v[2]) && (TMath::Abs(fGenDetHit_pid[j])==11 || fGenDetHit_pid[j]==22 || fGenDetHit_pid[j]==2112) ){//total into the hall
	      //big set of for loops!!
	      detid=detectormap[fGenDetHit_det[j]]; 
        if (fGenDetHit_VZ[j]<0)//for backside
          vrtx_z=0;
        else
          vrtx_z=1;//for forward 

        if (detid==0)//for cyc. detector (det=99)
          hit_radius = TMath::Sqrt(TMath::Power(fGenDetHit_X[j],2)+TMath::Power(fGenDetHit_Y[j],2));//for the cyclindrival detector to ignore the flux going into the beam dump
        else
          hit_radius = 999;
        pid=pidmap[(Int_t)TMath::Abs(fGenDetHit_pid[j])];
        //use a nested if to get the vertex range using fGenDetHit_VZ[j]
        //for (Int_t vrt_i=0;vrt_i<n_regions+1;vrt_i++){
        for (Int_t vrt_i=0;vrt_i<n_regions;vrt_i++){
          //use only z vertex cut to select regions with x,y limiting vertices only coming from MOLLER apparatus otherwise there will be over counting due to hall enclosure 
          if (vrt_i != 0 && fGenDetHit_VZ[j]*100>=z_vertex_cuts_low[vrt_i] && fGenDetHit_VZ[j]*100<=z_vertex_cuts_up[vrt_i] && fGenDetHit_VX[j]*100>=x_vertex_cuts_low[vrt_i] && fGenDetHit_VX[j]*100<=x_vertex_cuts_up[vrt_i] && fGenDetHit_VY[j]*100>=y_vertex_cuts_low[vrt_i] && fGenDetHit_VY[j]*100<=y_vertex_cuts_up[vrt_i]){
            //if (fGenDetHit_VZ[j]*100>=z_vertex_cuts_low[vrt_i] && fGenDetHit_VZ[j]*100<=z_vertex_cuts_up[vrt_i]){
            vrtx=vrt_i;
//            break;
          }
          // FIXME // Include a outside x,y,z regions option to get a negative of the beamline area (hall region by negation)  
          else if (vrt_i == 0 && (fGenDetHit_VZ[j]*100<=z_vertex_cuts_low[vrt_i] || fGenDetHit_VZ[j]*100>=z_vertex_cuts_up[vrt_i] || fGenDetHit_VX[j]*100<=x_vertex_cuts_low[vrt_i] || fGenDetHit_VX[j]*100>=x_vertex_cuts_up[vrt_i] || fGenDetHit_VY[j]*100<=y_vertex_cuts_low[vrt_i] || fGenDetHit_VY[j]*100>=y_vertex_cuts_up[vrt_i])){
            vrtx=vrt_i;
//            break;
          }
          else{
            vrtx = -1;//vertex outside of the simulation region (non-physical)
          }
//        } // Old end of for-loop, not it will add the event to any region's histgrams that may or may not overlap (before it was exclusive to just the first region that got it -> removed the 'breaks'). CSC 7/26/2017 EDIT

          if(vrtx>=0){
            kineE = TMath::Sqrt(TMath::Power(fGenDetHit_P[j]*1000,2) + TMath::Power(pidmass[(Int_t)TMath::Abs(fGenDetHit_pid[j])],2) ) - pidmass[(Int_t)TMath::Abs(fGenDetHit_pid[j])];
            for (Int_t ke_i=0;ke_i<n_energy_ranges;ke_i++){
              if (kineE > energy_cut_low[ke_i] && kineE <= energy_cut_up[ke_i]){
                keid=ke_i;
                break;
              }
              else{
                keid=-1;
              }      
            }
            if (keid>=0 && hit_radius > hit_radius_min[vrtx_z] ){// FIXME Should I make all the plots have an ALL region at vrtx = n_regions (an n_regions+1 index) and make the n_regions-1 index function as a catchall? YES FIXME ->>>> && vrtx<n_regions-1){
              flux_local[vrtx][detid][pid][keid]++;
              power_local[vrtx][detid][pid][keid]+=kineE;
              rho = TMath::Sqrt(TMath::Power(fGenDetHit_Z[j],2)+TMath::Power(fGenDetHit_X[j],2)); //z is in direction of beam, x is transverse, y is vertically upwards.
              phi = TMath::ASin(fGenDetHit_X[j]/rho)*180/TMath::Pi();
              // FIXME edit the histograms to be filled here.
              //following if is a redundant check I already checked  vrtx for negative values up
              Histo_kineE[vrtx][pid][keid]->Fill(kineE);
              Histo_vertex[vrtx][pid][keid]->Fill(fGenDetHit_VZ[j]*100,kineE/n_events);
              Histo_vertex_noWeight[vrtx][pid][keid]->Fill(fGenDetHit_VZ[j]*100,1);
              if (detid==0){
                Histo_RadDet[vrtx][pid][0]->Fill(phi,fGenDetHit_Y[j]*100,kineE/n_events);//fill cyl. phi detector
                if (fGenDetHit_Z[j]>=0)
                  Histo_RadDet[vrtx][pid][3]->Fill(fGenDetHit_X[j]*100,fGenDetHit_Y[j]*100,kineE/n_events);//fill cyl. detector	forward
                else
                  Histo_RadDet[vrtx][pid][4]->Fill(fGenDetHit_X[j]*100,fGenDetHit_Y[j]*100,kineE/n_events);//fill cyl. detector	backward
              }	
              else if (detid==1 || detid==2)
                Histo_RadDet[vrtx][pid][detid]->Fill(fGenDetHit_Z[j]*100,fGenDetHit_X[j]*100,kineE/n_events);//fill roof detector

              //Fill vertex 2D plots
              HistoVertex_RadDet[vrtx][pid][keid][0]->Fill(fGenDetHit_VX[j]*100,fGenDetHit_VY[j]*100,kineE/n_events);
              //HistoVertex_RadDet[vrtx][pid][keid][1]->Fill(fGenDetHit_VZ[j]*100,TMath::Sqrt(TMath::Power(fGenDetHit_VX[j]*100,2)+TMath::Power(fGenDetHit_VY[j]*100,2)),kineE/n_events);
              HistoVertex_RadDet[vrtx][pid][keid][1]->Fill(fGenDetHit_VZ[j]*100,fGenDetHit_VY[j]*100,kineE/n_events);
 
            }
            else if (hit_radius > hit_radius_min[vrtx_z])//without this condition warning will print for tracks going to the dump
              printf("warning: energy outside the ranges %4.3f \n",kineE);
          }// end for loop, run once per event per region it appears in
         //Run once per event
     /*   if (keid>=0 && hit_radius > hit_radius_min[vrtx_z]){
          //Fill vertex distribution 2D plots for all vertices using last index
          HistoVertex_RadDet[n_regions-1][pid][keid][0]->Fill(fGenDetHit_VX[j]*100,fGenDetHit_VY[j]*100,kineE/n_events);
          HistoVertex_RadDet[n_regions-1][pid][keid][1]->Fill(fGenDetHit_VZ[j]*100,TMath::Sqrt(TMath::Power(fGenDetHit_VX[j]*100,2)+TMath::Power(fGenDetHit_VY[j]*100,2)),kineE/n_events);
          HistoVertex_RadDet[n_regions-1][pid][keid][2]->Fill(fGenDetHit_VZ[j]*100,fGenDetHit_VY[j]*100,kineE/n_events);
          Histo_kineE[n_regions][pid][keid]->Fill(kineE);
            
          if (detid==0){
            Histo_RadDet[n_regions-1][pid][0]->Fill(phi,fGenDetHit_Y[j]*100,kineE/n_events);//fill cyl. detector //index 5 will fill all the vertices (vrtx from 0 to 6)
            if (fGenDetHit_Z[j]>=0)		
              Histo_RadDet[n_regions-1][pid][3]->Fill(fGenDetHit_X[j]*100,fGenDetHit_Y[j]*100,kineE/n_events);//fill cyl. detector	
            else
              Histo_RadDet[n_regions-1][pid][4]->Fill(fGenDetHit_X[j]*100,fGenDetHit_Y[j]*100,kineE/n_events);//fill cyl. detector	
          }	
          else if (detid==1 || detid==2)
            Histo_RadDet[n_regions-1][pid][detid]->Fill(fGenDetHit_Z[j]*100,fGenDetHit_X[j]*100,kineE/n_events);//fill cyl. detector //index n_regions-1 will fill all the vertices (vrtx from 0 to n_regions-1)
            //index n_regions-1 will fill all the vertices (vrtx from 0 to n_regions-1) if used here
          if (detid==0)
            Histo_RadDet[n_regions-1][pid][0]->Fill(phi,fGenDetHit_Y[j]*100,kineE/n_events);//fill cyl. detector
          else if (detid==1 || detid==2)
            Histo_RadDet[n_regions-1][pid][detid]->Fill(fGenDetHit_Z[j]*100,fGenDetHit_X[j]*100,kineE/n_events);//fill cyl. detector
        }*/
        }

        //else
        //printf("warning: vertex outside the ranges z = %4.3f cm \n",fGenDetHit_VZ[j]*100);
	      //now repeat above set for three hall detectors to get totals for each detector
      }

      // FIXME New filling - measures how much radiation, and from where, each shielding block is absorbing.
      if((kShlds || kShldHits) && (fGenDetHit_det[j]==SensVolume_v[3] || fGenDetHit_det[j]==SensVolume_v[4] || fGenDetHit_det[j]==SensVolume_v[5] || fGenDetHit_det[j]==SensVolume_v[6] || fGenDetHit_det[j]==SensVolume_v[7] || fGenDetHit_det[j]==SensVolume_v[8] || fGenDetHit_det[j]==SensVolume_v[9] || fGenDetHit_det[j]==SensVolume_v[10] || fGenDetHit_det[j]==SensVolume_v[11] || fGenDetHit_det[j]==SensVolume_v[12] || fGenDetHit_det[j]==SensVolume_v[13] || fGenDetHit_det[j]==SensVolume_v[14] || fGenDetHit_det[j]==SensVolume_v[15] || fGenDetHit_det[j]==SensVolume_v[16] || fGenDetHit_det[j]==SensVolume_v[17]) && (TMath::Abs(fGenDetHit_pid[j])==11 || fGenDetHit_pid[j]==22 || fGenDetHit_pid[j]==2112) ){//total into the hall
	      //big set of for loops!!
	      detid=detectormap[fGenDetHit_det[j]]; 
        pid=pidmap[(Int_t)TMath::Abs(fGenDetHit_pid[j])];
        for(Int_t q=3;q<n_shlds+3;q++){
          if(fGenDetHit_det[j]==SensVolume_v[q]){
            vrtx=q-3;
            break;
          }
          else vrtx=-1;
        }
        if(vrtx>=0){
          kineE = TMath::Sqrt(TMath::Power(fGenDetHit_P[j]*1000,2) + TMath::Power(pidmass[(Int_t)TMath::Abs(fGenDetHit_pid[j])],2) ) - pidmass[(Int_t)TMath::Abs(fGenDetHit_pid[j])];
          for (Int_t ke_i=0;ke_i<n_energy_ranges;ke_i++){
            if (kineE > energy_cut_low[ke_i] && kineE <= energy_cut_up[ke_i]){
              keid=ke_i;
              break;
          }
            else{
              keid=-1;
            }      
          }
          if (keid>=0){
            shld_flux_local[vrtx][pid][keid]++;
            shld_power_local[vrtx][pid][keid]+=kineE;
            rho = TMath::Sqrt(TMath::Power(fGenDetHit_Z[j],2)+TMath::Power(fGenDetHit_X[j],2)); //z is in direction of beam, x is transverse, y is vertically upwards.
            phi = TMath::ASin(fGenDetHit_X[j]/rho)*180/TMath::Pi();
            // FIXME edit the histograms to be filled here.
            //following if is a redundant check I already checked  vrtx for negative values up
            Histo_shld_kineE[vrtx][pid][keid]->Fill(kineE);
            Histo_shld_vertex[vrtx][pid][keid]->Fill(fGenDetHit_VZ[j]*100,kineE/n_events);
            Histo_shld_vertex_noWeight[vrtx][pid][keid]->Fill(fGenDetHit_VZ[j]*100,1);
            Histo_shld_hit[vrtx][pid][keid]->Fill(fGenDetHit_Z[j]*100,kineE/n_events);
            Histo_shld_hit_noWeight[vrtx][pid][keid]->Fill(fGenDetHit_Z[j]*100,1);

            //Fill vertex 2D plots
            HistoVertex_shld_RadDet[vrtx][pid][keid][0]->Fill(fGenDetHit_VX[j]*100,fGenDetHit_VY[j]*100,kineE/n_events);
            HistoVertex_shld_RadDet[vrtx][pid][keid][1]->Fill(fGenDetHit_VZ[j]*100,fGenDetHit_VY[j]*100,kineE/n_events);
            HistoHit_shld_RadDet[vrtx][pid][keid][0]->Fill(fGenDetHit_X[j]*100,fGenDetHit_Y[j]*100,kineE/n_events);
            HistoHit_shld_RadDet[vrtx][pid][keid][1]->Fill(fGenDetHit_Z[j]*100,fGenDetHit_Y[j]*100,kineE/n_events);
          }
        }
      }
// \NEW
    }
    if (i>200000 && earlybreak)
      break;
    if (i%500000==0)
      printf("Event %d of %d \n",i,nentries);
  }
  
  // FIXME Drawing and Saving Loops

  //vertex based histograms
  if (kShow1DKEPlots){
    //TCanvas * canvas_ke[n_regions+1][n_particles];
    //for(Int_t i=0;i<n_regions+1;i++){//vtx
    TCanvas * canvas_ke[n_regions][n_particles];
    for(Int_t i=0;i<n_regions;i++){//vtx
      for(Int_t j=0;j<n_particles;j++){//pid
	      canvas_ke[i][j]= new TCanvas(Form("canvas_ke_vrtx%02d_pid%d",i+1,j+1),Form("canvas_ke_vrtx%02d_pid%d",i+1,j+1),1500,500);
	      canvas_ke[i][j]->Divide(n_energy_ranges,1);
        for(Int_t k=0;k<n_energy_ranges;k++){//KE
	        canvas_ke[i][j]->cd(k+1);
	        if (k>1){
	          canvas_ke[i][j]->cd(k+1)->SetLogy();
          }
          Histo_kineE[i][j][k]->Draw();
	      }
	      if (kSave1DKEHisto){
	        canvas_ke[i][j]->SaveAs(plotsFolder+Form("canvas_ke_vrtx%02d_pid%d.png",i+1,j+1));
        }
        if (kSaveRootFile){
	        canvas_ke[i][j]->Write();
        }
      }
    }
  }

  if (kShow1DShldKEPlots){
    TCanvas * canvas_shld_ke[n_shlds][n_particles];
    for(Int_t i=0;i<n_shlds;i++){//shlds
      for(Int_t j=0;j<n_particles;j++){//pid
        canvas_shld_ke[i][j]= new TCanvas(Form("canvas_shld_ke_vrtx%02d_pid%d",i+1,j+1),Form("canvas_shld_ke_vrtx%02d_pid%d",i+1,j+1),1500,500);
	      canvas_shld_ke[i][j]->Divide(n_energy_ranges,1); 
        for(Int_t k=0;k<n_energy_ranges;k++){
          canvas_shld_ke[i][j]->cd(k+1);
          if(k>1){
            canvas_shld_ke[i][j]->cd(k+1)->SetLogy();
          }
          Histo_shld_kineE[i][j][k]->Draw();
        }
        if (kSave1DShldKEHisto){
	        canvas_shld_ke[i][j]->SaveAs(plotsFolder+Form("canvas_shld_ke_vrtx%02d_pid%d.png",i+1,j+1));
        }
        if (kSaveRootFile){
	        canvas_shld_ke[i][j]->Write();
        }
      }
    }
  }
  if (kShow1DVrtxPlots){
    TCanvas * canvas_vertx[n_regions][n_particles];
    TCanvas * canvas_vertx_noWeight[n_regions][n_particles];
    TCanvas * canvas_vertx_integral[n_regions][n_particles];
    Double_t norme;
    Double_t *integral;
    for(Int_t i=0;i<n_regions;i++){//vertex // ADD ON TO THIS FUNCTIONALITY: This creates x vs y plots of birth vertices in 6 z vertex regions
      for(Int_t j=0;j<n_particles;j++){//pid
        canvas_vertx[i][j]= new TCanvas(Form("canvas_vertx_vrtx%02d_pid%d",i+1,j+1),Form("canvas_vertx_vrtx%02d_pid%d",i+1,j+1),1500,500);	
        canvas_vertx[i][j]->Divide(n_energy_ranges,1);
        canvas_vertx_noWeight[i][j]= new TCanvas(Form("canvas_vertx_noWeight_vrtx%02d_pid%d",i+1,j+1),Form("canvas_vertx_noWeight_vrtx%02d_pid%d",i+1,j+1),1500,500);	
        canvas_vertx_noWeight[i][j]->Divide(n_energy_ranges,1);

        canvas_vertx_integral[i][j]= new TCanvas(Form("canvas_vertx_integral_vrtx%02d_pid%d",i+1,j+1),Form("canvas_vertx_integral_vrtx%02d_pid%d",i+1,j+1),1500,500);	
        canvas_vertx_integral[i][j]->Divide(n_energy_ranges,1);
        for(Int_t k=0;k<n_energy_ranges;k++){//KE
          canvas_vertx[i][j]->cd(k+1);
          canvas_vertx[i][j]->cd(k+1)->SetLogy();
          Histo_vertex[i][j][k]->DrawCopy();
          //plot integral
          norme=Histo_vertex[i][j][k]->Integral();
          integral=Histo_vertex[i][j][k]->GetIntegral();
          Histo_vertex[i][j][k]->SetContent(integral);
          Histo_vertex[i][j][k]->Scale(norme);
          canvas_vertx_integral[i][j]->cd(k+1);
          //canvas_vertx_integral[i][j]->cd(k+1)->SetLogy();
          Histo_vertex[i][j][k]->Draw();
          canvas_vertx_noWeight[i][j]->cd(k+1);
          canvas_vertx_noWeight[i][j]->cd(k+1)->SetLogy();
          Histo_vertex_noWeight[i][j][k]->Draw();
        }      
        if (kSave1DVrtxHisto){
          canvas_vertx[i][j]->SaveAs(plotsFolder+Form("canvas_vrtx_vrtx%02d_pid%d.png",i+1,j+1));
          canvas_vertx_noWeight[i][j]->SaveAs(plotsFolder+Form("canvas_vrtx_noWeight_vrtx%02d_pid%d.png",i+1,j+1));
          canvas_vertx_integral[i][j]->SaveAs(plotsFolder+Form("canvas_vrtx_integral_vrtx%02d_pid%d.png",i+1,j+1));
        }
        if (kSaveRootFile){
          canvas_vertx[i][j]->Write();
          canvas_vertx_noWeight[i][j]->Write();
          canvas_vertx_integral[i][j]->Write();
        }
      }
    }
  }

  if (kShow1DShldVrtxPlots){
    TCanvas * canvas_shld_vertx[n_shlds][n_particles];
    TCanvas * canvas_shld_vertx_noWeight[n_shlds][n_particles];
    TCanvas * canvas_shld_vertx_integral[n_shlds][n_particles];
    Double_t norme_shld;
    Double_t *integral_shld;
    for(Int_t i=0;i<n_shlds;i++){//vertex // ADD ON TO THIS FUNCTIONALITY: This creates x vs y plots of birth vertices in 6 z vertex regions
      for(Int_t j=0;j<n_particles;j++){//pid
        canvas_shld_vertx[i][j]= new TCanvas(Form("canvas_shld_vertx_vrtx%02d_pid%d",i+1,j+1),Form("canvas_shld_vertx_vrtx%02d_pid%d",i+1,j+1),1500,500);	
        canvas_shld_vertx[i][j]->Divide(n_energy_ranges,1);
        canvas_shld_vertx_noWeight[i][j]= new TCanvas(Form("canvas_shld_vertx_noWeight_vrtx%02d_pid%d",i+1,j+1),Form("canvas_shld_vertx_noWeight_vrtx%02d_pid%d",i+1,j+1),1500,500);	
        canvas_shld_vertx_noWeight[i][j]->Divide(n_energy_ranges,1);

        canvas_shld_vertx_integral[i][j]= new TCanvas(Form("canvas_shld_vertx_integral_vrtx%02d_pid%d",i+1,j+1),Form("canvas_shld_vertx_integral_vrtx%02d_pid%d",i+1,j+1),1500,500);	
        canvas_shld_vertx_integral[i][j]->Divide(n_energy_ranges,1);
        for(Int_t k=0;k<n_energy_ranges;k++){//KE
          canvas_shld_vertx[i][j]->cd(k+1);
          canvas_shld_vertx[i][j]->cd(k+1)->SetLogy();
          Histo_shld_vertex[i][j][k]->DrawCopy();
          //plot integral
          norme_shld=Histo_shld_vertex[i][j][k]->Integral();
          integral_shld=Histo_shld_vertex[i][j][k]->GetIntegral();
          Histo_shld_vertex[i][j][k]->SetContent(integral_shld);
          Histo_shld_vertex[i][j][k]->Scale(norme_shld);
          canvas_shld_vertx_integral[i][j]->cd(k+1);
          //canvas_shld_vertx_integral[i][j]->cd(k+1)->SetLogy();
          Histo_shld_vertex[i][j][k]->Draw();
          canvas_shld_vertx_noWeight[i][j]->cd(k+1);
          canvas_shld_vertx_noWeight[i][j]->cd(k+1)->SetLogy();
          Histo_shld_vertex_noWeight[i][j][k]->Draw();
        }      
        if (kSave1DShldVrtxHisto){
          canvas_shld_vertx[i][j]->SaveAs(plotsFolder+Form("canvas_shld_vrtx_vrtx%02d_pid%d.png",i+1,j+1));
          canvas_shld_vertx_noWeight[i][j]->SaveAs(plotsFolder+Form("canvas_shld_vrtx_noWeight_vrtx%02d_pid%d.png",i+1,j+1));
          canvas_shld_vertx_integral[i][j]->SaveAs(plotsFolder+Form("canvas_shld_vrtx_integral_vrtx%02d_pid%d.png",i+1,j+1));
        }
        if (kSaveRootFile){
          canvas_shld_vertx[i][j]->Write();
          canvas_shld_vertx_noWeight[i][j]->Write();
          canvas_shld_vertx_integral[i][j]->Write();
        }
      }
    }
  }

  if (kShow1DShldHitPlots){
    TCanvas * canvas_shld_hit[n_shlds][n_particles];
    TCanvas * canvas_shld_hit_noWeight[n_shlds][n_particles];
    TCanvas * canvas_shld_hit_integral[n_shlds][n_particles];
    Double_t norme_shld;
    Double_t *integral_shld;
    for(Int_t i=0;i<n_shlds;i++){//vertex // ADD ON TO THIS FUNCTIONALITY: This creates x vs y plots of birth vertices in 6 z vertex regions
      for(Int_t j=0;j<n_particles;j++){//pid
        canvas_shld_hit[i][j]= new TCanvas(Form("canvas_shld_hit_vrtx%02d_pid%d",i+1,j+1),Form("canvas_shld_hit_vrtx%02d_pid%d",i+1,j+1),1500,500);	
        canvas_shld_hit[i][j]->Divide(n_energy_ranges,1);
        canvas_shld_hit_noWeight[i][j]= new TCanvas(Form("canvas_shld_hit_noWeight_vrtx%02d_pid%d",i+1,j+1),Form("canvas_shld_hit_noWeight_vrtx%02d_pid%d",i+1,j+1),1500,500);	
        canvas_shld_hit_noWeight[i][j]->Divide(n_energy_ranges,1);

        canvas_shld_hit_integral[i][j]= new TCanvas(Form("canvas_shld_hit_integral_vrtx%02d_pid%d",i+1,j+1),Form("canvas_shld_hit_integral_vrtx%02d_pid%d",i+1,j+1),1500,500);	
        canvas_shld_hit_integral[i][j]->Divide(n_energy_ranges,1);
        for(Int_t k=0;k<n_energy_ranges;k++){//KE
          canvas_shld_hit[i][j]->cd(k+1);
          canvas_shld_hit[i][j]->cd(k+1)->SetLogy();
          Histo_shld_hit[i][j][k]->DrawCopy();
          //plot integral
          norme_shld=Histo_shld_hit[i][j][k]->Integral();
          integral_shld=Histo_shld_hit[i][j][k]->GetIntegral();
          Histo_shld_hit[i][j][k]->SetContent(integral_shld);
          Histo_shld_hit[i][j][k]->Scale(norme_shld);
          canvas_shld_hit_integral[i][j]->cd(k+1);
          //canvas_shld_hit_integral[i][j]->cd(k+1)->SetLogy();
          Histo_shld_hit[i][j][k]->Draw();
          canvas_shld_hit_noWeight[i][j]->cd(k+1);
          canvas_shld_hit_noWeight[i][j]->cd(k+1)->SetLogy();
          Histo_shld_hit_noWeight[i][j][k]->Draw();
        }      
        if (kSave1DShldHitHisto){
          canvas_shld_hit[i][j]->SaveAs(plotsFolder+Form("canvas_shld_hit_vrtx%02d_pid%d.png",i+1,j+1));
          canvas_shld_hit_noWeight[i][j]->SaveAs(plotsFolder+Form("canvas_shld_hit_noWeight_vrtx%02d_pid%d.png",i+1,j+1));
          canvas_shld_hit_integral[i][j]->SaveAs(plotsFolder+Form("canvas_shld_hit_integral_vrtx%02d_pid%d.png",i+1,j+1));
        }
        if (kSaveRootFile){
          canvas_shld_hit[i][j]->Write();
          canvas_shld_hit_noWeight[i][j]->Write();
          canvas_shld_hit_integral[i][j]->Write();
        }
      }
    }
  }
  if (kShow2DRoofPlots){
  //2D radiation distr on cylinder and disk detectors
    Double_t hallrad_color_max;
    Double_t hallrad_color_min;
    for(Int_t i=0;i<n_regions;i++){//region
      for(Int_t j=0;j<n_particles;j++){//pid
      	for(Int_t k=1;k<2;k++){//detid (start at 1 to skip counting the phi plot from now on)
          Histo_RadDet[i][j][k]->Scale(1e9);
          if (Histo_RadDet[i][j][k]->GetMinimum()<hallrad_color_min) hallrad_color_min = Histo_RadDet[i][j][k]->GetMinimum();
          if (Histo_RadDet[i][j][k]->GetMaximum()>hallrad_color_max) hallrad_color_max = Histo_RadDet[i][j][k]->GetMaximum();	
        }
      }
    }
    TCanvas * canvas_hallrad[n_regions];
    for(Int_t i=0;i<n_regions;i++){//vertex // THESE ARE THE TOP Heatmaps vs. vertex region
      canvas_hallrad[i] = new TCanvas(Form("canvas_hallrad_vrtx%02d",i+1),Form("canvas_hallrad_vrtx%02d",i+1),1500,1500);
      canvas_hallrad[i]->Divide(3,n_particles);
      for(Int_t j=0;j<n_particles;j++){//pid
      	for(Int_t k=0;k<3;k++){//detid FIXME k=3=Floor??? FIXME// Start from 1 to remove the phi plots - to reimplement change these 1's back to 2's
	        canvas_hallrad[i]->cd(3*j+1+k); //previously n_particles*j+1+k when k went from 0 to 2
	        canvas_hallrad[i]->cd(3*j+1+k)->SetLogz();
          Histo_RadDet[i][j][k]->Draw("colz");	
          Histo_RadDet[i][j][1]->GetZaxis()->SetRangeUser(hallrad_color_min,hallrad_color_max);
	        Histo_RadDet[i][j][k]->SetStats(0);
	      }
      }
      if (kSave2DRoofHisto)
	      canvas_hallrad[i]->SaveAs(plotsFolder+Form("canvas_hallrad_vrtx%02d.png",i+1));
      if (kSaveRootFile)
	      canvas_hallrad[i]->Write();
    }
  }


  if (kShow2DCylPlots){
    Double_t hallrad_cyc_color_max;
    Double_t hallrad_cyc_color_min;
    for(Int_t i=0;i<n_regions;i++){//region
      for(Int_t j=0;j<n_particles;j++){//pid
      	for(Int_t k=3;k<5;k++){//detid (just the front and back hallrad scatter plots)
          Histo_RadDet[i][j][k]->Scale(1e9);
          if (Histo_RadDet[i][j][k]->GetMinimum()<hallrad_cyc_color_min) hallrad_cyc_color_min = Histo_RadDet[i][j][k]->GetMinimum();
          if (Histo_RadDet[i][j][k]->GetMaximum()>hallrad_cyc_color_max) hallrad_cyc_color_max = Histo_RadDet[i][j][k]->GetMaximum();	
        }
      }
    }
    TCanvas * canvas_hallrad_cyc_xy[n_regions];
    for(Int_t i=0;i<n_regions;i++){//vertex
      canvas_hallrad_cyc_xy[i] = new TCanvas(Form("canvas_hallrad_cyc_xy_vrtx%02d",i+1),Form("canvas_hallrad_cyc_xy_vrtx%02d",i+1),1500,1500);
      canvas_hallrad_cyc_xy[i]->Divide(2,n_particles);
      for(Int_t j=0;j<n_particles;j++){//pid
	      for(Int_t k=0;k<2;k++){//detid   forward and backward cylindrical detectors showing
	        canvas_hallrad_cyc_xy[i]->cd(2*j+1+k);
	        canvas_hallrad_cyc_xy[i]->cd(2*j+1+k)->SetLogz();
	        Histo_RadDet[i][j][3+k]->Draw("colz");	
          Histo_RadDet[i][j][3+k]->GetZaxis()->SetRangeUser(hallrad_cyc_color_min,hallrad_cyc_color_max);// start at 3 because we are skipping the phi, top, and side hallrad plots.
	        Histo_RadDet[i][j][3+k]->SetStats(0);
      	}
      }
      if (kSave2DCylHisto)
	      canvas_hallrad_cyc_xy[i]->SaveAs(plotsFolder+Form("canvas_hallrad_cyc_xy_vrtx%02d.png",i+1));
      if (kSaveRootFile)
	      canvas_hallrad_cyc_xy[i]->Write();
    }
  }

  //2D radiation vertex distr 
  if (kShow2DVertexPlots){
    Double_t hallrad_vert_color_max[2];
    Double_t hallrad_vert_color_min[2];
    for(Int_t i=0;i<n_regions;i++){//region
      for(Int_t j=0;j<n_particles;j++){//pid
        for(Int_t l=0;l<n_energy_ranges;l++){//pid
      	  for(Int_t k=0;k<2;k++){//kinds of plots = 2 
            HistoVertex_RadDet[i][j][l][k]->Scale(1e9);
            if (HistoVertex_RadDet[i][j][l][k]->GetMinimum()<hallrad_vert_color_min[k]) hallrad_vert_color_min[k] = HistoVertex_RadDet[i][j][l][k]->GetMinimum();
            if (HistoVertex_RadDet[i][j][l][k]->GetMaximum()>hallrad_vert_color_max[k]) hallrad_vert_color_max[k] = HistoVertex_RadDet[i][j][l][k]->GetMaximum();	
          }
        }
      }
    }
    TCanvas * canvas_hallrad_xy_vrtx[n_regions];
    //TCanvas * canvas_hallrad_rz_vrtx[n_regions];
    TCanvas * canvas_hallrad_yz_vrtx[n_regions];
    for(Int_t i=0;i<n_regions;i++){//vertex
      canvas_hallrad_xy_vrtx[i]=new TCanvas(Form("canvas_hallrad_xy_vrtx%02d",i+1),Form("canvas_hallrad_xy_vrtx%02d",i+1),1500,1500);
      //canvas_hallrad_rz_vrtx[i]=new TCanvas(Form("canvas_hallrad_rz_vrtx%d",i+1),Form("canvas_hallrad_rz_vrtx%d",i+1),1500,1500);
      canvas_hallrad_yz_vrtx[i]=new TCanvas(Form("canvas_hallrad_yz_vrtx%02d",i+1),Form("canvas_hallrad_yz_vrtx%02d",i+1),1500,1500);
      canvas_hallrad_xy_vrtx[i]->Divide(n_particles,n_energy_ranges); 
      //canvas_hallrad_rz_vrtx[i]->Divide(n_particles,n_energy_ranges);
      canvas_hallrad_yz_vrtx[i]->Divide(n_particles,n_energy_ranges);
      for(Int_t j=0;j<n_particles;j++){//pid
        for(Int_t k=0;k<n_energy_ranges;k++){//energy_ranges           
          canvas_hallrad_xy_vrtx[i]->cd(n_energy_ranges*j+1+k);
          canvas_hallrad_xy_vrtx[i]->cd(n_energy_ranges*j+1+k)->SetLogz();
          HistoVertex_RadDet[i][j][k][0]->Draw("colz");// THESE ARE THE vertices of particle birth plots	
          HistoVertex_RadDet[i][j][k][0]->GetZaxis()->SetRangeUser(hallrad_vert_color_min[0],hallrad_vert_color_max[0]);
          HistoVertex_RadDet[i][j][k][0]->SetStats(0);

          //canvas_hallrad_rz_vrtx[i]->cd(3*j+1+k);                   
          //canvas_hallrad_rz_vrtx[i]->cd(3*j+1+k)->SetLogz();
          //HistoVertex_RadDet[i][j][k][1]->Draw("colz");	
          //HistoVertex_RadDet[i][j][k][1]->SetStats(0);
          
          canvas_hallrad_yz_vrtx[i]->cd(n_energy_ranges*j+1+k);           
          canvas_hallrad_yz_vrtx[i]->cd(n_energy_ranges*j+1+k)->SetLogz();
          HistoVertex_RadDet[i][j][k][1]->Draw("colz");	
          HistoVertex_RadDet[i][j][k][1]->GetZaxis()->SetRangeUser(hallrad_vert_color_min[1],hallrad_vert_color_max[1]);
          HistoVertex_RadDet[i][j][k][1]->SetStats(0);
        }
      }
      if (kSave2DVertexHisto){
	      canvas_hallrad_xy_vrtx[i]->SaveAs(plotsFolder+Form("canvas_hallrad_xy_vrtx%02d.png",i+1));
	      //canvas_hallrad_rz_vrtx[i]->SaveAs(plotsFolder+Form("canvas_hallrad_rz_vrtx%d.png",i+1));
	      canvas_hallrad_yz_vrtx[i]->SaveAs(plotsFolder+Form("canvas_hallrad_yz_vrtx%02d.png",i+1));
      }
      if (kSaveRootFile){
      	canvas_hallrad_xy_vrtx[i]->Write();
      	//canvas_hallrad_rz_vrtx[i]->Write();
      	canvas_hallrad_yz_vrtx[i]->Write();
      }
    }
  }

  //end of plots for hall roof and wall detector, begin plots of shielding block detectors
  
  if (kShow2DShldVertexPlots){
    Double_t hallrad_shld_color_max[2];
    Double_t hallrad_shld_color_min[2];
    for(Int_t i=0;i<n_shlds;i++){//region
      for(Int_t j=0;j<n_particles;j++){//pid
        for(Int_t l=0;l<n_energy_ranges;l++){//pid
      	  for(Int_t k=0;k<2;k++){//kinds of plots = 2 
            HistoVertex_shld_RadDet[i][j][l][k]->Scale(1e9);
            if (HistoVertex_shld_RadDet[i][j][l][k]->GetMinimum()<hallrad_shld_color_min[k]) hallrad_shld_color_min[k] = HistoVertex_shld_RadDet[i][j][l][k]->GetMinimum();
            if (HistoVertex_shld_RadDet[i][j][l][k]->GetMaximum()>hallrad_shld_color_max[k]) hallrad_shld_color_max[k] = HistoVertex_shld_RadDet[i][j][l][k]->GetMaximum();	
          }
        }
      }
    }
    TCanvas * canvas_shld_hallrad_xy_vrtx[n_shlds];
    TCanvas * canvas_shld_hallrad_yz_vrtx[n_shlds];
    for(Int_t i=0;i<n_shlds;i++){//vertex
      canvas_shld_hallrad_xy_vrtx[i]=new TCanvas(Form("canvas_shld_hallrad_xy_vrtx%02d",i+1),Form("canvas_shld_hallrad_xy_vrtx%02d",i+1),1500,1500);
      canvas_shld_hallrad_yz_vrtx[i]=new TCanvas(Form("canvas_shld_hallrad_yz_vrtx%02d",i+1),Form("canvas_shld_hallrad_yz_vrtx%02d",i+1),1500,1500);
      canvas_shld_hallrad_xy_vrtx[i]->Divide(n_particles,n_energy_ranges);
      canvas_shld_hallrad_yz_vrtx[i]->Divide(n_particles,n_energy_ranges);
      for(Int_t j=0;j<n_particles;j++){//pid
        for(Int_t k=0;k<n_energy_ranges;k++){//energy ranges    
          canvas_shld_hallrad_xy_vrtx[i]->cd(n_energy_ranges*j+1+k);
          canvas_shld_hallrad_xy_vrtx[i]->cd(n_energy_ranges*j+1+k)->SetLogz();
          HistoVertex_shld_RadDet[i][j][k][0]->Draw("colz");// THESE ARE THE vertices of particle birth plots	for hitting a given sensitive shielding block
          HistoVertex_shld_RadDet[i][j][k][0]->GetZaxis()->SetRangeUser(hallrad_shld_color_min[0],hallrad_shld_color_max[0]);	
          HistoVertex_shld_RadDet[i][j][k][0]->SetStats(0);

          canvas_shld_hallrad_yz_vrtx[i]->cd(n_energy_ranges*j+1+k);   
          canvas_shld_hallrad_yz_vrtx[i]->cd(n_energy_ranges*j+1+k)->SetLogz();
          HistoVertex_shld_RadDet[i][j][k][1]->Draw("colz");	
          HistoVertex_shld_RadDet[i][j][k][1]->GetZaxis()->SetRangeUser(hallrad_shld_color_min[1],hallrad_shld_color_max[1]);	
          HistoVertex_shld_RadDet[i][j][k][1]->SetStats(0);
        }
      }
      if (kSave2DShldVertexHisto){
	      canvas_shld_hallrad_xy_vrtx[i]->SaveAs(plotsFolder+Form("canvas_shld_hallrad_xy_vrtx%02d.png",i+1));
	      canvas_shld_hallrad_yz_vrtx[i]->SaveAs(plotsFolder+Form("canvas_shld_hallrad_yz_vrtx%02d.png",i+1));
      }
      if (kSaveRootFile){
      	canvas_shld_hallrad_xy_vrtx[i]->Write();
      	canvas_shld_hallrad_yz_vrtx[i]->Write();
      }
    }
  }
  if (kShow2DShldHitPlots){
    Double_t hallrad_shld_hit_color_max[2];
    Double_t hallrad_shld_hit_color_min[2];
    for(Int_t i=0;i<n_shlds;i++){//region
      for(Int_t j=0;j<n_particles;j++){//pid
        for(Int_t l=0;l<n_energy_ranges;l++){//pid
      	  for(Int_t k=0;k<2;k++){//kinds of plots = 2 
            HistoHit_shld_RadDet[i][j][l][k]->Scale(1e9);
            if (HistoHit_shld_RadDet[i][j][l][k]->GetMinimum()<hallrad_shld_hit_color_min[k]) hallrad_shld_hit_color_min[k] = HistoHit_shld_RadDet[i][j][l][k]->GetMinimum();
            if (HistoHit_shld_RadDet[i][j][l][k]->GetMaximum()>hallrad_shld_hit_color_max[k]) hallrad_shld_hit_color_max[k] = HistoHit_shld_RadDet[i][j][l][k]->GetMaximum();	
          }
        }
      }
    }
    TCanvas * canvas_shld_hit_hallrad_xy_vrtx[n_shlds];
    TCanvas * canvas_shld_hit_hallrad_yz_vrtx[n_shlds];
    for(Int_t i=0;i<n_shlds;i++){//vertex
      canvas_shld_hit_hallrad_xy_vrtx[i]=new TCanvas(Form("canvas_shld_hit_hallrad_xy_vrtx%02d",i+1),Form("canvas_shld_hit_hallrad_xy_vrtx%02d",i+1),1500,1500);
      canvas_shld_hit_hallrad_yz_vrtx[i]=new TCanvas(Form("canvas_shld_hit_hallrad_yz_vrtx%02d",i+1),Form("canvas_shld_hit_hallrad_yz_vrtx%02d",i+1),1500,1500);
      canvas_shld_hit_hallrad_xy_vrtx[i]->Divide(n_particles,n_energy_ranges); 
      canvas_shld_hit_hallrad_yz_vrtx[i]->Divide(n_particles,n_energy_ranges);
      for(Int_t j=0;j<n_particles;j++){//pid
        for(Int_t k=0;k<n_energy_ranges;k++){//energy ranges    
          canvas_shld_hit_hallrad_xy_vrtx[i]->cd(n_energy_ranges*j+1+k);
          canvas_shld_hit_hallrad_xy_vrtx[i]->cd(n_energy_ranges*j+1+k)->SetLogz();
          HistoHit_shld_RadDet[i][j][k][0]->Draw("colz");// THESE ARE THE particle impact hit position plots	
          HistoHit_shld_RadDet[i][j][k][0]->GetZaxis()->SetRangeUser(hallrad_shld_hit_color_min[0],hallrad_shld_hit_color_max[0]);	
          HistoHit_shld_RadDet[i][j][k][0]->SetStats(0);

          canvas_shld_hit_hallrad_yz_vrtx[i]->cd(n_energy_ranges*j+1+k);   
          canvas_shld_hit_hallrad_yz_vrtx[i]->cd(n_energy_ranges*j+1+k)->SetLogz();
          HistoHit_shld_RadDet[i][j][k][1]->Draw("colz");	
          HistoHit_shld_RadDet[i][j][k][1]->GetZaxis()->SetRangeUser(hallrad_shld_hit_color_min[1],hallrad_shld_hit_color_max[1]);	
          HistoHit_shld_RadDet[i][j][k][1]->SetStats(0);
        }
      }
      if (kSave2DShldHitHisto){
	      canvas_shld_hit_hallrad_xy_vrtx[i]->SaveAs(plotsFolder+Form("canvas_shld_hit_hallrad_xy_vrtx%02d.png",i+1));
	      canvas_shld_hit_hallrad_yz_vrtx[i]->SaveAs(plotsFolder+Form("canvas_shld_hit_hallrad_yz_vrtx%02d.png",i+1));
      }
      if (kSaveRootFile){
      	canvas_shld_hit_hallrad_xy_vrtx[i]->Write();
      	canvas_shld_hit_hallrad_yz_vrtx[i]->Write();
      }
    }
  }

  // Textfile outputs
  const char * detector[2+n_shlds];
  detector[0]="Side";detector[1]="Top";
  for(Int_t d=0;d<n_shlds;d++){
    detector[2+d] = svertex_shld[d].Data(); 
    //"Total","Side","Top","Hall","TargetHut","TargetHutPoly","LeadCollar","LeadCollarPoly","Coll1ShldUS","Coll1ShldDS","Coll1ShldPoly","Coll4Shld","Coll4ShldPoly","HybridShld"};
  }
  const char * chpid[n_particles]         = {"e+-","photon","n0"};
  const char * chenrange[n_energy_ranges] = {"E<10","10<E<30","30<E"};

  TList * list_power = new TList;
  TString strline;
  char line[600];
  char line1[600];
  char line2[600];
  strline="Rootfile_name";
  list_power->Add(new TObjString(strline));
  list_outputs << strline << endl;
  //strline=added_file;
  strline=added_file_array[1];
  list_power->Add(new TObjString(strline));
  list_outputs << strline << endl;
  // POWER
  strline="Total_Radiation_Power_into_the_specified_detector_(MeV/event)";
  list_power->Add(new TObjString(strline));
  list_outputs << strline << endl;
  printf(" \n Total_Radiation_Power_into_the_specified_detector_(MeV/event) \n");
  printf(" %20s %20s","Type","E_Range_(MeV/event)");
  sprintf(line," %20s %20s","Type","E_Range_(MeV/event)");
  for(Int_t t=0;t<2+n_shlds;t++){
    printf(" %13s",detector[t]);
    sprintf(line,"%s %13s",line,detector[t]);
  }
  printf(" \n");
  list_power->Add(new TObjString(line));
  list_outputs << line << endl;
  Double_t sum=0;
  Double_t shld_sum=0;
 
  // FIXME Add a variable that gets += every time a unique count of radiation gets printed in order to sum up all the radiation being produced/absorbed across all regions
  for(Int_t i=0;i<n_particles;i++){//pid
    for(Int_t j=0;j<n_energy_ranges;j++){//energy range
      printf(" %20s %20s",chpid[i],chenrange[j]);
      sprintf(line," %20s %20s",chpid[i],chenrange[j]);
      sprintf(line1," ");//empty previous values
      for(Int_t k=0;k<2;k++){//detector                             // number of hall (roof, walls) detectors present
	      for (Int_t s=0;s<n_regions;s++)
	        sum+=power_local[s][k][i][j];//sum over all the vertices
        printf("%12.3E",sum/n_events);
	      sprintf(line1,"%s %12.3E",line1,sum/n_events);
	      sum=0;
      }
      for(Int_t k=0;k<n_shlds;k++){
	      shld_sum+=shld_power_local[k][i][j];
        printf("%12.3E",shld_sum/n_events);
	      sprintf(line1,"%s %12.3E",line1,shld_sum/n_events);
	      shld_sum=0;
      }
      printf("\n");
      sprintf(line," %s %s",line,line1);
      list_power->Add(new TObjString(line));
      list_outputs << line << endl;
    }
  }

  printf(" \n Vertex_Cut:Radiation_Power_into_the_hall_(MeV/event) \n");
  strline="Vertex_Cut:Radiation_Power_into_the_hall_(MeV/event)";
  list_power->Add(new TObjString(strline));
  list_outputs << strline << endl;
  printf(" %20s %20s %20s \t %13s \t %13s \n","Vertex","Type","E_Range_(MeV/event)",detector[0],detector[1]);
  sprintf(line," %20s %20s %20s \t %13s \t %13s ","Vertex","Type","E_Range_(MeV/event)",detector[0],detector[1]);
  list_power->Add(new TObjString(line));
  list_outputs << line << endl;
  for (Int_t i=0;i<n_regions;i++){
    for(Int_t j=0;j<n_particles;j++){//pid
      for(Int_t k=0;k<n_energy_ranges;k++){//energy range
	      printf(" %20s %20s %20s",svertex[i].Data(),chpid[j],chenrange[k]);
	      sprintf(line," %20s %20s %20s",svertex[i].Data(),chpid[j],chenrange[k]);
	      sprintf(line1," ");//empty previous values
	      for(Int_t l=0;l<2;l++){//detector                             // number of hall (roof, wall) detectors present
	        printf("%12.3E",power_local[i][l][j][k]/n_events);
          sprintf(line1,"%s %12.3E",line1,power_local[i][l][j][k]/n_events);
	      }
	      printf("\n");
	      sprintf(line," %s %s",line,line1);
	      list_power->Add(new TObjString(line));
        list_outputs << line << endl;
      }
    }
  }
  
  printf(" \n ShldBlock_Cut:Radiation_Power_into_the_Shielding_Blocks_(MeV/event) \n");
  strline="ShldBlock_Cut:Radiation_Power_into_the_Shielding_Blocks_(MeV/event)";
  list_power->Add(new TObjString(strline));
  list_outputs << strline << endl;
  printf(" %20s %20s %20s","ShldBlock","Type","E_Range_(MeV/event)");
  sprintf(line," %20s %20s %20s","ShldBlock","Type","E_Range_(MeV/event)");
  printf(" \n");
  list_power->Add(new TObjString(line));
  list_outputs << line << endl;
  for (Int_t i=0;i<n_shlds;i++){
    for(Int_t j=0;j<n_particles;j++){//pid
      for(Int_t k=0;k<n_energy_ranges;k++){//energy range
	      printf(" %20s %20s %20s",svertex_shld[i].Data(),chpid[j],chenrange[k]);
	      sprintf(line," %20s %20s %20s",svertex_shld[i].Data(),chpid[j],chenrange[k]);
	      sprintf(line1," ");//empty previous values
	      printf("%12.3E",shld_power_local[i][j][k]/n_events);
        sprintf(line1,"%s %12.3E",line1,shld_power_local[i][j][k]/n_events);
	      printf("\n");
	      sprintf(line," %s %s",line,line1);
	      list_power->Add(new TObjString(line));
        list_outputs << line << endl;
      }
    }
  }

  // FLUX  
  sum=0;
  shld_sum=0;
  TList * list_flux = new TList;
  printf(" \n Total_Radiation_Flux_into_the_specified_detector_(Counts, for %d events)\n",n_events);
  sprintf(line2,"Total_Radiation_Flux_into_the_specified_detector_(Counts, for %d events",n_events);
  list_flux->Add(new TObjString(line2));
  list_outputs << line2 << endl;
  printf(" %20s %20s","Type","E_Range_(MeV)");
  sprintf(line," %20s %20s","Type","E_Range_(MeV)");
  for(Int_t t=0;t<2+n_shlds;t++){
    printf(" %13s",detector[t]);
    sprintf(line,"%s %13s",line,detector[t]);
  }
  printf(" \n");
  list_flux->Add(new TObjString(line));
  list_outputs << line << endl;
  for(Int_t i=0;i<n_particles;i++){//pid
    for(Int_t j=0;j<n_energy_ranges;j++){//energy range
      printf(" %20s %20s",chpid[i],chenrange[j]);
      sprintf(line," %20s %20s",chpid[i],chenrange[j]);
      sprintf(line1," ");//empty previous values
      for(Int_t k=0;k<2;k++){//detector                             // number of hall (roof, walls) detectors present
	      for (Int_t s=0;s<n_regions;s++)
	        sum+=flux_local[s][k][i][j];//sum over all the vertices
        printf("%12.3E",sum);
	      sprintf(line1,"%s %12.3E",line1,sum);
	      sum=0;
      }
      for(Int_t k=0;k<n_shlds;k++){
        shld_sum+=shld_flux_local[k][i][j];
        printf("%12.3E",shld_sum);
	      sprintf(line1,"%s %12.3E",line1,shld_sum);
	      shld_sum=0;
      }
      printf("\n");
      sprintf(line," %s %s",line,line1);
      list_flux->Add(new TObjString(line));
      list_outputs << line << endl;
    }
  }

  printf(" \n Vertex_Cut:Radiation_Flux_into_the_hall_(Counts, for %d events)\n",n_events);
  sprintf(line2,"Vertex_Cut:Radiation_Flux_into_the_hall_(Counts, for %d events)",n_events);
  list_flux->Add(new TObjString(line2));
  list_outputs << line2 << endl;
  printf(" %20s %20s %20s \t %13s \t %13s \n","Vertex","Type","E_Range_(MeV)",detector[0],detector[1]);
  sprintf(line," %20s %20s %20s \t %13s \t %13s ","Vertex","Type","E_Range_(MeV)",detector[0],detector[1]);
  list_flux->Add(new TObjString(line));
  list_outputs << line << endl;
  for (Int_t i=0;i<n_regions;i++){
    for(Int_t j=0;j<n_particles;j++){//pid
      for(Int_t k=0;k<n_energy_ranges;k++){//energy range
        printf(" %20s %20s %20s",svertex[i].Data(),chpid[j],chenrange[k]);
        sprintf(line," %20s %20s %20s",svertex[i].Data(),chpid[j],chenrange[k]);
        sprintf(line1," ");//empty previous values
        for(Int_t l=0;l<2;l++){//detector                             // number of hall (roof, walls) detectors present
          printf("%12.3E",flux_local[i][l][j][k]);
          sprintf(line1,"%s %12.3E",line1,flux_local[i][l][j][k]);
        }
        printf("\n");
        sprintf(line," %s %s",line,line1);
        list_flux->Add(new TObjString(line));
        list_outputs << line << endl;
      }
    }
  }
  
  printf(" \n ShldBlock_Cut:Radiation_Flux_into_the_Shielding_Blocks_(Counts, for %d events)\n",n_events);
  sprintf(line2,"ShldBlock_Cut:Radiation_Flux_into_the_Shielding_Blocks_(Counts, for %d events)",n_events);
  list_flux->Add(new TObjString(line2));
  list_outputs << line2 << endl;
  printf(" %20s %20s %20s","ShldBlock","Type","E_Range_(MeV)");
  sprintf(line," %20s %20s %20s","ShldBlock","Type","E_Range_(MeV)");
  printf(" \n");
  list_flux->Add(new TObjString(line));
  list_outputs << line << endl;
  for (Int_t i=0;i<n_shlds;i++){
    for(Int_t j=0;j<n_particles;j++){//pid
      for(Int_t k=0;k<n_energy_ranges;k++){//energy range
	      printf(" %20s %20s %20s",svertex_shld[i].Data(),chpid[j],chenrange[k]);
	      sprintf(line," %20s %20s %20s",svertex_shld[i].Data(),chpid[j],chenrange[k]);
	      sprintf(line1," ");//empty previous values
	      printf("%12.3E",shld_flux_local[i][j][k]);
        sprintf(line1,"%s %12.3E",line1,shld_flux_local[i][j][k]);
	      printf("\n");
	      sprintf(line," %s %s",line,line1);
	      list_flux->Add(new TObjString(line));
        list_outputs << line << endl;
      }
    }
  }

  if (kSaveRootFile){
    rootfile->WriteObject(list_power,"textout_power");
    rootfile->WriteObject(list_flux,"textout_flux");
    rootfile->Write();
  }

  if(kShowGraphic)
    theApp.Run();

  if (kSaveRootFile){
    rootfile->Close();
  }

  list_outputs.close();
  return(1);
}

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
