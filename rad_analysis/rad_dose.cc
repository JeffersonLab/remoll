/*
Rakitha Mon Aug 25 14:48:01 EDT 2014
Plot generated related to the radiation in the hall

Radiation plots
KE intercepted by the cylindrical and two disk detectors
vertex distribution on the cylindrical and two disk detectors

Vertex cuts are based on the shielding blocks

//////

Cameron Clarke Wednesday July 26 14:41:57 EDT 2017
Plots added for y vs z vertex distributions

*/

#include <vector>
#include <string>
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

using namespace std;

#define __IO_MAXHIT 10000


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
Int_t hybrid_hut = 6024;        // Hybrid toroid's phased out shielding hut
Int_t hybrid_poly_hut = 6025;   // Hybrid toroid's phased out poly hut
Int_t hybrid_lead_roof = 6027;  // Hybrid toroid's lead roof


Int_t SensVolume_v[n_detectors] = {99,101,103,6000,6003,6004,6007,6008,6010,6011,6012,6020,6021,6024,6025,6027};//Detector numbers (wall, ceiling, floor, hall, lead target hut, poly target hut, lead collar, poly collar, block 1, block 2, blocks 1 and 2 poly shield, block 3, block 3's poly shield, hybrid concrete hut, hybrid poly hut, hybrid lead roof)//look at everything going out to the hall

const int n_energy_ranges = 3;
const int n_particles = 3;

const int n_regions = 8; // originally 6, used for mapping localized radiation to the whole hall
const int n_sinks = 8;   // used for mapping radiation into any one spot
const int n_sources = 8; // used for mapping total radiation versus which sensitive detectors it originiated from, not including radiation into those regions themselves
////Flux and power parameters Full range //indices [region][detector][energy range][pid] = [8][16][3][3] - 3 energy ranges for e,gamma, n for three hall detectors. 
Double_t flux_local[n_regions][n_detectors][n_energy_ranges][n_particles]={{{{0}}}}; // The last index is for the shieldings: target, shielding blocks 1 to 4, and other vertices
Double_t power_local[n_regions][n_detectors][n_energy_ranges][n_particles]={{{{0}}}};
// NEW
Double_t flux_sinks[n_sinks][n_detectors][n_energy_ranges][n_particles]={{{{0}}}};  // The last index is for mapping where the radiation lands specifically, xyz coordinates are the origin vertices.
Double_t power_sinks[n_sinks][n_detectors][n_energy_ranges][n_particles]={{{{0}}}};
Double_t flux_sources[n_sources][n_detectors][n_energy_ranges][n_particles]={{{{0}}}}; // The last index is for mapping where the radiation originated from, xyz coordinates are the hit position (any hit, not just ceiling or wall).
Double_t power_sources[n_sources][n_detectors][n_energy_ranges][n_particles]={{{{0}}}};

std::map<int,int> detectormap;
std::map<int,int> pidmap;
std::map<int,double> pidmass;

// FIXME get the hit_radius cuts right based on new beam pipes etc.
Double_t hit_radius_min[2] = {0.46038,0.46038}; //m inner radius of the beam pipe 45.72 cm and outer radius of the beam pipe 46.038 cm and radius of the detector plane is 1.9 m
Double_t hit_radius;
Double_t kineE;

//boolean switches

Bool_t earlybreak=kFALSE;       //exit early from the main tree entries loop

Bool_t kSaveRootFile=kTRUE;     //save histograms and canvases into a rootfile
Bool_t kShowGraphic=kTRUE;      //Show canvases and they will be saved the rootfile. Set this to false to show no graphics but save them to rootfile
//Bool_t kShowGraphic=kFALSE    //Show canvases and they will be saved the rootfile. Set this to false to show no graphics but save them to rootfile

//Boolean parameter to disable/enable saving histograms as png
Bool_t kSave1DKEHisto=kTRUE;    //option to save histograms
Bool_t kSave1DVrtxHisto=kTRUE;  //option to save histograms

Bool_t kSave2DHisto=kTRUE;      //option to save 2D hit distribution histograms
Bool_t kSave2DVertexHisto=kTRUE;//option to save 2D vertex distribution

Bool_t kShow1DKEPlots=kTRUE;    //disabling the show the canvases will not be shown or written to the root files
Bool_t kShow1DVrtxPlots=kTRUE;
Bool_t kShow2Dhits=kTRUE;
Bool_t kShow2DVertex=kTRUE;

//end of boolean switches
void set_plot_style();

TFile * rootfile;
int main(Int_t argc,Char_t* argv[]) {
  TApplication theApp("App",&argc,argv);

  ofstream list_outputs;

  set_plot_style();

  //to ignore high energy moller scattering at the detector plane use,
  //hit_radius_min[0] = 1.0;
  //hit_radius_min[1] = 1.0;

  //remoll Tree
  TChain * Tmol =new TChain("T");
  //Cameron Clarke runs:
  //input info:
  Int_t n_events=1e6;
  Int_t beamcurrent = 85;//uA

  TString added_file="/home/cameronc/gitdir/dose_remoll/output/beam_tracking_1M.root";
  TString plotsFolder="/home/cameronc/gitdir/dose_remoll/output/Plots_tracking_1M_NEW/";//Name of folder for saving plots
  TString rootfilename=plotsFolder+"tracking_1M_plots_NEW.root";//name of the rootfile to save generated histograms
  list_outputs.open(plotsFolder+"list_outputs_tracking_1M_NEW.txt");
  
  Tmol->Add(added_file);
  list_outputs << "Contents of textout_flux and textout_power lists of strings" << std::endl;

  // Alright: Everything necessary exists for me to plot vertex vs. z plots, but the R vs. z plots are probably enough for now to be honest (need to be set to some absolute scale). It would be nice to have a radiation at top of hall plot vs. z (color mapped maybe) distribution as well though. I also want to generate plots of the last significant scattering vertex, not just the particle creation vertices, in case these are significantly different.

  //generic hit (for sens detectors)
  Tmol->SetBranchAddress("rate",&fEvRate);
  Tmol->SetBranchAddress("hit.n",&fNGenDetHit);
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
  Tmol->SetBranchAddress("hit.lx",&fGenDetHit_Lx);
  Tmol->SetBranchAddress("hit.ly",&fGenDetHit_Ly);
  Tmol->SetBranchAddress("hit.lz",&fGenDetHit_Lz);
  Tmol->SetBranchAddress("hit.ldTh",&fGenDetHit_LdTh);
  Tmol->SetBranchAddress("hit.ldE",&fGenDetHit_LdE);
  Tmol->SetBranchAddress("hit.edep",&fGenDetHit_edep);

  Int_t nentries = (Int_t)Tmol->GetEntries();

  if (kSaveRootFile){
    TString rootfilestatus="RECREATE";
    rootfile = new TFile(rootfilename, rootfilestatus);
    rootfile->cd();
  }

  gROOT->SetStyle("Plain");
  //gStyle->SetOptStat(0); 
  gStyle->SetOptStat("eMR");

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
  
  //indices asigned to pid numbers
  pidmap[11]=0; //electron 
  pidmap[22]=1; //photon
  pidmap[2112]=2; //neutron

  pidmass[11]=0.511;//MeV
  pidmass[22]=0.0;
  pidmass[2112]=939.565;//MeV

  //hall radiation from all source, target, collimators separated for ep,gamma, and n for three energy ranges

  // FIXME add more histograms for other kinds of plots
  TH1F *Histo_kineE[n_regions+1][n_particles][n_energy_ranges];
  TH1F *Histo_vertex[n_regions][n_particles][n_energy_ranges];
  TH1F *Histo_vertex_noWeight[n_regions][n_particles][n_energy_ranges];


  //hall radiation 2D hit distribution for the cylinder using y vs phi and using x vs. y for disks
  /*
    When using cylindrical coordinates, the hit distr from z>0 and z<0 are projected into the y vs phi plane where phi goes from -90 to 90
   */
  TH2F *Histo_RadDet[n_regions+1][n_particles][5];//6 vertex ranges for three particles species for three detectors (0-cylinder phi-y, 1 and 2 are top and bottom disk detectors, 3,4 is cylinder x-y forward backward)

  TH2F *HistoVertex_RadDet[n_regions+1][n_particles][n_energy_ranges][3];//6 vertex ranges for three particles species for three energy ranges for y:x, and r:z, and y:z NEW //I may extend to add x:z, and y:z vertex distributions
  //TH2F *HistoVertex_RadDet[n_regions][n_particles][n_energy_ranges][2];//6 vertex ranges for three particles species for three energy ranges for y:x, and r:z //I may extend to add x:z, and y:z vertex distributions

  // FIXME consider reading these xyz cuts from an external file for ease of shielding and collimator modifications (extrude beamlines too?).
  // FIXME Add more cuts for different kinds of ranges
  //                                      { change the binning to reflect the opposite nature, good, all binned in one spot, decent-needs better boundaries->775?, 775?, decent, decent, seems to miss a whole lot }
  //                                      { hall**            ,  target       ,  collar        ,  coll1shld, +magnet ,  coll4shld,  hybshld         ,  other} -> Hall is an inverted volume in x and z, not y.
  Double_t z_vertex_cuts_low[n_regions] = {-235.-80           , -235.-80.     ,  285.1+10.-20. ,  403.     ,  775.   ,  812.     ,  992.            , -600. };//last index store vertices outside of other ranges 
  Double_t z_vertex_cuts_up[n_regions]  = { 1781.837          ,  235.+80.     ,  285.1+10.+20. ,  775.     ,  812.   ,  992.     ,  1821.837        ,  1822.};
  Double_t x_vertex_cuts_low[n_regions] = {-296.5             ,  91.5-296.-80., -18.5-20.      , -213.     , -386.84 , -386.84   ,  91.5-285.75-40. , -500. };
  Double_t x_vertex_cuts_up[n_regions]  = { 296.5             ,  91.5+296.+80.,  18.5+20.      ,  213.     ,  386.84 ,  386.84   ,  91.5+285.75+40. ,  500. };
  Double_t y_vertex_cuts_low[n_regions] = {-330.              , -40.-250.-40. , -18.5-20.      , -213.     , -290.   ,  -290.     , -40.-250.-40.    , -500. };
  Double_t y_vertex_cuts_up[n_regions]  = { 330.              , -40.+250.+40. ,  18.5+20.      ,  213.     ,  290.   ,  290.     , -40.+250.+40.    ,  500. };
  Double_t R_vertex_cuts_up[n_regions]  = { 5000.             ,  500.         ,  75.           ,  350.     ,  500.   ,  500.     ,  450.            ,  500. };
  Int_t    x_vertex_bin_counts[n_regions]={ 1000, 300, 50, 300, 300, 300, 300, 1000}; // default, overridden below
  Int_t    y_vertex_bin_counts[n_regions]={ 1000, 300, 50, 300, 300, 300, 300, 1000};
  Int_t    z_vertex_bin_counts[n_regions]={ 1000, 300, 50, 300, 300, 300, 300, 1000};
  Int_t    R_vertex_bin_counts[n_regions]={ 1000, 300, 50, 300, 300, 300, 300, 1000};
  // FIXME I feel like I should be using a constant bin/area metric, instead of arbitrary as it is now -> this would help the net energy color scales/binning problem.
  Int_t    x_area_per_bin = 1; // 1 cm per bin
  Int_t    y_area_per_bin = 1; // 1 cm per bin
  Int_t    z_area_per_bin = 1; // 1 cm per bin
  Int_t    R_area_per_bin = 1; // 1 cm per bin
  for (int q=1;q<n_regions;q++){  
    z_vertex_bin_counts[q] = (z_vertex_cuts_up[q]-z_vertex_cuts_low[q])/z_area_per_bin;
    x_vertex_bin_counts[q] = (x_vertex_cuts_up[q]-x_vertex_cuts_low[q])/x_area_per_bin;
    y_vertex_bin_counts[q] = (y_vertex_cuts_up[q]-y_vertex_cuts_low[q])/y_area_per_bin;
    R_vertex_bin_counts[q] = (R_vertex_cuts_up[q]-0)/R_area_per_bin;
  }

  z_vertex_bin_counts[0] = 6000;
  x_vertex_bin_counts[0] = 6000;
  y_vertex_bin_counts[0] = 3000;
  R_vertex_bin_counts[0] = 3000;
  Double_t Hall_z_vertices_low = -3000;
  Double_t Hall_z_vertices_up  =  3000;
  Double_t Hall_x_vertices_low = -3000;
  Double_t Hall_x_vertices_up  =  3000;
  Double_t Hall_y_vertices_low = -1000;
  Double_t Hall_y_vertices_up  =  2000;
  Double_t Hall_R_vertices_up  =  3000;

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

  Double_t energy_cut_low[n_energy_ranges]={0,10,100};
  Double_t energy_cut_up[n_energy_ranges]={10,100,12000};
  // FIXME why these energy upper limits? Ask Rakitha?
  Int_t bin_ranges[n_regions+1][n_particles][n_energy_ranges+1]={
           {{0,10,100,12000},{0,10,100,4000},{0,10,100,4000}},    //Hall               (elec,gamma,neutron)
           {{0,10,100,12000},{0,10,100,4000},{0,10,100,4000}},    //target 
           {{0,10,100,12000},{0,10,100,4000},{0,10,100,4000}},    //lead collar 
			   //
           {{0,10,100,12000},{0,10,100,4000},{0,10,100,4000}},     //Shielding Block 1 and 2
			     {{0,10,100,12000},{0,10,100,4000},{0,10,100,4000}},     //Shielding Block 3
			     {{0,10,100,12000},{0,10,100,4000},{0,10,100,4000}},     //+Magnet region
           {{0,10,100,12000},{0,10,100,4000},{0,10,100,4000}},     //Shielding Block 4
			   // << these were only up to 1000 energy range, changed to match
			     {{0,10,100,12000},{0,10,100,4000},{0,10,100,4000}},    //other
			     {{0,10,100,12000},{0,10,100,4000},{0,10,100,4000}}     //total
  };
  // FIXME add more, make more clear
  //                                       { hall**,  target , collar, coll1shld,+magnet ,coll4shld,hybshld,other} -> Hall is an inverted volume in x and z, not y.
  Int_t vertex_bin_ranges_low[n_regions] = {-3000. , -235.-80., 295.1-10.-10., 403., 770., 812., 992.    , -600. };//last index store vertices outside of other ranges 
  Int_t vertex_bin_ranges_up[n_regions]  = { 3000. ,  235.+80., 295.1+10.+10., 770., 812., 992., 1821.837,  1822.};
  Int_t vertex_bin_counts[n_regions]     = { 2000  ,  500     , 50           , 500 , 500 , 500 , 500     ,  2000 };
  //Int_t vertex_bin_ranges_low[n_regions] = {-235.-80 , -235.-80., 295.1-10.-10., 403., 770., 812., 992.    , -600. };//last index store vertices outside of other ranges 
  //Int_t vertex_bin_ranges_up[n_regions]  = { 1781.837,  235.+80., 295.1+10.+10., 770., 812., 992., 1821.837,  1822.};
  TString ke_range[n_energy_ranges] = {"KE<10","10<KE<100","100<KE"};
  TString spid[n_particles]={"e-","#gamma","n0"};
  TString svertex[n_regions+1]={"Hall","ShTarget","LeadCollar","Coll1Shld","+Magnet","Coll4Shld","HybridShld","Other","All"};     // The Hall, the target hut, the lead collar, the first and second shielding blocks around the 1st collimator, the shielding block in front of the hybrid and collimator 4 (and 3), the hybrid shielding hut or roof, everything else, everything.
  TString svertex_2D[n_regions]={"Hall","ShTarget","LeadCollar","Coll1Shld","+Magnet","Coll4Shld","HybridShld","All"};            // last index stores hits not within tgt or collimators


  // FIXME Define more/less histograms
  for(Int_t i=0;i<n_regions+1;i++){//vertices
    for(Int_t j=0;j<n_particles;j++){//pid
      for(Int_t k=0;k<n_energy_ranges;k++){//KE
	      //1D radiation histograms
        Histo_kineE[i][j][k]=new TH1F(Form("Histo_kineE_%d_%d_%d",i+1,j+1,k+1),Form("%s from %s Area in %s MeV Range; KineE (MeV)",spid[j].Data(),svertex[i].Data(),ke_range[k].Data()),100,bin_ranges[i][j][k],bin_ranges[i][j][k+1]);
	      if (i<n_regions){
	        Histo_vertex[i][j][k]=new TH1F(Form("Histo_vertex_%d_%d_%d",i+1,j+1,k+1),Form("%s Vertices from %s Area in %s MeV Range (KE weighted);Z vertex (cm);W/#muA",spid[j].Data(),svertex[i].Data(),ke_range[k].Data()),vertex_bin_counts[i],vertex_bin_ranges_low[i],vertex_bin_ranges_up[i]);
	        Histo_vertex_noWeight[i][j][k]=new TH1F(Form("Histo_vertex_noWeight_%d_%d_%d",i+1,j+1,k+1),Form("%s Vertices from %s Area in %s MeV Range ;Z vertex (cm);Hz/#muA",spid[j].Data(),svertex[i].Data(),ke_range[k].Data()),vertex_bin_counts[i],vertex_bin_ranges_low[i],vertex_bin_ranges_up[i]);
        }
        if (i<n_regions){
	        //2D vertex distribution histograms
	        if (i>0){
            HistoVertex_RadDet[i][j][k][0]=new TH2F(Form("HistoVertex_RadDet_v%d_p%d_k%d_0",i+1,j+1,k+1),Form(" %s from %s Area in %s MeV Range; x (cm); y (cm); (W/#muA)",spid[j].Data(),svertex_2D[i].Data(),ke_range[k].Data()),x_vertex_bin_counts[i],x_vertex_cuts_low[i]-1,x_vertex_cuts_up[i]+1,y_vertex_bin_counts[i],y_vertex_cuts_low[i]-1,y_vertex_cuts_up[i]+1);
	          HistoVertex_RadDet[i][j][k][1]=new TH2F(Form("HistoVertex_RadDet_v%d_p%d_k%d_1",i+1,j+1,k+1),Form(" %s from %s Area in %s MeV Range; z (cm); R (cm); (W/#muA)",spid[j].Data(),svertex_2D[i].Data(),ke_range[k].Data()),z_vertex_bin_counts[i],z_vertex_cuts_low[i]-1,z_vertex_cuts_up[i]+1,R_vertex_bin_counts[i],0,R_vertex_cuts_up[i]+1);
	          HistoVertex_RadDet[i][j][k][2]=new TH2F(Form("HistoVertex_RadDet_v%d_p%d_k%d_2",i+1,j+1,k+1),Form(" %s from %s Area in %s MeV Range; z (cm); y (cm); (W/#muA)",spid[j].Data(),svertex_2D[i].Data(),ke_range[k].Data()),z_vertex_bin_counts[i],z_vertex_cuts_low[i]-1,z_vertex_cuts_up[i]+1,y_vertex_bin_counts[i],y_vertex_cuts_low[i]-1,y_vertex_cuts_up[i]+1);
          }
          else if (i==0){
            HistoVertex_RadDet[i][j][k][0]=new TH2F(Form("HistoVertex_RadDet_v%d_p%d_k%d_0",i+1,j+1,k+1),Form(" %s from %s Area in %s MeV Range; x (cm); y (cm); (W/#muA)",spid[j].Data(),svertex_2D[i].Data(),ke_range[k].Data()),x_vertex_bin_counts[i],Hall_x_vertices_low-1,Hall_x_vertices_up+1,y_vertex_bin_counts[i],Hall_y_vertices_low-1,Hall_y_vertices_up+1);
	          HistoVertex_RadDet[i][j][k][1]=new TH2F(Form("HistoVertex_RadDet_v%d_p%d_k%d_1",i+1,j+1,k+1),Form(" %s from %s Area in %s MeV Range; z (cm); R (cm); (W/#muA)",spid[j].Data(),svertex_2D[i].Data(),ke_range[k].Data()),z_vertex_bin_counts[i],Hall_z_vertices_low-1,Hall_z_vertices_up+1,R_vertex_bin_counts[i],0,Hall_R_vertices_up+1);
	          HistoVertex_RadDet[i][j][k][2]=new TH2F(Form("HistoVertex_RadDet_v%d_p%d_k%d_2",i+1,j+1,k+1),Form(" %s from %s Area in %s MeV Range; z (cm); y (cm); (W/#muA)",spid[j].Data(),svertex_2D[i].Data(),ke_range[k].Data()),z_vertex_bin_counts[i],Hall_z_vertices_low-1,Hall_z_vertices_up+1,y_vertex_bin_counts[i],Hall_y_vertices_low-1,Hall_y_vertices_up+1);
	        }
        }
      }
      //if (i==5)//change the title only for 2D histogram 
	    //svertex[5]="All";
      //2D radiation histo
      if (i<n_regions){
	      Histo_RadDet[i][j][0]=new TH2F(Form("Histo_RadDet_%d_%d_0",i+1,j+1),Form("Cyl. Det: %s from %s Area; #phi (Deg.); y (cm); (W/#muA)",spid[j].Data(),svertex_2D[i].Data()),90,-90,90,100,-800,800);//default bin sizes 360 and 400
	      Histo_RadDet[i][j][1]=new TH2F(Form("Histo_RadDet_%d_%d_1",i+1,j+1),Form("Top Disk. Det: %s from %s Area; z (cm); x (cm); (W/#muA)",spid[j].Data(),svertex_2D[i].Data()),100,-3000,3000,100,-3000,3000);//default bin sizes 300, 300
	      Histo_RadDet[i][j][2]=new TH2F(Form("Histo_RadDet_%d_%d_2",i+1,j+1),Form("Bottom Disk. Det: %s from %s Area; z (cm); x (cm); (W/#muA)",spid[j].Data(),svertex_2D[i].Data()),100,-3000,3000,100,-3000,3000);//default bin sizes 300, 300
	      Histo_RadDet[i][j][3]=new TH2F(Form("Histo_RadDet_%d_%d_3f",i+1,j+1),Form("Cyl. Det: %s from %s Area : Forward; x (cm); y (cm); (W/#muA)",spid[j].Data(),svertex_2D[i].Data()),260,-2600,2600,100,-800,800);//default bin sizes 520, 400
	      Histo_RadDet[i][j][4]=new TH2F(Form("Histo_RadDet_%d_%d_4b",i+1,j+1),Form("Cyl. Det: %s from %s Area : Backward; x (cm); y (cm); (W/#muA)",spid[j].Data(),svertex_2D[i].Data()),260,-2600,2600,100,-800,800);//default bin sizes 520, 400
      }
      //if (i==n_regions-1)
      //svertex[n_regions-1]="Other";
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
    Tmol->GetEntry(i);
    for (int j = 0; j<fNGenDetHit; j++){

      //for rate weighted simulations
      //if(fEvRate<0)
      //break;
      
      //for e-beam on target
      //record power and flux by the hall detectors only for pid==11,22,2112 particles
      // FIXME edit the detector definitions
      if((fGenDetHit_det[j]==SensVolume_v[0] || fGenDetHit_det[j]==SensVolume_v[1] || fGenDetHit_det[j]==SensVolume_v[2]) && (TMath::Abs(fGenDetHit_pid[j])==11 || fGenDetHit_pid[j]==22 || fGenDetHit_pid[j]==2112) ){//total into the hall
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
        for (Int_t vrt_i=0;vrt_i<n_regions+1;vrt_i++){
          //use only z vertex cut to select regions with x,y limiting vertices only coming from MOLLER apparatus otherwise there will be over counting due to hall enclosure 
          if (vrt_i != 0 && fGenDetHit_VZ[j]*100>=z_vertex_cuts_low[vrt_i] && fGenDetHit_VZ[j]*100<=z_vertex_cuts_up[vrt_i] && fGenDetHit_VX[j]*100>=x_vertex_cuts_low[vrt_i] && fGenDetHit_VX[j]*100<=x_vertex_cuts_up[vrt_i] && fGenDetHit_VY[j]*100>=y_vertex_cuts_low[vrt_i] && fGenDetHit_VY[j]*100<=y_vertex_cuts_up[vrt_i]){
            //if (fGenDetHit_VZ[j]*100>=z_vertex_cuts_low[vrt_i] && fGenDetHit_VZ[j]*100<=z_vertex_cuts_up[vrt_i]){
            vrtx=vrt_i;
//            break;
          }
          // FIXME // Include a outside x,y,z regions option to get a negative of the beamline area (hall region by negation)  
          else if (vrt_i == 0 && fGenDetHit_VZ[j]*100<=z_vertex_cuts_low[vrt_i] && fGenDetHit_VZ[j]*100>=z_vertex_cuts_up[vrt_i] && fGenDetHit_VX[j]*100<=x_vertex_cuts_low[vrt_i] && fGenDetHit_VX[j]*100>=x_vertex_cuts_up[vrt_i] && fGenDetHit_VY[j]*100<=y_vertex_cuts_low[vrt_i] && fGenDetHit_VY[j]*100>=y_vertex_cuts_up[vrt_i]){
            vrtx=vrt_i;
//            break;
          }
          else
            vrtx = -1;//vertex outside of the simulation region (non-physical)
//        } // Old end of for-loop, not it will add the event to any region's histgrams that may or may not overlap (before it was exclusive to just the first region that got it -> removed the 'breaks'). CSC 7/26/2017 EDIT

        if(vrtx>=0){
          kineE = TMath::Sqrt(TMath::Power(fGenDetHit_P[j]*1000,2) + TMath::Power(pidmass[(Int_t)TMath::Abs(fGenDetHit_pid[j])],2) ) - pidmass[(Int_t)TMath::Abs(fGenDetHit_pid[j])];
          for (Int_t ke_i=0;ke_i<n_energy_ranges;ke_i++){
            if (kineE > energy_cut_low[ke_i] && kineE <= energy_cut_up[ke_i]){
              keid=ke_i;
              break;
            } else
              keid=-1;	    
          }
          if (keid>=0 && hit_radius > hit_radius_min[vrtx_z]){
            flux_local[vrtx][detid][pid][keid]++;
            power_local[vrtx][detid][pid][keid]+=kineE;
            rho = TMath::Sqrt(TMath::Power(fGenDetHit_Z[j],2)+TMath::Power(fGenDetHit_X[j],2)); //z is in direction of beam, x is transverse, y is vertically upwards.
            phi = TMath::ASin(fGenDetHit_X[j]/rho)*180/TMath::Pi();
            // FIXME edit the histograms to be filled here.
            //following if is a redundant check I already checked  vrtx for negative values up
            Histo_kineE[vrtx][pid][keid]->Fill(kineE);
            Histo_vertex[vrtx][pid][keid]->Fill(fGenDetHit_VZ[j]*100,kineE/n_events);
            Histo_vertex_noWeight[vrtx][pid][keid]->Fill(fGenDetHit_VZ[j]*100,6.241e+12/n_events);
            if (vrtx<n_regions-1){//2D histograms are  available for target, collimators 1 through 5 (vrtx from 0 to n_regions-1)and vrtx=n_regions will be filled by all vertices
              if (detid==0){
                Histo_RadDet[vrtx][pid][0]->Fill(phi,fGenDetHit_Y[j]*100,kineE/n_events);//fill cyl. detector
                if (fGenDetHit_Z[j]>=0)
                   Histo_RadDet[vrtx][pid][3]->Fill(fGenDetHit_X[j]*100,fGenDetHit_Y[j]*100,kineE/n_events);//fill cyl. detector	forward
                else
                  Histo_RadDet[vrtx][pid][4]->Fill(fGenDetHit_X[j]*100,fGenDetHit_Y[j]*100,kineE/n_events);//fill cyl. detector	backward
                }	
              else if (detid==1 || detid==2)
                Histo_RadDet[vrtx][pid][detid]->Fill(fGenDetHit_Z[j]*100,fGenDetHit_X[j]*100,kineE/n_events);//fill cyl. detector

                //Fill vertex 2D plots
                HistoVertex_RadDet[vrtx][pid][keid][0]->Fill(fGenDetHit_VX[j]*100,fGenDetHit_VY[j]*100,kineE/n_events);
                HistoVertex_RadDet[vrtx][pid][keid][1]->Fill(fGenDetHit_VZ[j]*100,TMath::Sqrt(TMath::Power(fGenDetHit_VX[j]*100,2)+TMath::Power(fGenDetHit_VY[j]*100,2)),kineE/n_events);
                HistoVertex_RadDet[vrtx][pid][keid][2]->Fill(fGenDetHit_VZ[j]*100,fGenDetHit_VY[j]*100,kineE/n_events);
            }
 
          }else if (hit_radius > hit_radius_min[vrtx_z])//without this condition warning will print for tracks going to the dump
            printf("warning: energy outside the ranges %4.3f \n",kineE);
        }// end for loop, run once per event per region it appears in
         //Run once per event
        if (keid>=0 && hit_radius > hit_radius_min[vrtx_z]){
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
        }
      }

        //else
        //printf("warning: vertex outside the ranges z = %4.3f cm \n",fGenDetHit_VZ[j]*100);
	      //now repeat above set for three hall detectors to get totals for each detector
      }
	

      
    }
    
    if (i>200000  && earlybreak)
      break;
    if (i%500000==0)
      printf("Event %d of %d \n",i,nentries);


  }
  
// FIXME edit these loops  
  //vertex based histograms
  if (kShow1DKEPlots){
    TCanvas * canvas_ke[n_regions+1][n_particles];
    for(Int_t i=0;i<n_regions+1;i++){//vtx
      for(Int_t j=0;j<n_particles;j++){//pid
	      canvas_ke[i][j]= new TCanvas(Form("canvas_ke_vrtx%d_pid%d",i+1,j+1),Form("canvas_ke_vrtx%d_pid%d",i+1,j+1),1500,500);
	      canvas_ke[i][j]->Divide(n_energy_ranges,1);  //FIXME check the splitting based on energy ranges or not.
	      for(Int_t k=0;k<n_energy_ranges;k++){//KE
	        canvas_ke[i][j]->cd(k+1);
	        if (k>1){
	          canvas_ke[i][j]->cd(k+1)->SetLogy();
          }
          Histo_kineE[i][j][k]->Draw();
	      }
	      if (kSave1DKEHisto){
	        canvas_ke[i][j]->SaveAs(plotsFolder+Form("canvas_ke_vrtx%d_pid%d.png",i+1,j+1));
        }
        if (kSaveRootFile){
	        canvas_ke[i][j]->Write();
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
        canvas_vertx[i][j]= new TCanvas(Form("canvas_vertx_vrtx%d_pid%d",i+1,j+1),Form("canvas_vertx_vrtx%d_pid%d",i+1,j+1),1500,500);	
        canvas_vertx[i][j]->Divide(n_energy_ranges,1);
        canvas_vertx_noWeight[i][j]= new TCanvas(Form("canvas_vertx_noWeight_vrtx%d_pid%d",i+1,j+1),Form("canvas_vertx_noWeight_vrtx%d_pid%d",i+1,j+1),1500,500);	
        canvas_vertx_noWeight[i][j]->Divide(n_energy_ranges,1);

        canvas_vertx_integral[i][j]= new TCanvas(Form("canvas_vertx_integral_vrtx%d_pid%d",i+1,j+1),Form("canvas_vertx_integral_vrtx%d_pid%d",i+1,j+1),1500,500);	
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
          canvas_vertx[i][j]->SaveAs(plotsFolder+Form("canvas_vrtx_vrtx%d_pid%d.png",i+1,j+1));
          canvas_vertx_noWeight[i][j]->SaveAs(plotsFolder+Form("canvas_vrtx_noWeight_vrtx%d_pid%d.png",i+1,j+1));
          canvas_vertx_integral[i][j]->SaveAs(plotsFolder+Form("canvas_vrtx_integral_vrtx%d_pid%d.png",i+1,j+1));
        }
        if (kSaveRootFile){
          canvas_vertx[i][j]->Write();
          canvas_vertx_noWeight[i][j]->Write();
          canvas_vertx_integral[i][j]->Write();
        }
      }
    }
  }

  //2D radiation distr on cylinder and disk detectors
  if (kShow2Dhits){
    TCanvas * canvas_hallrad[n_regions];
    for(Int_t i=0;i<n_regions;i++){//vertex // THESE ARE THE TOP Heatmaps vs. vertex region
      canvas_hallrad[i] = new TCanvas(Form("canvas_hallrad_vrtx%d",i+1),Form("canvas_hallrad_vrtx%d",i+1),1500,1500);
      canvas_hallrad[i]->Divide(n_particles,n_energy_ranges);
      for(Int_t j=0;j<n_particles;j++){//pid
      	for(Int_t k=0;k<n_energy_ranges;k++){//detid
	        canvas_hallrad[i]->cd(n_energy_ranges*j+1+k);
	        canvas_hallrad[i]->cd(n_energy_ranges*j+1+k)->SetLogz();
	        Histo_RadDet[i][j][k]->Draw("colz");	
	        Histo_RadDet[i][j][k]->SetStats(0);
	      }
      }
      if (kSave2DHisto)
	      canvas_hallrad[i]->SaveAs(plotsFolder+Form("canvas_hallrad_vrtx%d.png",i+1));
      if (kSaveRootFile)
	      canvas_hallrad[i]->Write();
    }

    TCanvas * canvas_hallrad_cyc_xy[n_regions];
    for(Int_t i=0;i<n_regions;i++){//vertex
      canvas_hallrad_cyc_xy[i] = new TCanvas(Form("canvas_hallrad_cyc_xy_vrtx%d",i+1),Form("canvas_hallrad_cyc_xy_vrtx%d",i+1),1500,1500);
      canvas_hallrad_cyc_xy[i]->Divide(2,n_particles);
      for(Int_t j=0;j<n_particles;j++){//pid
	      for(Int_t k=0;k<2;k++){//detid
	        canvas_hallrad_cyc_xy[i]->cd(2*j+1+k);
	        canvas_hallrad_cyc_xy[i]->cd(2*j+1+k)->SetLogz();
	        Histo_RadDet[i][j][n_particles+k]->Draw("colz");	
	        Histo_RadDet[i][j][n_particles+k]->SetStats(0);
      	}
      }
      if (kSave2DHisto)
	      canvas_hallrad_cyc_xy[i]->SaveAs(plotsFolder+Form("canvas_hallrad_cyc_xy_vrtx%d.png",i+1));
      if (kSaveRootFile)
	      canvas_hallrad_cyc_xy[i]->Write();
    }
  }

  //2D radiation vertex distr 
  if (kShow2DVertex){
    TCanvas * canvas_hallrad_xy_vrtx[n_regions];
    TCanvas * canvas_hallrad_rz_vrtx[n_regions];
    TCanvas * canvas_hallrad_yz_vrtx[n_regions];
    for(Int_t i=0;i<n_regions;i++){//vertex
      canvas_hallrad_xy_vrtx[i]=new TCanvas(Form("canvas_hallrad_xy_vrtx%d",i+1),Form("canvas_hallrad_xy_vrtx%d",i+1),1500,1500);
      canvas_hallrad_rz_vrtx[i]=new TCanvas(Form("canvas_hallrad_rz_vrtx%d",i+1),Form("canvas_hallrad_rz_vrtx%d",i+1),1500,1500);
      canvas_hallrad_yz_vrtx[i]=new TCanvas(Form("canvas_hallrad_yz_vrtx%d",i+1),Form("canvas_hallrad_yz_vrtx%d",i+1),1500,1500);
      canvas_hallrad_xy_vrtx[i]->Divide(n_particles,n_energy_ranges); // FIXME check the divide ordering energy vs particle axes
      canvas_hallrad_rz_vrtx[i]->Divide(n_particles,n_energy_ranges);
      canvas_hallrad_yz_vrtx[i]->Divide(n_particles,n_energy_ranges);
      for(Int_t j=0;j<n_particles;j++){//pid
        for(Int_t k=0;k<3;k++){//detid                                // FIXME change the detector regions investigated
          canvas_hallrad_xy_vrtx[i]->cd(3*j+1+k);
          canvas_hallrad_xy_vrtx[i]->cd(3*j+1+k)->SetLogz();
          HistoVertex_RadDet[i][j][k][0]->Draw("colz");// THESE ARE THE vertices of particle birth plots	
          HistoVertex_RadDet[i][j][k][0]->SetStats(0);

          canvas_hallrad_rz_vrtx[i]->cd(3*j+1+k);                     // FIXME these 3's refer to the number of detectors present
          canvas_hallrad_rz_vrtx[i]->cd(3*j+1+k)->SetLogz();
          HistoVertex_RadDet[i][j][k][1]->Draw("colz");	
          HistoVertex_RadDet[i][j][k][1]->SetStats(0);
          
          canvas_hallrad_yz_vrtx[i]->cd(3*j+1+k);                     // FIXME these 3's refer to the number of detectors present
          canvas_hallrad_yz_vrtx[i]->cd(3*j+1+k)->SetLogz();
          HistoVertex_RadDet[i][j][k][2]->Draw("colz");	
          HistoVertex_RadDet[i][j][k][2]->SetStats(0);
        }
      }
      if (kSave2DVertexHisto){
	      canvas_hallrad_xy_vrtx[i]->SaveAs(plotsFolder+Form("canvas_hallrad_xy_vrtx%d.png",i+1));
	      canvas_hallrad_rz_vrtx[i]->SaveAs(plotsFolder+Form("canvas_hallrad_rz_vrtx%d.png",i+1));
	      canvas_hallrad_yz_vrtx[i]->SaveAs(plotsFolder+Form("canvas_hallrad_yz_vrtx%d.png",i+1));
      }
      if (kSaveRootFile){
      	canvas_hallrad_xy_vrtx[i]->Write();
      	canvas_hallrad_rz_vrtx[i]->Write();
      	canvas_hallrad_yz_vrtx[i]->Write();
      }
    }
  }
  //end of plots for hall detector

  Char_t * detector[4]                = {"Total","Side","Top","Bottom"};
  Char_t * chpid[n_particles]         = {"abs(electrons)","Photons","Neutrons"};
  Char_t * chenrange[n_energy_ranges] = {"E<10","10<E<100","100<E"};

  TList * list_power = new TList;
  TString strline;
  char line[400];
  char line1[400];
  strline="Rootfile name";
  list_power->Add(new TObjString(strline));
  list_outputs << line << endl;
  strline=added_file;//"/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/";//main root files used//_allshield_BorConcrete
  list_power->Add(new TObjString(strline));
  list_outputs << line << endl;
  strline="Total Radiation Power to the hall (W/uA)";
  list_power->Add(new TObjString(strline));
  list_outputs << line << endl;
  printf(" \n Total Radiation Power to the hall (W/uA) \n");
  printf(" %20s %20s %13s %13s %13s \n","Type","E Rnge (MeV)",detector[1],detector[2],detector[3]);
  sprintf(line," %20s %20s %13s %13s %13s ","Type","E Rnge (MeV)",detector[1],detector[2],detector[3]);
  list_power->Add(new TObjString(line));
  list_outputs << line << endl;
  Double_t sum=0;

  for(Int_t i=0;i<n_particles;i++){//pid
    for(Int_t j=0;j<n_energy_ranges;j++){//energy range
      printf(" %20s %20s",chpid[i],chenrange[j]);
      sprintf(line," %20s %20s",chpid[i],chenrange[j]);
      sprintf(line1," ");//empty previous values
      for(Int_t k=0;k<3;k++){//detector                   // FIXME number of detectors present
      	for (Int_t s=0;s<n_regions;s++)
	         sum+=power_local[s][k][i][j];//sum over all the vertices
      	printf("%12.3E",sum/n_events);
	      sprintf(line1,"%s %12.3E",line1,sum/n_events);	
	      sum=0;
      }
      printf("\n");
      sprintf(line," %s %s",line,line1);
      list_power->Add(new TObjString(line));
      list_outputs << line << endl;
    }
  }

  printf(" \n Vertex Cut : Radiation Power to the hall (W/uA) \n");
  strline="Vertex Cut : Radiation Power to the hall (W/uA)";
  list_power->Add(new TObjString(strline));
  list_outputs << line << endl;
  printf(" %20s %20s %20s %13s %13s %13s \n","Vertex","Type","E Rnge (MeV)",detector[1],detector[2],detector[3]);
  sprintf(line," %20s %20s %20s %13s %13s %13s ","Vertex","Type","E Rnge (MeV)",detector[1],detector[2],detector[3]);
  list_power->Add(new TObjString(line));
  list_outputs << line << endl;
  for (Int_t i=0;i<n_regions;i++){
    for(Int_t j=0;j<n_particles;j++){//pid
      for(Int_t k=0;k<n_energy_ranges;k++){//energy range
	      printf(" %20s %20s %20s",svertex[i].Data(),chpid[j],chenrange[k]);
	      sprintf(line," %20s %30s %20s",svertex[i].Data(),chpid[j],chenrange[k]);
	      sprintf(line1," ");//empty previous values
	      for(Int_t l=0;l<3;l++){//detector                             // FIXME number of detectors present
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
  

  sum=0;
  TList * list_flux = new TList;
  printf(" \n Total Radiation Flux to the hall (Hz/uA)\n");
  strline="Total Radiation Flux to the hall (Hz/uA)";
  list_flux->Add(new TObjString(strline));
  list_outputs << line << endl;
  printf(" %20s %20s %13s %13s %13s \n","Type","E Rnge (MeV)",detector[1],detector[2],detector[3]);
  sprintf(line," %20s %20s %13s %13s %13s ","Type","E Rnge (MeV)",detector[1],detector[2],detector[3]);
  list_flux->Add(new TObjString(line));
  list_outputs << line << endl;
  for(Int_t i=0;i<n_particles;i++){//pid
    for(Int_t j=0;j<n_energy_ranges;j++){//energy range
      printf(" %20s %20s",chpid[i],chenrange[j]);
      sprintf(line," %20s %20s",chpid[i],chenrange[j]);
      sprintf(line1," ");//empty previous values
      for(Int_t k=0;k<3;k++){//detector                             // FIXME number of detectors present
	      for (Int_t s=0;s<n_regions;s++)
	        sum+=flux_local[s][k][i][j];//sum over all the vertices
	      printf("%12.3E",sum*6.241e+12/n_events);
	      sprintf(line1,"%s %12.3E",line1,sum*6.241e+12/n_events);
	      sum=0;
      }
      printf("\n");
      sprintf(line," %s %s",line,line1);
      list_flux->Add(new TObjString(line));
      list_outputs << line << endl;
    }
  }

  printf(" \n Vertex Cut : Radiation Flux to the hall (Hz/uA) \n");
  strline="Vertex Cut : Radiation Flux to the hall (Hz/uA)";
  list_flux->Add(new TObjString(strline));
  list_outputs << line << endl;
  printf(" %20s %20s %20s %13s %13s %13s \n","Vertex","Type","E Rnge (MeV)",detector[1],detector[2],detector[3]);
  sprintf(line," %20s %20s %20s %13s %13s %13s ","Vertex","Type","E Rnge (MeV)",detector[1],detector[2],detector[3]);
  list_flux->Add(new TObjString(line));
  list_outputs << line << endl;
  for (Int_t i=0;i<n_regions;i++){
    for(Int_t j=0;j<n_particles;j++){//pid
      for(Int_t k=0;k<n_energy_ranges;k++){//energy range
        printf(" %20s %20s %20s",svertex[i].Data(),chpid[j],chenrange[k]);
        sprintf(line," %20s %30s %20s",svertex[i].Data(),chpid[j],chenrange[k]);
        sprintf(line1," ");//empty previous values
        for(Int_t l=0;l<3;l++){//detector                             // FIXME number of detectors present
          printf("%12.3E",flux_local[i][l][j][k]*6.241e+12/n_events);
          sprintf(line1,"%s %12.3E",line1,flux_local[i][l][j][k]*6.241e+12/n_events);
        }
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
    // FIXME what are these?
    Double_t stops[NRGBs] = { 0.00, 0.34, 0.61, 0.84, 1.00 };
    Double_t red[NRGBs]   = { 0.00, 0.00, 0.87, 1.00, 0.51 };
    Double_t green[NRGBs] = { 0.00, 0.81, 1.00, 0.20, 0.00 };
    Double_t blue[NRGBs]  = { 0.51, 1.00, 0.12, 0.00, 0.00 };
    TColor::CreateGradientColorTable(NRGBs, stops, red, green, blue, NCont);
    gStyle->SetNumberContours(NCont);
}
