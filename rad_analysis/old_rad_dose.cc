/*
Rakitha Mon Aug 25 14:48:01 EDT 2014
Plot generated related to the radiation in the hall

Radiation plots
KE intercepted by the cylindrical and two disk detectors
vertex distribution on the cylindrical and two disk detectors

Vertex cuts are based on the shielding blocks


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
Double_t fGenDetHit_edep[__IO_MAXHIT];
//Double_t fGenDetHit_T[__IO_MAXHIT];

//input info
Int_t n_events=1e7;
Int_t beamcurrent = 85;//uA

Int_t hall_det_cyl = 99;//cylindircal det at the radius close to hall wall 
Int_t hall_det_top = 101;//cylindircal det at the top of the hall
Int_t hall_det_bottom = 103;//cylindircal det at the bottom of the hall

Int_t SensVolume_v[3] = {99,101,103};//look at everything going out to the hall


////Flux and power parameters Full range //indices [detector][energy range] there is 3 energy ranges for e,gamma, n for three hall detectors. The last index is for the shieldings: target, shielding blocks 1 to 4, and other vertices
Double_t flux_local[6][3][3][3]={{{{0}}}};

Double_t power_local[6][3][3][3]={{{{0}}}};

std::map<int,int> detectormap;

std::map<int,int> pidmap;

std::map<int,double> pidmass;

Double_t hit_radius_min[2] = {0.46038,0.46038}; //m inner radius of the beam pipe 45.72 cm and outer radius of the beam pipe 46.038 cm and radius of the detector plane is 1.9 m
Double_t hit_radius;
Double_t kineE;

//boolean switches

Bool_t earlybreak=kFALSE;//kFALSE;//kTRUE;//kFALSE;//exit early from the main tree entries loop

Bool_t kSaveRootFile=kTRUE;//kTRUE;//kTRUE;//save histograms and canvases into a rootfile
Bool_t kShowGraphic=kFALSE;//kTRUE;//Show canvases and they will be saved the rootfile. Set this to false to show no graphics but save them to rootfile

//Boolean parameter to disable/enable saving histograms as png
Bool_t kSave1DKEHisto=kTRUE;//kTRUE;//kFALSE;//option to save histograms
Bool_t kSave1DVrtxHisto=kTRUE;//kTRUE;//kFALSE;//option to save histograms

Bool_t kSave2DHisto=kTRUE;//kTRUE;//kFALSE;//option to save 2D hit distribution histograms
Bool_t kSave2DVertexHisto=kTRUE;//option to save 2D vertex distribution

Bool_t kShow1DKEPlots=kTRUE;//disabling the show the canvases will not be shown or written to the root files
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
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoll_Coll_Test4.root");
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoll_Coll_Test2.root");
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1.root");
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_moller_1.root");
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_2.root");//col ids are not in order, missing col 2005 in simulation
  //generic hit (for sens detectors)
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_3.root");//col ids are not in order, missing col 2005 in simulation
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_new_1.root");
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_new_2.root");

  //target shielding
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_tgtshieldConcrete_e_beam_1M.root");
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_tgtshieldBorConcrete_e_beam_1M.root");
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_tgtshieldW_e_beam_1M.root");

  //all shielding
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_allshieldBorConcrete_e_beam_1M.root");
  //fixed col-3 and make first shielding block lead
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_allshieldBorConcrete_e_beam_1M_update_1.root");
  //added Tgt and DS shielding hutd and hall A concrete wall, ceiling and floor, bottom disk detector is not available now
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_col3US40_kryptoDet_concreteHall_shldHuts_1.root");
  //Added tgt and US Poly shielding at bulk covering whole shield blocks all concrete are std. concrete
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_col3US40_kryptoDet_concreteHall_shldHuts_withPoly_noBoron_1.root");
  //Added tgt and US Poly shielding at bulk covering whole shield blocks all concrete are std. concrete 
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_col3US40_kryptoDet_concreteHall_shldHuts_withPoly_noBoron_2M_*.root");
  //Added US poly bottom shield
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_col3US40_kryptoDet_concreteHall_shldHuts_withPolyUpdated_noBoron_1.root");
  //Poly shielding are now kryptonite to check the leakage
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_col3US40_kryptoDet_concreteHall_shldHuts_withPolyKrypto_noBoron_1.root");
  //Poly shielding are now kryptonite and TGT and sh-2/3  are HD_Concrete to check the leakage
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_col3US40_kryptoDet_concreteHall_shldHuts_withPolyKrypto_HDConcrete_1.root");
  //added Poly shielding to DS shld4 and shielding hut
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_shldHuts_withPoly_2.root");
  //added Poly shielding to DS shld4 and shielding hut changed tgt hut and shld blocks 2,3 and 4 are tungsten
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_shldHutsTungsten_withPoly_1.root");
  //added Poly shielding to DS shld4 and shielding hut set to kryptonite
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_shldHuts_withPolyKrypto_1.root");
  //added Poly shielding to DS shld4 and shielding hut set to kryptonite shld blocks 2,3 and 4 are tungsten
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_shldHutsTungsten_withPolyKrypto_1.root");
  //merged col1 design
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_mergedcol1.root");
  //merged col1 design with same Rout as before
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_mergedcol1_oldRout.root");
  //merged col1 design tungsten
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_mergedcol1_design1.root");
  //merged col1 design tungsten, no col-3 and shld-2 and 3 are tungsten
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_mergedcol1_design1_check1.root");
  //merged col1 design tungsten, no col-3 and tgt shield, shld-2 and 3 are tungsten. large bore at tgt shielding and shld-1
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_mergedcol1_design1_check2.root");
  //merged col1 design tungsten, no col-3 and tgt shield, shld-2 and 3 are tungsten. large bore at tgt shielding and shld-block1 is removed . small bore at shld-2 face//
  //Increased the height of shld block 2 to match height of the shld block 3
  //more stat runs
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_mergedcol1_design1_check3_*.root");
  //switched back to concrete for tgt and shld block 2 and 3
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_mergedcol1_design1_check4.root");
  //design1_check3 with copper shielding and collimators (1 and 2)
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_1M_mergedcol1_design1_check3_copper.root");
  //testing beampipe
  //Tmol->Add("/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_100k_mergedcol1_design1_beampipe_test.root");
  
  //Final_1
  //fins are updated to match engineering CAD
  //Tmol->Add("/home/rakithab/DataCenter/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_concrete_shield_W_cols_1M_1.root");
  //fins are updated to match engineering CAD and materials changed to CW95
  //Tmol->Add("/home/rakithab/DataCenter/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_concrete_shield_CW95_cols_1M_1.root");
  //fins are updated to match engineering CAD and materials changed to CW95 and lead colar US of col-1
  //Tmol->Add("/home/rakithab/DataCenter/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_concrete_shield_CW95_cols_Lead_col_1M_1.root");
  //fins are updated to match engineering CAD and materials changed to CW95 and lead colar US of col-1 more stats from ifarm
  //Tmol->Add("/home/rakithab/DataCenter/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_final1_1M_*.root");
  ////fins are updated to match engineering CAD and materials changed to CW95 and lead colar US , tungsten target shielding, extended DS shielding beam pipe
  //Tmol->Add("/home/rakithab/DataCenter/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_tgt_tungsten_shield_1M_1.root");
  ////fins are updated to match engineering CAD and materials changed to CW95 and lead colar US , lead target shielding, extended DS shielding beam pipe
  //Tmol->Add("/home/rakithab/DataCenter/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_tgt_lead_shield_1M_1.root");
  ////fins are updated to match engineering CAD and materials changed to CW95 and lead colar US , lead target shielding, thick poly shield around tgt and col-1 extended DS shielding beam pipe
  //Tmol->Add("/home/rakithab/DataCenter/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/remoller_e_beam_tgt_lead_shield_thickPoly_1M_1.root");
  ////fins are updated to match engineering CAD and materials changed to CW95 and lead colar US , lead target shielding, thick poly shield around tgt and col-1 extended DS shielding beam pipe, 10 million generated in ifarm
//
//
  //Tmol->Add("/home/rakithab/DataCenter/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/ifarm_e_beam_tgt_lead_shield_thickPoly_1M_*.root");
  
  //Cameron Clarke runs:
  
  //TString added_file="/home/cameronc/gitdir/dose_remoll/output/check3_500000_1.root";
  //TString added_file="/home/cameronc/gitdir/dose_remoll/output/check3_1000000_1.root";
  TString added_file="/home/cameronc/gitdir/dose_remoll/output/beam_tracking_10M.root";
  //TString added_file="/home/cameronc/gitdir/dose_remoll/output/check3_1000000_3.root";
  Tmol->Add(added_file);



  //rootfilename="/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Plots/rootfiles/remoller_e_beam_tgt_lead_shield_thickPoly_10M_ShieldPlots.root";//remoller_e_beam_1M_baresetup_cutDetPlane.root
  //TString rootfilename="/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Plots/rootfiles/remoller_e_beam_concrete_shield_CW95_cols_Lead_col_1M_ShieldPlots.root";//name of the rootfile to save generated histograms
  //TString rootfilename="/home/cameronc/gitdir/dose_remoll/output/Plots_check3/check3_plots_1M.root";//name of the rootfile to save generated histograms
  //TString rootfilename="/home/cameronc/gitdir/dose_remoll/output/Plots_less_hybshld/less_hybshld_plots_1M.root";//name of the rootfile to save generated histograms
  TString rootfilename="/home/cameronc/gitdir/dose_remoll/output/Plots_tracking_10M/tracking_10M_plots.root";//name of the rootfile to save generated histograms
  //TString rootfilename="/home/cameronc/gitdir/dose_remoll/output/Plots_check3_3/check3_plots_1M_3.root";//name of the rootfile to save generated histograms
  
  
  //TString plotsFolder="/home/cameronc/gitdir/dose_remoll/output/Plots_check3/";//Name of folder for saving plots
  //TString plotsFolder="/home/cameronc/gitdir/dose_remoll/output/Plots_less_hybshld/";//Name of folder for saving plots
  TString plotsFolder="/home/cameronc/gitdir/dose_remoll/output/Plots_tracking_10M/";//Name of folder for saving plots
  //TString plotsFolder="/home/cameronc/gitdir/dose_remoll/output/Plots_check3_3/";//Name of folder for saving plots
  TString outputFolder=added_file;//Name of file for data

  //list_outputs.open("/home/cameronc/gitdir/dose_remoll/output/Plots_less_hybshld/list_outputs_less_hybshld.txt");
  list_outputs.open("/home/cameronc/gitdir/dose_remoll/output/Plots_tracking_10M/list_outputs_tracking_10M.txt");
  //list_outputs.open("/home/cameronc/gitdir/dose_remoll/output/Plots_check3_3/list_outputs_check3_3.txt");

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
  Tmol->SetBranchAddress("hit.vx",&fGenDetHit_VX);
  Tmol->SetBranchAddress("hit.vy",&fGenDetHit_VY);

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
  detectormap[99]=0;
  detectormap[101]=1;
  detectormap[103]=2;
  //indices asigned to pid numbers
  pidmap[11]=0; //electron 
  pidmap[22]=1; //photon
  pidmap[2112]=2; //neutron

  pidmass[11]=0.511;//MeV
  pidmass[22]=0.0;
  pidmass[2112]=939.565;//MeV




  //hall radiation from all source, target, collimators separated for ep,gamma, and n for three energy ranges
  TH1F *Histo_kineE[7][3][3];
  TH1F *Histo_vertex[6][3][3];
  TH1F *Histo_vertex_noWeight[6][3][3];


  //hall radiation 2D hit distribution  for the cylinder using y vs phi and using x vs. y for disks
  /*
    When using cylindrical coordinates, the hit distr from z>0 and z<0 are projected into the y vs phi plane where phi goes from -90 to 90
   */
  TH2F *Histo_RadDet[6][3][5];//6 vertex ranges for three particles species for three detectors (0-cylinder phi-y, 1 and 2 are top and bottom disk detectors, 3,4 is cylinder x-y forward backward)

  TH2F *HistoVertex_RadDet[6][3][3][2];//6 vertex ranges for three particles species for three energy ranges for y:x, and r:z //I may extend to add x:z, and y:z vertex distributions

  Double_t z_vertex_cuts_low[6] = {-236.5,275.1,483.,590.,852.,-600.};//last index store vertices outside of other ranges 
  Double_t z_vertex_cuts_up[6] = {236,295.1,589.99,690.,952.,1700.};//the gap between shld-2 and shld-3 are closed now so changing 483 to 583 to 483 to 589.99
  Double_t x_vertex_cuts_low[6] = {-296.,-35.,-113.5,-133.,-224.6,-500.};
  Double_t x_vertex_cuts_up[6] = {296.,35.,113.5,133.,346.8,500.};
  Double_t y_vertex_cuts_low[6] = {-250.,-35.,-113.5,-133.,-250.,-500.};
  Double_t y_vertex_cuts_up[6] = {250.,35.,113.5,133.,250.,500.};
  Double_t R_vertex_cuts_up[6] =  {300. , 50.0, 160.0, 180.0, 450., 300.};

  Double_t energy_cut_low[3]={0,10,100};
  Double_t energy_cut_up[3]={10,100,12000};
  Int_t bin_ranges[7][3][4]={{{0,10,100,12000},{0,10,100,4000},{0,10,100,4000}},//target : (elec,gamma,neutron)
			     {{0,10,100,500},{0,10,100,500},{0,10,100,500}},//Shielding Block 1
			     {{0,10,100,1000},{0,10,100,1000},{0,10,100,1000}},//Shielding Block 2
			     {{0,10,100,500},{0,10,100,500},{0,10,100,500}},//Shielding Block 3
			     {{0,10,100,500},{0,10,100,500},{0,10,100,500}},//Shielding Block 4
			     {{0,10,100,12000},{0,10,100,4000},{0,10,100,4000}},//other
			     {{0,10,100,12000},{0,10,100,4000},{0,10,100,4000}}//total
  };
  Int_t vertex_bin_ranges_low[6] = {-236,275,482,589,851,-600};
  Int_t vertex_bin_ranges_up[6] = {236,296,584,691,953,1600};
  TString ke_range[3] = {"KE<10","10<KE<100","100<KE"};
  TString spid[3]={"e-","#gamma","n0"};
  TString svertex[7]={"ShTarget","ShBlock-1","ShBlock-2","ShBlock-3","ShBlock-4","Other","All"};//last index stores hits not within tgt or collimators
  TString svertex_2D[6]={"ShTarget","ShBlock-1","ShBlock-2","ShBlock-3","ShBlock-4","All"};//last index stores hits not within tgt or collimators

  
  for(Int_t i=0;i<7;i++){//vertices
    for(Int_t j=0;j<3;j++){//pid
      for(Int_t k=0;k<3;k++){//KE
	//1D radiation histograms
	Histo_kineE[i][j][k]=new TH1F(Form("Histo_kineE_%d_%d_%d",i+1,j+1,k+1),Form("%s from %s Area in %s MeV Range; KineE (MeV)",spid[j].Data(),svertex[i].Data(),ke_range[k].Data()),100,bin_ranges[i][j][k],bin_ranges[i][j][k+1]);
	if (i<6){
	Histo_vertex[i][j][k]=new TH1F(Form("Histo_vertex_%d_%d_%d",i+1,j+1,k+1),Form("%s Vertices from %s Area in %s MeV Range (KE weighted);Z vertex (cm);W/#muA",spid[j].Data(),svertex[i].Data(),ke_range[k].Data()),500,vertex_bin_ranges_low[i],vertex_bin_ranges_up[i]);
	Histo_vertex_noWeight[i][j][k]=new TH1F(Form("Histo_vertex_noWeight_%d_%d_%d",i+1,j+1,k+1),Form("%s Vertices from %s Area in %s MeV Range ;Z vertex (cm);Hz/#muA",spid[j].Data(),svertex[i].Data(),ke_range[k].Data()),500,vertex_bin_ranges_low[i],vertex_bin_ranges_up[i]);

	//2D vertex distribution histograms
	HistoVertex_RadDet[i][j][k][0]=new TH2F(Form("HistoVertex_RadDet_v%d_p%d_k%d_0",i+1,j+1,k+1),Form(" %s from %s Area in %s MeV Range; x (cm); y (cm); (W/#muA)",spid[j].Data(),svertex_2D[i].Data(),ke_range[k].Data()),200,x_vertex_cuts_low[i],x_vertex_cuts_up[i],400,y_vertex_cuts_low[i],y_vertex_cuts_up[i]);
	HistoVertex_RadDet[i][j][k][1]=new TH2F(Form("HistoVertex_RadDet_v%d_p%d_k%d_1",i+1,j+1,k+1),Form(" %s from %s Area in %s MeV Range; z (cm); R (cm); (W/#muA)",spid[j].Data(),svertex_2D[i].Data(),ke_range[k].Data()),200,z_vertex_cuts_low[i],z_vertex_cuts_up[i],400,0,R_vertex_cuts_up[i]);
	}
      }
      //if (i==5)//change the title only for 2D histogram 
	  //svertex[5]="All";
      //2D radiation histo
      if (i<6){
	Histo_RadDet[i][j][0]=new TH2F(Form("Histo_RadDet_%d_%d_0",i+1,j+1),Form("Cyl. Det: %s from %s Area; #phi (Deg.); y (cm); (W/#muA)",spid[j].Data(),svertex_2D[i].Data()),90,-90,90,100,-800,800);//default bin sizes 360 and 400
	Histo_RadDet[i][j][1]=new TH2F(Form("Histo_RadDet_%d_%d_1",i+1,j+1),Form("Top Disk. Det: %s from %s Area; z (cm); x (cm); (W/#muA)",spid[j].Data(),svertex_2D[i].Data()),100,-3000,3000,100,-3000,3000);//default bin sizes 300, 300
	Histo_RadDet[i][j][2]=new TH2F(Form("Histo_RadDet_%d_%d_2",i+1,j+1),Form("Bottom Disk. Det: %s from %s Area; z (cm); x (cm); (W/#muA)",spid[j].Data(),svertex_2D[i].Data()),100,-3000,3000,100,-3000,3000);//default bin sizes 300, 300
	Histo_RadDet[i][j][3]=new TH2F(Form("Histo_RadDet_%d_%d_3f",i+1,j+1),Form("Cyl. Det: %s from %s Area : Forward; x (cm); y (cm); (W/#muA)",spid[j].Data(),svertex_2D[i].Data()),260,-2600,2600,100,-800,800);//default bin sizes 520, 400
	Histo_RadDet[i][j][4]=new TH2F(Form("Histo_RadDet_%d_%d_4b",i+1,j+1),Form("Cyl. Det: %s from %s Area : Backward; x (cm); y (cm); (W/#muA)",spid[j].Data(),svertex_2D[i].Data()),260,-2600,2600,100,-800,800);//default bin sizes 520, 400
      }
      //if (i==5)
      //svertex[5]="Other";

    }
  }


 
  

  int detid=-1;
  int pid = -1;
  Int_t vrtx = -1;//index for vertex range
  Int_t keid = -1;
  Int_t vrtx_z=-1;

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
	for (Int_t vrt_i=0;vrt_i<7;vrt_i++){
	  //use only z vertex cut to select regions with x,y limiting vertices only coming from MOLLER apparatus otherwise there will be over counting due to hall enclosure 
	  if (fGenDetHit_VZ[j]*100>=z_vertex_cuts_low[vrt_i] && fGenDetHit_VZ[j]*100<=z_vertex_cuts_up[vrt_i] && fGenDetHit_VX[j]*100>=x_vertex_cuts_low[vrt_i] && fGenDetHit_VX[j]*100<=x_vertex_cuts_up[vrt_i] && fGenDetHit_VY[j]*100>=y_vertex_cuts_low[vrt_i] && fGenDetHit_VY[j]*100<=y_vertex_cuts_up[vrt_i]){
	  //if (fGenDetHit_VZ[j]*100>=z_vertex_cuts_low[vrt_i] && fGenDetHit_VZ[j]*100<=z_vertex_cuts_up[vrt_i]){
	    vrtx=vrt_i;
	    break;
	  }else
	    vrtx = -1;//vertex outside of the simulation region (non-physical)
	}

	if(vrtx>=0){
	  kineE = TMath::Sqrt(TMath::Power(fGenDetHit_P[j]*1000,2) + TMath::Power(pidmass[(Int_t)TMath::Abs(fGenDetHit_pid[j])],2) ) - pidmass[(Int_t)TMath::Abs(fGenDetHit_pid[j])];
	  for (Int_t ke_i=0;ke_i<3;ke_i++){
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
	    //following if is a redundant check I already checked  vrtx for negative values up
	    Histo_kineE[vrtx][pid][keid]->Fill(kineE);
	    Histo_kineE[6][pid][keid]->Fill(kineE);
	    Histo_vertex[vrtx][pid][keid]->Fill(fGenDetHit_VZ[j]*100,kineE/n_events);
	    Histo_vertex_noWeight[vrtx][pid][keid]->Fill(fGenDetHit_VZ[j]*100,6.241e+12/n_events);
	    if (vrtx<5){//2D histograms are  available for target, col 1 to 5 (vrtx from 0 to 5)and vrtx=6 will be filled by all vertices
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
	    }
	    if (detid==0){
	      Histo_RadDet[5][pid][0]->Fill(phi,fGenDetHit_Y[j]*100,kineE/n_events);//fill cyl. detector //index 5 will fill all the vertices (vrtx from 0 to 6)
	      if (fGenDetHit_Z[j]>=0)		
		Histo_RadDet[5][pid][3]->Fill(fGenDetHit_X[j]*100,fGenDetHit_Y[j]*100,kineE/n_events);//fill cyl. detector	
	      else
		Histo_RadDet[5][pid][4]->Fill(fGenDetHit_X[j]*100,fGenDetHit_Y[j]*100,kineE/n_events);//fill cyl. detector	
	    }	
	    else if (detid==1 || detid==2)
	      Histo_RadDet[5][pid][detid]->Fill(fGenDetHit_Z[j]*100,fGenDetHit_X[j]*100,kineE/n_events);//fill cyl. detector //index 5 will fill all the vertices (vrtx from 0 to 6)	    
	    //index 5 will fill all the vertices (vrtx from 0 to 5) if used here
	    if (detid==0)
	      Histo_RadDet[5][pid][0]->Fill(phi,fGenDetHit_Y[j]*100,kineE/n_events);//fill cyl. detector
	    else if (detid==1 || detid==2)
	      Histo_RadDet[5][pid][detid]->Fill(fGenDetHit_Z[j]*100,fGenDetHit_X[j]*100,kineE/n_events);//fill cyl. detector
	      
	    //Fill vertex 2D plots for all vertices using index 5
	    HistoVertex_RadDet[5][pid][keid][0]->Fill(fGenDetHit_VX[j]*100,fGenDetHit_VY[j]*100,kineE/n_events);
	    HistoVertex_RadDet[5][pid][keid][1]->Fill(fGenDetHit_VZ[j]*100,TMath::Sqrt(TMath::Power(fGenDetHit_VX[j]*100,2)+TMath::Power(fGenDetHit_VY[j]*100,2)),kineE/n_events);
	    

	  }else if (hit_radius > hit_radius_min[vrtx_z])//without this condition warning will print for tracks going to the dump
	    printf("warning: energy outside the ranges %4.3f \n",kineE);
	} //else
	  //printf("warning: vertex outside the ranges z = %4.3f cm \n",fGenDetHit_VZ[j]*100);
	

	





	//now repeat above set for three hall detectors to get totals for each detector
	
	      
      }
	

      
    }
    
    if (i>200000  && earlybreak)
      break;
    if (i%500000==0)
      printf("Event %d of %d \n",i,nentries);


  }
  
  
  //vertex based histograms
  if (kShow1DKEPlots){
    TCanvas * canvas_ke[7][3];
    for(Int_t i=0;i<7;i++){//vtx
      for(Int_t j=0;j<3;j++){//pid
	canvas_ke[i][j]= new TCanvas(Form("canvas_ke_vrtx%d_pid%d",i+1,j+1),Form("canvas_ke_vrtx%d_pid%d",i+1,j+1),1500,500);
	canvas_ke[i][j]->Divide(3,1);
	for(Int_t k=0;k<3;k++){//KE
	  canvas_ke[i][j]->cd(k+1);
	  if (k>1)
	    canvas_ke[i][j]->cd(k+1)->SetLogy();
	  Histo_kineE[i][j][k]->Draw();
	}
	if (kSave1DKEHisto)
	  canvas_ke[i][j]->SaveAs(plotsFolder+Form("canvas_ke_vrtx%d_pid%d.png",i+1,j+1));
	if (kSaveRootFile)
	  canvas_ke[i][j]->Write();
      }
    }
  }
  if (kShow1DVrtxPlots){
    TCanvas * canvas_vertx[6][3];
    TCanvas * canvas_vertx_noWeight[6][3];
    TCanvas * canvas_vertx_integral[6][3];
    Double_t norme;
    Double_t *integral;
    for(Int_t i=0;i<6;i++){//vertex // ADD ON TO THIS FUNCTIONALITY: This creates x vs y plots of birth vertices in 6 z vertex regions
      for(Int_t j=0;j<3;j++){//pid
	canvas_vertx[i][j]= new TCanvas(Form("canvas_vertx_vrtx%d_pid%d",i+1,j+1),Form("canvas_vertx_vrtx%d_pid%d",i+1,j+1),1500,500);	
	canvas_vertx[i][j]->Divide(3,1);
	canvas_vertx_noWeight[i][j]= new TCanvas(Form("canvas_vertx_noWeight_vrtx%d_pid%d",i+1,j+1),Form("canvas_vertx_noWeight_vrtx%d_pid%d",i+1,j+1),1500,500);	
	canvas_vertx_noWeight[i][j]->Divide(3,1);

	canvas_vertx_integral[i][j]= new TCanvas(Form("canvas_vertx_integral_vrtx%d_pid%d",i+1,j+1),Form("canvas_vertx_integral_vrtx%d_pid%d",i+1,j+1),1500,500);	
	canvas_vertx_integral[i][j]->Divide(3,1);
	for(Int_t k=0;k<3;k++){//KE
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
    TCanvas * canvas_hallrad[6];
    for(Int_t i=0;i<6;i++){//vertex // THESE ARE THE TOP Heatmaps vs. vertex region
      canvas_hallrad[i] = new TCanvas(Form("canvas_hallrad_vrtx%d",i+1),Form("canvas_hallrad_vrtx%d",i+1),1500,1500);
      canvas_hallrad[i]->Divide(3,3);
      for(Int_t j=0;j<3;j++){//pid
	for(Int_t k=0;k<3;k++){//detid
	  canvas_hallrad[i]->cd(3*j+1+k);
	  canvas_hallrad[i]->cd(3*j+1+k)->SetLogz();
	  Histo_RadDet[i][j][k]->Draw("colz");	
	  Histo_RadDet[i][j][k]->SetStats(0);
	}
      }
      if (kSave2DHisto)
	canvas_hallrad[i]->SaveAs(plotsFolder+Form("canvas_hallrad_vrtx%d.png",i+1));
      if (kSaveRootFile)
	canvas_hallrad[i]->Write();
    }

    TCanvas * canvas_hallrad_cyc_xy[6];
    for(Int_t i=0;i<6;i++){//vertex
      canvas_hallrad_cyc_xy[i] = new TCanvas(Form("canvas_hallrad_cyc_xy_vrtx%d",i+1),Form("canvas_hallrad_cyc_xy_vrtx%d",i+1),1500,1500);
      canvas_hallrad_cyc_xy[i]->Divide(2,3);
      for(Int_t j=0;j<3;j++){//pid
	for(Int_t k=0;k<2;k++){//detid
	  canvas_hallrad_cyc_xy[i]->cd(2*j+1+k);
	  canvas_hallrad_cyc_xy[i]->cd(2*j+1+k)->SetLogz();
	  Histo_RadDet[i][j][3+k]->Draw("colz");	
	  Histo_RadDet[i][j][3+k]->SetStats(0);
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
    TCanvas * canvas_hallrad_xy_vrtx[6];
    TCanvas * canvas_hallrad_rz_vrtx[6];
    for(Int_t i=0;i<6;i++){//vertex
      canvas_hallrad_xy_vrtx[i]=new TCanvas(Form("canvas_hallrad_xy_vrtx%d",i+1),Form("canvas_hallrad_xy_vrtx%d",i+1),1500,1500);
      canvas_hallrad_rz_vrtx[i]=new TCanvas(Form("canvas_hallrad_rz_vrtx%d",i+1),Form("canvas_hallrad_rz_vrtx%d",i+1),1500,1500);
      canvas_hallrad_xy_vrtx[i]->Divide(3,3);
      canvas_hallrad_rz_vrtx[i]->Divide(3,3);
      for(Int_t j=0;j<3;j++){//pid
	for(Int_t k=0;k<3;k++){//detid
	  canvas_hallrad_xy_vrtx[i]->cd(3*j+1+k);
	  canvas_hallrad_xy_vrtx[i]->cd(3*j+1+k)->SetLogz();
	  HistoVertex_RadDet[i][j][k][0]->Draw("colz");// THESE ARE THE vertices of particle birth plots	
	  HistoVertex_RadDet[i][j][k][0]->SetStats(0);

	  canvas_hallrad_rz_vrtx[i]->cd(3*j+1+k);
	  canvas_hallrad_rz_vrtx[i]->cd(3*j+1+k)->SetLogz();
	  HistoVertex_RadDet[i][j][k][1]->Draw("colz");	
	  HistoVertex_RadDet[i][j][k][1]->SetStats(0);
	}
      }
      if (kSave2DVertexHisto){
	canvas_hallrad_xy_vrtx[i]->SaveAs(plotsFolder+Form("canvas_hallrad_xy_vrtx%d.png",i+1));
	canvas_hallrad_rz_vrtx[i]->SaveAs(plotsFolder+Form("canvas_hallrad_rz_vrtx%d.png",i+1));
      }
      if (kSaveRootFile){
	canvas_hallrad_xy_vrtx[i]->Write();
	canvas_hallrad_rz_vrtx[i]->Write();
      }
    }
  }
  //end of plots for hall detector

  Char_t * detector[4] = {"Total","Side","Top","Bottom"};
  Char_t * chpid[3] = {"abs(electrons)","Photons","Neutrons"};
  Char_t * chenrange[3]={"E<10","10<E<100","100<E"};


  TList * list_power = new TList;
  TString strline;
  char line[400];
  char line1[400];
  strline="Rootfile name";
  list_power->Add(new TObjString(strline));
  list_outputs << line << endl;
  strline=outputFolder;//"/home/rakithab/Simulation_Analysis/Remoll/MOLLER_12GeV/Rootfiles/";//main root files used//_allshield_BorConcrete
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

  for(Int_t i=0;i<3;i++){//pid
    for(Int_t j=0;j<3;j++){//energy range
      printf(" %20s %20s",chpid[i],chenrange[j]);
      sprintf(line," %20s %20s",chpid[i],chenrange[j]);
      sprintf(line1," ");//empty previous values
      for(Int_t k=0;k<3;k++){//detector
	for (Int_t s=0;s<6;s++)
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
  for (Int_t i=0;i<6;i++){
    for(Int_t j=0;j<3;j++){//pid
      for(Int_t k=0;k<3;k++){//energy range
	printf(" %20s %20s %20s",svertex[i].Data(),chpid[j],chenrange[k]);
	sprintf(line," %20s %30s %20s",svertex[i].Data(),chpid[j],chenrange[k]);
	sprintf(line1," ");//empty previous values
	for(Int_t l=0;l<3;l++){//detector
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
  for(Int_t i=0;i<3;i++){//pid
    for(Int_t j=0;j<3;j++){//energy range
      printf(" %20s %20s",chpid[i],chenrange[j]);
      sprintf(line," %20s %20s",chpid[i],chenrange[j]);
      sprintf(line1," ");//empty previous values
      for(Int_t k=0;k<3;k++){//detector
	for (Int_t s=0;s<6;s++)
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
  for (Int_t i=0;i<6;i++){
    for(Int_t j=0;j<3;j++){//pid
      for(Int_t k=0;k<3;k++){//energy range
	printf(" %20s %20s %20s",svertex[i].Data(),chpid[j],chenrange[k]);
	sprintf(line," %20s %30s %20s",svertex[i].Data(),chpid[j],chenrange[k]);
	sprintf(line1," ");//empty previous values
	for(Int_t l=0;l<3;l++){//detector
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

    Double_t stops[NRGBs] = { 0.00, 0.34, 0.61, 0.84, 1.00 };
    Double_t red[NRGBs]   = { 0.00, 0.00, 0.87, 1.00, 0.51 };
    Double_t green[NRGBs] = { 0.00, 0.81, 1.00, 0.20, 0.00 };
    Double_t blue[NRGBs]  = { 0.51, 1.00, 0.12, 0.00, 0.00 };
    TColor::CreateGradientColorTable(NRGBs, stops, red, green, blue, NCont);
    gStyle->SetNumberContours(NCont);
}
