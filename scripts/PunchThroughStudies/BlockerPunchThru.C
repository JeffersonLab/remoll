// Simple example macro for reroot analysis of remoll simulations for MOLLER
//  
//  Need to use reroot (CERN's root compiled with special remoll libraries,
//  or root when you have linked to ibremoll.so, i.e. by a command like
//    setenv LD_PRELOAD build/libremoll.so
//  needs to have a subdirectory "images/Blocker" defined for the images to go to
//
//  Run using (for example):
//   build/reroot
//   .L scripts/BlockerPunchThru.C
//   BlockerPunchThru()
//
//  Can put multiple root files in the script, and they will be chained together.
//
//  This script will take root files of a certain Blocker thickness. It will first loop
//  through the events and flag any in which a primary particle hits a Blocker hole.
//  Then it will exclude these flagged events and plot electrons and positrons that hit
//  upstream and downstream of the surface of the Blocker. Those particles that hit
//  downstream of the surface are flagged as "punch through particles". 
//  Punch-through particles are plotted on the rings of the main detector. An
//  xy plot and radial plot are included. Finally, all charged particles that reach the
//  main detector rings are plotted (xy and r).

#include <TF1.h>
void BlockerPunchThru()
{
  //Allow Tree to use the root files that you call
  TChain* T = new TChain("T");

  //Add files from the directory "rootfilesPT/", "##0" is the Blocker thickness in mm. Replace these root files with whatever root files you would like to analyze.
  T->Add("rootfilesPT/blocker/WBlockerC12_ep_100k_##0mm0.root");
  T->Add("rootfilesPT/blocker/WBlockerC12_ep_100k_##0mm1.root");
  T->Add("rootfilesPT/blocker/WBlockerC12_ep_100k_##0mm2.root");
  T->Add("rootfilesPT/blocker/WBlockerC12_ep_100k_##0mm3.root");
  T->Add("rootfilesPT/blocker/WBlockerC12_ep_100k_##0mm4.root");
  T->Add("rootfilesPT/blocker/WBlockerC12_ep_100k_##0mm5.root");
  T->Add("rootfilesPT/blocker/WBlockerC12_ep_100k_##0mm6.root");
  T->Add("rootfilesPT/blocker/WBlockerC12_ep_100k_##0mm7.root");
  T->Add("rootfilesPT/blocker/WBlockerC12_ep_100k_##0mm8.root");
  T->Add("rootfilesPT/blocker/WBlockerC12_ep_100k_##0mm9.root");

  //Define variables that we will loop over later
  Double_t rate = 0;
  std::vector<remollEventParticle_t>* parts = 0;
  std::vector<remollGenericDetectorHit_t>* hits = 0;

  //Define some branches of the Tree (which is "T")
  T->SetBranchAddress("rate", &rate);
  T->SetBranchAddress("hit", &hits);
  T->SetBranchAddress("part", &parts);

  gROOT -> SetBatch(kTRUE);

//----------------------------------------------------------------------------

  //Define Histograms that we will fill later

  //---Blocker Surfaces---

  //2D histogram for e+ and e- hitting the upstream Blocker surface
  TH2F *us_Blocker_virt = new TH2F("us_Blocker_virt","Upstream Blocker (##0mm) Virtual Detector Charged Particle Hits", 100, -150, 150, 100, -150, 150);
  us_Blocker_virt -> GetXaxis() -> SetTitle("x Position of Hits [mm]");
  us_Blocker_virt -> GetYaxis() -> SetTitle("y Position of Hits [mm]");

  //2D histogram for e+ and e- hitting the downstream Blocker surface, i.e., the particles that punch through the Blocker
  TH2F *ds_Blocker_virt = new TH2F("ds_Blocker_virt","Downstream Blocker (##0mm) Virtual Detector Charged Particle Hits", 100, -150, 150, 100, -150, 150);
  ds_Blocker_virt -> GetXaxis() -> SetTitle("x Position of Hits [mm]");
  ds_Blocker_virt -> GetYaxis() -> SetTitle("y Position of Hits [mm]");

  //---Main Detector PT---

  //1D histogram for e+ and e- that punch through the Blocker and make it to the main detector on the rings
  TH1F *radial_main = new TH1F("radial_main","Energies of Charged Particles that Exited the Downstream Blocker (##0mm) Surface and Hit the Main Detector", 50, 0, 1500);
  radial_main -> GetXaxis() -> SetTitle("Energies of Hits [MeV]");

  //2D histogram for particles that punch through the Blocker and make it to the main detector on the rings (xy plot)
  TH2F *punch_thru_xy_main = new TH2F("punch_thru_xy_main","Charged Particles that Exited the Downstream Blocker (##0mm) Surface and Hit the Main Detector", 100, -1200, 1200, 100, -1200, 1200);
  punch_thru_xy_main -> GetXaxis() -> SetTitle("x Position of Hits [mm]");
  punch_thru_xy_main -> GetYaxis() -> SetTitle("y Position of Hits [mm]");

  //---All Charged Particles---

  //1D histogram for e+ and e- that make it to the main detector on the rings
  TH1F *radial_main_all = new TH1F("radial_main_all","Energies of All Charged Particles that Hit the Main Detector for a ##0mm Blocker", 50, 0, 1500);
  radial_main_all -> GetXaxis() -> SetTitle("Energies of Hits [MeV]");

  //2D histogram for particles that make it to the main detector on the rings (xy plot)
  TH2F *xy_main_all = new TH2F("xy_main_all","All Charged Particles that Hit the Main Detector for a ##0mm Blocker", 100, -1200, 1200, 100, -1200, 1200);
  xy_main_all -> GetXaxis() -> SetTitle("x Position of Hits [mm]");
  xy_main_all -> GetYaxis() -> SetTitle("y Position of Hits [mm]");

  //---Booleans---

  //Define logical flag for punch-through
  bool punch_thru = false;

//----------------------------------------------------------------------------

  //Loop over all events
  for (size_t iev = 0; iev < T->GetEntries(); iev++)
  {
    T->GetEntry(iev);

    punch_thru = false;

    //Process over hits
    for (size_t ihit = 0; ihit < hits->size(); ihit++)
    {
      remollGenericDetectorHit_t hit = hits->at(ihit);

      if ((hit.pid==11 || hit.pid==-11)&& hit.r >=35.306 && hit.r<97.9932)
      {

          if (hit.det == 1004 && hit.pz >= 0)
          {
            us_Blocker_virt -> Fill(hit.x, hit.y, rate);
          }//end upstream if

          if (hit.det == 1005 && hit.pz >= 0)
          {
            ds_Blocker_virt -> Fill(hit.x, hit.y, rate);
            //Flag particles as "punch through particles"
            punch_thru = true; 
          }//end downstream if

      }//end pid and hole if

    }//end process over hits

    //process over hits again to look at the main detector
    for (size_t ihit = 0; ihit < hits->size(); ihit++)
    {
      remollGenericDetectorHit_t hit = hits->at(ihit);

        //Only look at "punch through particles" that hit the main detector on rings 1-7 and make sure they are e- or e+ with positive z momentum.
        if (punch_thru && hit.det==28 && hit.r>=650 && hit.r<=1200 && (hit.pid==11 || hit.pid==-11) && hit.pz>=0)
        {
          punch_thru_xy_main -> Fill(hit.x, hit.y, rate);
          radial_main -> Fill(hit.e, rate);
       	}//end main det if

        //Only look at charged particles that hit the main detector on rings 1-7.
        if (hit.det==28 && hit.r>=650 && hit.r<=1200 && hit.pz>=0 && (hit.pid==11 || hit.pid==-11))
        {
          xy_main_all -> Fill(hit.x, hit.y, rate);
          radial_main_all -> Fill(hit.e, rate);
      	}//end main det if

    }//end process over hits
  }//end process over events

//--------------------------------------------------------------------------------
//Draw and save the histograms

  //---Blocker Surfaces---

  //Create a canvas on which to draw the histograms
  TCanvas *virt = new TCanvas("virt","Charged Particles Hitting the Virtual Detectors Upstream and Downstream of the Blocker", 1000, 1000, 1000, 1000);

  //Divide the canvas into 1 column, 2 rows
  virt -> Divide(1,2);

  //Look at the first entry on the canvas and draw the upstream detector
  virt -> cd(1);
  us_Blocker_virt -> Draw("colz");

  //Look at the second entry on the canvas and draw the downstream detector
  virt -> cd(2);
  ds_Blocker_virt -> Draw("colz");

  //Save the canvas as an image in the director "remoll/images/Blocker/"
  virt -> SaveAs("images/Blocker/Punch_Thru_US_DS_WBlocker_##0.png");


  //---Main Detector PT---

  //Create a canvas on which to draw the histograms
  TCanvas *main = new TCanvas("main","Electrons and Positrons Hitting the Main Detector", 1000, 1000, 1000, 1000);

  //Divide the canvas into 1 column, 2 rows
  main -> Divide(1,2);

  //Look at the first entry on the canvas and draw the xy main detector
  main -> cd(1);
  punch_thru_xy_main -> Draw("colz");

  //Look at the second entry on the canvas and draw the r main detector
  main -> cd(2);
  radial_main -> Draw("B");

  //Save the canvas as an image in the director "remoll/images/Blocker/"
  main -> SaveAs("images/Blocker/Punch_Thru_Main_WBlocker_##0.png");


  //---Main Detector---

  //Create a canvas on which to draw the histograms
  TCanvas *main_all_charged = new TCanvas("main_all_charged","Particles Hitting the Main Detector", 1000, 1000, 1000, 1000);

  //Divide the canvas into 1 column, 2 rows
  main_all_charged -> Divide(1,2);

  //Look at the first entry on the canvas and draw the xy main detector
  main_all_charged -> cd(1);
  xy_main_all -> Draw("colz");

  //Look at the second entry on the canvas and draw the r main detector
  main_all_charged -> cd(2);
  radial_main_all -> Draw("B");

  //Save the canvas as an image in the director "remoll/images/Blocker/"
  main_all_charged -> SaveAs("images/Blocker/Main_All_WBlocker_##0.png");


}//end BlockerPunchThru()

