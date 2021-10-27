// Simple example macro for reroot analysis of remoll simulations for MOLLER
//
//  Need to use reroot (CERN's root compiled with special remoll libraries,
//  or root when you have linked to ibremoll.so, i.e. by a command like
//    setenv LD_PRELOAD build/libremoll.so
//  needs to have a subdirectory "images/NewPT" defined for the images to go to
//
//  Run using (for example):
//   build/reroot
//   .L scripts/PunchThru##C12ep.C
//   PunchThru##C12ep()
//
//  Can put multiple root files in the script, and they will be chained together. You must
//  specify which root files that this should run over.

//  This script was written with the intention of looking at ep scattering on the C12
//  thin target, but the script should work for any event generator and any target.
//
//  This script will take root files of a certain sieve thickness, ##mm. It will first loop
//  through the events and flag any in which a primary particle hits a sieve hole.
//  Then it will exclude these flagged events and plot electrons and positrons that hit
//  upstream and downstream of the surface of the sieve. Those particles that hit
//  downstream of the surface are flagged as "punch through particles".
//  Punch-through particles are plotted on the rings of the main detector. An
//  xy plot and radial plot are included. Finally, all charged particles that reach the
//  main detector rings are plotted (xy and r).
//
//  Addition: I now also plot electrons and positrons that hit the main detector after
//  passing through the sieve holes. This is so that I can look at the amount of noise our
//  signal is likely to see.

#include <TF1.h>
void PunchThru##C12ep()
{
  //Allow Tree to use the root files that you call
  TChain* T = new TChain("T");

  //Add files from the directory "rootfilesPT/", "##" is the sieve thickness in mm. Replace these root files with whatever root files you would like to analyze.
  T->Add("rootfilesPT/WSieveC12_ep_100k_80mm0.root");
  T->Add("rootfilesPT/WSieveC12_ep_100k_##mm1.root");
  T->Add("rootfilesPT/WSieveC12_ep_100k_##mm2.root");
  T->Add("rootfilesPT/WSieveC12_ep_100k_##mm3.root");
  T->Add("rootfilesPT/WSieveC12_ep_100k_##mm4.root");
  T->Add("rootfilesPT/WSieveC12_ep_100k_##mm5.root");
  T->Add("rootfilesPT/WSieveC12_ep_100k_##mm6.root");
  T->Add("rootfilesPT/WSieveC12_ep_100k_##mm7.root");
  T->Add("rootfilesPT/WSieveC12_ep_100k_##mm8.root");
  T->Add("rootfilesPT/WSieveC12_ep_100k_##mm9.root");

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

  //---Sieve surface histograms---

  //2D histogram for e+ and e- hitting the upstream sieve surface
  TH2F *us_sieve_virt = new TH2F("us_sieve_virt","Upstream Sieve (##mm) Virtual Detector Charged Particle Hits for ep Scattering with Thin Carbon Target", 7000, -150, 150, 7000, -150, 150);
  us_sieve_virt -> GetXaxis() -> SetTitle("x Position of Hits [mm]");
  us_sieve_virt -> GetYaxis() -> SetTitle("y Position of Hits [mm]");

  //2D histogram for e+ and e- hitting the downstream sieve surface, i.e., the particles that punch through the sieve
  TH2F *ds_sieve_virt = new TH2F("ds_sieve_virt","Downstream Sieve (##mm) Virtual Detector Charged Particle Hits for ep Scattering with Thin Carbon Target", 7000, -150, 150, 7000, -150, 150);
  ds_sieve_virt -> GetXaxis() -> SetTitle("x Position of Hits [mm]");
  ds_sieve_virt -> GetYaxis() -> SetTitle("y Position of Hits [mm]");

  //---Main detector histograms---

  //2D histogram for e+ and e- that punch through the sieve and make it to the main detector on the rings (xy plot)
  TH2F *punch_thru_main = new TH2F("punch_thru_main","Charged Particles that Exited the Downstream Sieve (##mm) Surface and Hit the Main Detector for ep Scattering with Thin Carbon Target", 1000, -1200, 1200, 1000, -1200, 1200);
  punch_thru_main -> GetXaxis() -> SetTitle("x Position of Hits [mm]");
  punch_thru_main -> GetYaxis() -> SetTitle("y Position of Hits [mm]");

  //1D histogram for e+ and e- that punch through the sieve and make it to the main detector on the rings (r plot)
  TH1F *radial_main = new TH1F("radial_main","Charged Particles that Exit the Downstream Sieve (##mm) Surface and Hit the Main Detector", 1000, 650, 1200);
  radial_main -> GetXaxis() -> SetTitle("r Position of Hits [mm]");

  //2D histogram for electrons and positrons that make it to the main detector on the rings after passing through the sieve holes (xy plot)
  TH2F *main_holes = new TH2F("main_holes","Charged Particles that Pass Through the Sieve Holes (##mm) and Hit the Main Detector for ep Scattering with Thin Carbon Target", 1000, -1200, 1200, 1000, -1200, 1200);
  main_holes -> GetXaxis() -> SetTitle("x Position of Hits [mm]");
  main_holes -> GetYaxis() -> SetTitle("y Position of Hits [mm]");

  //1D histogram for electrons and positrons that make it to the main detector on the rings after passing through the sieve holes (r plot)
  TH1F *radial_main_holes = new TH1F("radial_main_holes","Charged Particles that Pass Through the Sieve Holes (##mm) and Hit the Main Detector", 1000, 650, 1200);
  radial_main_holes -> GetXaxis() -> SetTitle("r Position of Hits [mm]");

  //2D histogram for all electrons and positrons that make it to the main detector on the rings (xy plot)
  TH2F *main_all = new TH2F("main_all","All Charged Particles that Hit the Main Detector for ep Scattering with Thin Carbon Target and a ##mm Sieve", 1000, -1200, 1200, 1000, -1200, 1200);
  main_all -> GetXaxis() -> SetTitle("x Position of Hits [mm]");
  main_all -> GetYaxis() -> SetTitle("y Position of Hits [mm]");

  //1D histogram for all electrons and positrons that make it to the main detector on the rings (r plot)
  TH1F *radial_main_all = new TH1F("radial_main_all","All Charged Particles that Hit the Main Detector for ep Scattering with Thin Carbon Target and a ##mm Sieve", 1000, 650, 1200);
  radial_main_all -> GetXaxis() -> SetTitle("r Position of Hits [mm]");

//----------------------------------------------------------------------------

  //Define logical flag for each hole
  bool no_hole_1 = false;
  bool no_hole_2 = false;
  bool no_hole_3 = false;

  bool hole_1 = false;
  bool hole_2 = false;
  bool hole_3 = false;

  //Define logical flag for punch-through. When this is true, it means that we are looking at perticles that exit the downstream surface of the sieve.
  bool punch_thru = false;

//----------------------------------------------------------------------------

  //Loop over all events
  for (size_t iev = 0; iev < T->GetEntries(); iev++)
  {
    T->GetEntry(iev);

    //Define initial values for booleans
    no_hole_1 = true;
    no_hole_2 = true;
    no_hole_3 = true;
    punch_thru = false;

    //Process hits, i.e., loop over all the hits in this event
    for (size_t ihit = 0; ihit < hits->size(); ihit++)
    {
      remollGenericDetectorHit_t hit = hits->at(ihit);

      //no_hole_# is false if the primary electron goes through the hole
      if (hit.det == 1001 && (hit.trid == 1))
      {
        no_hole_1 = false;
      }

      if (hit.det == 1002 && (hit.trid == 1))
      {
        no_hole_2 = false;
      }

      if (hit.det == 1003 && (hit.trid == 1))
      {
        no_hole_3 = false;
      }
    }//end process over hits

    //Process over hits again
    for (size_t ihit = 0; ihit < hits->size(); ihit++)
    {
      remollGenericDetectorHit_t hit = hits->at(ihit);

      //Look at only charged particles for events in which a primary electron did NOT go through a sieve hole. The radial restriction is to focus on only the sieve surface.
      if ((hit.pid==11 || hit.pid==-11) && no_hole_1 && no_hole_2 && no_hole_3 && hit.r >=35.306 && hit.r<97.9932)
      {

        //Look at only the virtual detector placed exactly upstream of the sieve and ensure that we are only looking at particles with positive z momentum.
        if (hit.det == 1004 && hit.pz >= 0)
        {
          //Fill the histogram that plots only particles that hit the upstream sieve surface
          us_sieve_virt -> Fill(hit.x, hit.y, rate);
        }//end upstream if

          if (hit.det == 1005 && hit.pz >= 0)
          {
            //Fill the histogram that plots only particles that exit the downstream sieve surface
            ds_sieve_virt -> Fill(hit.x, hit.y, rate);
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
          //Fill histogram for charged particles that exit the downstream surface of the sieve and reach the main detector.
          punch_thru_main -> Fill(hit.x, hit.y, rate);
          //This is the same plot as above but is radial instead of xy.
          radial_main -> Fill(hit.r, rate);
	       }//end main det if

    }//end process over hits

  }//end process over events

//----------------------------------------------------------------------------

  //Loop over all events to flag the holes differently.
  for (size_t iev = 0; iev < T->GetEntries(); iev++)
  {
    T->GetEntry(iev);

    hole_1 = false;
    hole_2 = false;
    hole_3 = false;

    //Process over hits to look at the main detector
    for (size_t ihit = 0; ihit < hits->size(); ihit++)
    {
      remollGenericDetectorHit_t hit = hits->at(ihit);

        //Look at all charged particles that hit the main detector on rings 1-7 and make sure they are e- or e+ with positive z momentum.
       if (hit.det==28 && hit.r>=650 && hit.r<=1200 && (hit.pid==11 || hit.pid==-11) && hit.pz>=0)
        {
          //Fill a histogram with ALL charged particles that hit the main detector.
          main_all -> Fill(hit.x, hit.y, rate);
          //This is the same plot as above but is radial instead of xy.
          radial_main_all -> Fill(hit.r, rate);
        }//end hole and radius if

    }//end process over hits

    //Process hits, i.e., loop over all the hits in this event
    for (size_t ihit = 0; ihit < hits->size(); ihit++)
    {
      remollGenericDetectorHit_t hit = hits->at(ihit);

      //hole_# is true if the primary electron goes through the hole
      if (hit.det == 1001 && (hit.trid == 1))
      {
        hole_1 = true;
      }

      if (hit.det == 1002 && (hit.trid == 1))
      {
        hole_2 = true;
      }

      if (hit.det == 1003 && (hit.trid == 1))
      {
        hole_3 = true;
      }

    }//end process over hits

    //process over hits to look at the main detector
    for (size_t ihit = 0; ihit < hits->size(); ihit++)
    {
      remollGenericDetectorHit_t hit = hits->at(ihit);

        //Only look at sieve hole particles that hit the main detector on rings 1-7 and make sure they are e- or e+ with positive z momentum.
       if ((hole_1 || hole_2 || hole_3) && hit.det==28 && hit.r>=650 && hit.r<=1200 && (hit.pid==11 || hit.pid==-11) && hit.pz>=0)
        {
          //Fill the histogram that plots the charged particles that go through the sieve holes.
          main_holes -> Fill(hit.x, hit.y, rate);
          //This is the same plot as above but is radial instead of xy.
          radial_main_holes -> Fill(hit.r, rate);
        }//end hole and radius if

    }//end process over hits

  }//end process over events


//--------------------------------------------------------------------------------
//Draw and save the histograms

  //---Sieve Surfaces---

  //Create a canvas on which to draw the histograms
  TCanvas *virt = new TCanvas("virt","Charged Particles Hitting the Virtual Detectors Upstream and Downstream of the Sieve", 1000, 1000, 1000, 1000);

  //Divide the canvas into 1 column, 2 rows
  virt -> Divide(1,2);

  //Look at the first entry on the canvas and draw the upstream detector
  virt -> cd(1);
  us_sieve_virt -> Draw();

  //Look at the second entry on the canvas and draw the downstream detector
  virt -> cd(2);
  ds_sieve_virt -> Draw();

  //Save the canvas as an image in the director "remoll/images/PT/"
  virt -> SaveAs("images/NewPT/Punch_Thru_US_DS_WSieve_##.png");

  //---Main Detector Punch Through---

  //Create a canvas on which to draw the histograms
  TCanvas *main = new TCanvas("main","Electrons and Positrons Hitting the Main Detector", 1000, 1000, 1000, 1000);

  //Divide the canvas into 1 column, 2 rows
  main -> Divide(1,2);

  //Look at the first entry on the canvas and draw the xy main detector
  main -> cd(1);
  punch_thru_main -> Draw();

  //Look at the second entry on the canvas and draw the r main detector
  main -> cd(2);
  radial_main -> Draw();

  //Save the canvas as an image in the director "remoll/images/PT/"
  main -> SaveAs("images/NewPT/Punch_Thru_Main_WSieve_##.png");


  //---Main Detector All---

  //Create a canvas on which to draw the histograms
  TCanvas *main_for_all = new TCanvas("main_for_all","Particles Hitting the Main Detector", 1000, 1000, 1000, 1000);

  //Divide the canvas into 1 column, 2 rows
  main_for_all -> Divide(1,2);

  //Look at the first entry on the canvas and draw the xy main detector
  main_for_all -> cd(1);
  main_all -> Draw();

  //Look at the second entry on the canvas and draw the r main detector
  main_for_all -> cd(2);
  radial_main_all -> Draw();

  //Save the canvas as an image in the director "remoll/images/PT/"
  main_for_all -> SaveAs("images/NewPT/Punch_Thru_Main_All_WSieve_##.png");

  //---Main Detector Holes---

  //Create a canvas on which to draw the histograms
  TCanvas *main_for_holes = new TCanvas("main_for_holes","Particles Hitting the Main Detector", 1000, 1000, 1000, 1000);

  //Divide the canvas into 1 column, 2 rows
  main_for_holes -> Divide(1,2);

  //Look at the first entry on the canvas and draw the xy main detector
  main_for_holes -> cd(1);
  main_holes -> Draw();

  //Look at the second entry on the canvas and draw the r main detector
  main_for_holes -> cd(2);
  radial_main_holes -> Draw();

  //Save the canvas as an image in the director "remoll/images/PT/"
  main_for_holes -> SaveAs("images/NewPT/Punch_Thru_Main_Holes_WSieve_##.png");

}//end PunchThru##C12ep()

