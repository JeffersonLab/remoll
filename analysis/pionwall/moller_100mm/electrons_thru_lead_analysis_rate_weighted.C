#include "../remoll.h"

#include <TH2.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <ctime>
#include <TProfile2D.h>
#include <TPad.h>
#include <TAttLine.h>
#include <iostream>
#include <fstream>
// Local analysis class
//
// Run commands in this order from the remoll directory:
// build/reroot 
// .L analysis/<SCRIPT_NAME>
// my_analysis t("PATH/TO/FILES/<FILE_NAME>*.root")
// t.Loop()
// t.Draw()

// This is the most up-to-date and heavily commented electron analysis script, and is essentially an extension of the primary electron analysis script which consequently is not nearly as heavily commented.

// This script uses the same structure as the primary electron analysis to track electrons going through the lead wall. This has been altered to have rate weighting rather than
// cross section weighting.
//
// The resulting plots can be seen in moller elog 99, and in moller elog 94, although 94 was using cross section weighting.

class my_analysis: public remoll {
public:

   // Define histrograms
   // I have used a convention in which all histogram names start with h_, followed by hit if they look at hit parameters or ev if they look at event parameters, before including the variable and a qualifier as to what detector
   // is beign looked at.
   TH1D* h_hit_e_moller_det;
   TH1D* h_hit_e_pre_lead;
   TH1D* h_hit_e_showermax_back;
   TH1D* h_hit_e_showermax_front;
   TH1D* h_hit_e_post_lead;
   TH1D* h_hit_r_post_lead;
   TH1D* h_hit_e_post_lead_in_lucite_radial_range;
   TH1D* h_hit_r_pre_lead;
   TH1D* h_ev_e_showermax_front;
   TH1D* h_ev_r_showermax_front;
   TH1D* h_ev_e_showermax_thru;
   TH1D* h_ev_r_showermax_thru;
   TH1D* h_ev_showermax_exit_hits;
   TH1D* h_ev_showermax_exit_from_primary_hits;

   TH1D* h_ev_e_leaving_showermax;
   TH1D* h_ev_e_leaving_lead;
   TH1D* h_ev_e_incident_lead;

   TH2D* h_ev_e_primary_vs_number_of_hits_showermax;
   TH2D* h_ev_e_incident_vs_number_of_hits_lead;
   TH2D* h_ev_e_primary_vs_e_leaving_showermax;
   TH2D* h_ev_e_incident_vs_e_leaving_lead;
   TH2D* h_ev_hits_incident_vs_hits_leaving_lead;
   // Note that for 1D histograms only the X axis needs its scale set, the Y axis will auto-adjust.
   //
   // create a bin variable so that you don't have to enter it in every histogram
   Int_t nbins = 100; 


   // This is a C++ class called my_analysis, it contains the histograms 
   my_analysis(const TString& name = "/u/home/mundy/remoll/test_remollout.root"): remoll(name) {
                
                // Initialize histograms
                // note that in general autoscaling is preferable unless you know the exact bounds you want the plot to have in which case you leave the x and y boundary parameters as 0,0 so that root
                // will autoscale the plot.
                h_hit_e_moller_det = new TH1D("h_hit_e_moller_det","Moller Virtual Detector Energy Spectrum",nbins,0,0);
                
                h_hit_e_pre_lead = new TH1D("h_hit_e_pre_lead","Pre Lead Complete Energy Spectrum",nbins,0,0);
   
                h_hit_e_post_lead = new TH1D("h_hit_e_post_lead","Post Lead Complete Energy Spectrum",nbins,0,0);

                h_hit_e_showermax_front = new TH1D("h_hit_e_showermax_front","Incident Showermax Energy Spectrum",nbins,0,0);
               
                h_hit_e_showermax_back = new TH1D("h_hit_e_showermax_back","Exiting Showermax Complete Energy Spectrum",nbins,0,0);
                

                h_ev_e_showermax_front = new TH1D("h_ev_e_showermax_front","Energy of incident Primary Electrons",nbins,0,0);                
                h_ev_r_showermax_front = new TH1D("h_ev_r_showermax_front","Radii of incident Primary Electrons",nbins,0,0);                

                h_ev_e_showermax_thru = new TH1D("h_ev_e_showermax_thru","Energy distribution of primary electrons that hit the showermax and had something go through",nbins,0,0);                
                h_ev_r_showermax_thru = new TH1D("h_ev_r_showermax_thru","Energy distribution of events that had electrons hit the showermax",nbins,0,0);                

                h_hit_r_pre_lead = new TH1D("h_hit_r_pre_lead","Pre Lead Radial Distribution",nbins,0,0);
                h_hit_r_post_lead = new TH1D("h_hit_r_post_lead","Post Lead Radial Distribution",nbins,0,0);
               
                // Note that you can manipulate axis titles and other parameters using these histogram classes.
                h_ev_showermax_exit_hits = new TH1D("h_ev_showermax_exit_hits","Number of exit hits per event",nbins,0,0);
                h_ev_showermax_exit_hits->GetXaxis()->SetTitle("Number of hits exiting showermax for a single event");

                h_ev_showermax_exit_from_primary_hits = new TH1D("h_ev_showermax_exit_from_primary_hits","Number of exit hits per primary electron incident hit",nbins,0,0);
                h_ev_showermax_exit_from_primary_hits->GetXaxis()->SetTitle("Number of hits exiting showermax from a single entering primary");
              

                h_ev_e_leaving_showermax = new TH1D("h_ev_e_leaving_showermax","Net Energy of Electrons Exiting Showermax",nbins,0,0);
                h_ev_e_leaving_lead = new TH1D("h_ev_e_leaving_lead","Net Energy of Electrons Exiting Lead",nbins,0,0);

                h_ev_e_incident_lead = new TH1D("h_ev_e_incident_lead","Net Energy of Electrons Incident on Lead",nbins,0,0);

                h_ev_e_primary_vs_number_of_hits_showermax = new TH2D("h_ev_e_primary_vs_number_of_hits_showermax","Energy of Primary Electrons vs Number of Electrons Leaving Showermax",nbins,0,0,nbins,0,0);
                        h_ev_e_primary_vs_number_of_hits_showermax->GetXaxis()->SetTitle("Primary Electron Energy (MeV)");
                        h_ev_e_primary_vs_number_of_hits_showermax->GetYaxis()->SetTitle("Number of Exiting Electrons");

                
                h_ev_e_incident_vs_number_of_hits_lead = new TH2D("h_ev_e_incident_vs_number_of_hits_lead","Sum of Incident Electron Energy from a Single Event vs Number of Electrons Leaving Lead from that Event",nbins,0,0,nbins,0,0);
                        h_ev_e_incident_vs_number_of_hits_lead->GetXaxis()->SetTitle("Sum of Incident Electron Energy (MeV)");
                        h_ev_e_incident_vs_number_of_hits_lead->GetYaxis()->SetTitle("Number of Exiting Electrons");
                

                h_ev_hits_incident_vs_hits_leaving_lead = new TH2D("h_ev_hits_incident_vs_hits_leaving_lead", "Number of Electrons Incident on Lead vs Number of Electrons Exiting Lead for a Single Event",nbins,0,0,nbins,0,0);
                        h_ev_hits_incident_vs_hits_leaving_lead->GetXaxis()->SetTitle("Electrons Incident on lead");
                        h_ev_hits_incident_vs_hits_leaving_lead->GetYaxis()->SetTitle("Electrons Leaving Lead");
                


                h_ev_e_primary_vs_e_leaving_showermax = new TH2D("h_ev_e_primary_vs_e_leaving_showermax","Energy of Primary Electrons vs Sum of Electron Energies Leaving Showermax",nbins,0,0,nbins,0,0);
                        h_ev_e_primary_vs_e_leaving_showermax->GetXaxis()->SetTitle("Primary Electron Energy (MeV)");
                        h_ev_e_primary_vs_e_leaving_showermax->GetYaxis()->SetTitle("Total Energy of Exiting Electrons (MeV)");
                

                
                h_ev_e_incident_vs_e_leaving_lead = new TH2D("h_ev_e_incident_vs_e_leaving_lead","Sum of Electron Energy Incident on Lead vs Sum of Electron Energies Leaving Lead",nbins,0,0,nbins,0,0);
                        h_ev_e_incident_vs_e_leaving_lead->GetXaxis()->SetTitle("Sum of Incident Electron Energy (MeV) from a Single Event");
                        h_ev_e_incident_vs_e_leaving_lead->GetYaxis()->SetTitle("Total Energy of Exiting Electrons (MeV) from a Single Event");
              }  


         // LOOP function
         // Note that looping through millions of events will take several minutes regardless of what you ask it to do with them.
         void Loop(){

         // These booleans are used to decide whether to add an EVENT (rather than a hit) to a histogram
         bool left_showermax = false;
         bool hit_showermax = false;
         bool hit_lead = false;
         bool left_lead = false;
        
         // Here we define doubles to store data in
         double primary_e_1 = 0;
         double primary_r_1 = 0;
         double exit_count_showermax = 0;
         double exit_count_lead = 0;
         double incident_count = 0;
         double sum_showermax_energy = 0;
         double sum_energy_post_lead = 0;
         double sum_incident_energy = 0;
               
                 // basic check to make sure we actually pulled in something
                 if (fChain == 0) return;

                 // Counter used to print progress to terminal while the function is running
                 int events_crunched = 0;
                 
                 // create a stopwatch, which starts timing
                 
                 clock_t start_time = clock();
                  
                 // This for loop iterates through all events, events can register hits but are not themselves hits
                 for (Long64_t event = 0; event < fChain->GetEntries(); fChain->GetEntry(event++)) {
                       
                        // This is the timer code, which is extremely useful because it tells you relative information on how long it takes to process each event, and how changing geometry affects this.
                        // Furthermore it prevents the terminal from freezing for an hour, and lets you know quickly if the script is actually processing or if it's hung up on something.
                        events_crunched++;
                        int remainder = events_crunched % 100000;
                        if( remainder == 0){
                                clock_t runtime = clock();
                                clock_t net_time = (runtime - start_time);
                                double  real_time = ((double)net_time)/CLOCKS_PER_SEC;       
                                double avg_per_sec = events_crunched / real_time;
                                std::cout << " " << events_crunched << " events processed at average rate of " << avg_per_sec << " events per second." << std::endl;
                                };

                        // reset booleans
                        left_showermax = false;
                        hit_showermax = false;
                        primary_e_1 = 0;
                        primary_r_1 = 0;
                        exit_count_showermax = 0;
                        hit_lead = false;
                        left_lead = false;
                        incident_count = 0;
                        sum_showermax_energy = 0;
                        sum_energy_post_lead = 0;
                        sum_incident_energy = 0;
                        exit_count_lead = 0;
                        


                         // Use 'if' statements to select what cuts you want on events, perform any analysis
                         // outside the loop in the function below.
                         // 
                         // Looping should be linear time, passing through each event and hit only once.
                         // Script runtime will increase with each comparison
                         if( hit->size() > 0){
                                

                                 for (size_t i = 0; i < hit->size(); i++) {
                                 // Ideally nest as many statements as possible to
                                 // reduce absolute number of comparisons 
                                 //
                                 // Note that Fill can take a second argument (or third for TH2) that is a weight. It seems like ev is defined as a remoll event in remolltypes.hh, so it's actually a struct with various attributes.
                                           
                                                // The first cut should be the particle id in this case electrons, nest all other cuts for that particle within it to save time.
                                                //
                                                // *NOTE* positrons should be included in future simulations, simply change the line below to if(hit->at(i).pid == 11 || hit->at(i).pid == -11){
                                                //
                                              if(hit->at(i).pid == 11){
                                                
                                                 // Unless you intend to look at backsplashing particles, put a cut to only count particles moving forward in the lab frame i.e. pz > 0        

                                                 // Front of showermax
                                                 if(hit->at(i).det == 30 && hit->at(i).pz > 0){
                                                        h_hit_e_showermax_front->Fill(hit->at(i).p, rate);
                                                        // A particle with track id 1 or 2 seems to refer to one of the moller scattered electrons, there are no particles with trid 0, 3, 4, or 5
                                                        // and trid 1 and 2 seem to correspond to mother track id 0 which means they came from the particle gun. This also places a 1 MeV minimum energy constraint
                                                        if((hit->at(i).trid == 1 || hit->at(i).trid == 2) && hit->at(i).p > 1){
                                                                
                                                                // The precise z coordinates were implemented due to a bug, they can probably be removed without consequence since the bug was due to a very strange batch of data
                                                                // rather than anything in the actual code.
                                                                if(hit->at(i).z > 29073.98 && hit->at(i).z < 29074.02){
                                                                                 hit_showermax = true;
                                                                                 primary_e_1 = hit->at(i).p;
                                                                                 primary_r_1 = hit->at(i).r;
                                                                                
                                                                                 
                                                                                
                                                                        }       
                                                                }
                                                        }
                                                 // back of showermax
                                                 else if(hit->at(i).det == 31 && hit->at(i).pz > 0 && hit->at(i).p > 1){
 
                                                        if(hit_showermax == true){
                                                                exit_count_showermax++; 
                                                                left_showermax = true;
                                                                sum_showermax_energy += hit->at(i).p;
                                                                        
                                                                 }
                                                        }
                                                 // pre-lead
                                                 else if(hit->at(i).det == 4050 && hit->at(i).pz > 0 && hit->at(i).p > 1){
                                                        // Since there can be multiple incident electrons per event, we can't look at them individually
                                                        // as such we manually "bin" the incident and exiting electrons based on the primary that ultimately produced them
                                                        // Furthermore, we only look at those primaries that had electrons leave the lead
                                                        
                                                        // We make an assumption that hits are stored in the tree logically, i.e. a hit after the showermax will be before a hit incident on the lead
                                                        if(left_showermax == true){
                                                                hit_lead = true;
                                                                incident_count++;
                                                                sum_incident_energy += hit->at(i).p;
                                                                }
                                                        
                                                        
                                                        }
                                                 // post-lead
                                                 else if(hit->at(i).det == 4051 && hit->at(i).pz > 0 && hit->at(i).p > 1){
                                                        if(hit_lead == true){
                                                                exit_count_lead++;
                                                                left_lead = true;
                                                                sum_energy_post_lead += hit->at(i).p;
                                                                }
                                                        }
                                                                
                                                   
                                                };
                                
                                         } 
                                 
                                 }

                                 // This is where the hit iteration ends and we check the booleans to decide which histograms to fill, before the next event is looped over.
                                 h_ev_showermax_exit_hits->Fill(exit_count_showermax,rate);
                                 if(hit_showermax == true){
                                        h_ev_e_showermax_front->Fill(primary_e_1,rate);
                                        h_ev_r_showermax_front->Fill(primary_r_1,rate);
                                        if(left_showermax == true){
                                                h_ev_e_showermax_thru->Fill(primary_e_1,rate);
                                                h_ev_r_showermax_thru->Fill(primary_r_1,rate);
                                                h_ev_showermax_exit_from_primary_hits->Fill(exit_count_showermax,rate);

                                                h_ev_e_leaving_showermax->Fill(sum_showermax_energy,rate);
                                                h_ev_e_primary_vs_number_of_hits_showermax->Fill(primary_e_1, exit_count_showermax, rate);
                                                h_ev_e_primary_vs_e_leaving_showermax->Fill(primary_e_1, sum_showermax_energy, rate);

                                                // Given that an electron has to have left the showermax for hit_lead to be true it makes sense to continue nesting
                                                if(hit_lead == true){
                                                        h_ev_e_incident_lead->Fill(sum_incident_energy, rate);
                                                        
                                                        if(left_lead == true){
                                                                h_ev_e_incident_vs_number_of_hits_lead->Fill(sum_incident_energy,exit_count_lead, rate);
                                                                h_ev_hits_incident_vs_hits_leaving_lead->Fill(incident_count,exit_count_lead,rate);
                                                                h_ev_e_leaving_lead->Fill(sum_energy_post_lead, rate);
                                                                h_ev_e_incident_vs_e_leaving_lead->Fill(sum_incident_energy,sum_energy_post_lead, rate);    
                                                                }
                                                        }
                                        }
                         }   
                
        }
};
        
        // This command should be run after Loop() returns you to the terminal. It actually fills and saves the histograms.
        void Draw(){

                // Create canvases and draw histograms
                Int_t canvas_x = 600;
                Int_t canvas_y = 400;

                // Note that the canvas "titles" don't show up in the plots however they are useful to know what you intend to put in a canvas.
                TCanvas *c1 = new TCanvas("c1", "showermax_front_electrons", canvas_x, canvas_y);
                TCanvas *c2 = new TCanvas("c2", "showermax_front_primary_electrons_energy", canvas_x, canvas_y);
                TCanvas *c3 = new TCanvas("c3", "showermax_front_primary_electrons_radius", canvas_x, canvas_y);
                TCanvas *c4 = new TCanvas("c4", "primary_electrons_with_tracks_thru_showermax_energy", canvas_x, canvas_y);
                TCanvas *c5 = new TCanvas("c5", "primary_electrons_with_tracks_thru_showermax_radius", canvas_x, canvas_y);
                TCanvas *c6 = new TCanvas("c6", "electrons_exiting_showermax_for_all_events", canvas_x, canvas_y);
                TCanvas *c7 = new TCanvas("c7", "electrons_exiting_showermax_for_primary_events", canvas_x, canvas_y);

                TCanvas *c8 = new TCanvas("c8", "sum_of_exiting_electron_energy_for_a_primary", canvas_x, canvas_y);
                TCanvas *c9 = new TCanvas("c9", "primary_electron_energy_vs_number_of_hits", canvas_x, canvas_y);
                TCanvas *c10 = new TCanvas("c10", "primary_electron_energy_vs_sum_of_exiting_electron_energies", canvas_x, canvas_y);

                TCanvas *c11 = new TCanvas("c11", "sum_of_electron_energies_incident_on_lead_for_a_single_event", canvas_x, canvas_y);
                TCanvas *c12 = new TCanvas("c12", "sum_of_incident_electron_energies_vs_number_of_exiting_electrons", canvas_x, canvas_y);
                TCanvas *c13 = new TCanvas("c13", "sum_of_incident_hits_vs_exiting_hits_for_a_single_event", canvas_x, canvas_y);
                TCanvas *c14 = new TCanvas("c14", "sum_of_electron_energies_leaving_lead_for_a_single_event", canvas_x, canvas_y);
                TCanvas *c15 = new TCanvas("c15", "sum_of_incident_electron_energies_vs_sum_of_exiting_electron_energies_for_a_single_event", canvas_x, canvas_y);
                
                //A log scale is often useful on the y axis so that your plot looks like an actual distribution rather than a straight line at the maximum.
                c1->cd();
                gPad->SetLogy();
                h_hit_e_showermax_front->Draw();

                c2->cd();
                gPad->SetLogy();
                h_ev_e_showermax_front->Draw();

                c3->cd();
                gPad->SetLogy();
                h_ev_r_showermax_front->Draw();

                c4->cd();
                gPad->SetLogy();
                h_ev_e_showermax_thru->Draw();

                c5->cd();
                gPad->SetLogy();
                h_ev_r_showermax_thru->Draw();
                
                c6->cd();
                h_ev_showermax_exit_hits->Draw();
                
                c7->cd();
                h_ev_showermax_exit_from_primary_hits->Draw();
               
                c8->cd();
                gPad->SetLogy();
                h_ev_e_leaving_showermax->Draw();

                // COLZ can be used on 2D histograms to add a "third" axis, a color dimension. This makes the plot into a heat map. In general you always want 2D histograms to use COLZ
                // because otherwise they just turn into unintelligible black blobs.
                c9->cd();
                h_ev_e_primary_vs_number_of_hits_showermax->Draw("COLZ");

                c10->cd();
                h_ev_e_primary_vs_e_leaving_showermax->Draw("COLZ");

                c11->cd();
                gPad->SetLogy();
                h_ev_e_incident_lead->Draw();
                
                c12->cd();
                h_ev_e_incident_vs_number_of_hits_lead->Draw("COLZ");

                c13->cd();
                h_ev_hits_incident_vs_hits_leaving_lead->Draw("COLZ");

                c14->cd();
                gPad->SetLogy();
                h_ev_e_leaving_lead->Draw();

                c15->cd();
                h_ev_e_incident_vs_e_leaving_lead->Draw("COLZ");


                // The commented out section below is the beginnings of writing to files, although it was never implemented in my scripts.
                /*
                // Write effective Entries to a text file.
                ofstream my_file;
                myfile.open("/u/home/mundy/remoll/moller_output/100_mm/600_series/energy_spectrum_effective_entries.txt");
                if(my_file.is_open()){}
                        // write stuff
             */

                // Write any plots that may be overlayed into .root files
                
                // Creates a new file, or overwrites an existing one of the same name
               /* TFile* file = new TFile("/u/scratch/mundy/remoll/moller_plots/root_files_for_overlay/post_lead_energy/xs_weighted_electron_E_post_lead_rate_weighted_100mm.root","RECREATE");
                
                // The file is opened on creation, and any histograms can be written. Write histograms, not canvases, 
                // multiple histograms can be written, they must be individually requested by a TFile object via ->Get("name")
                h_hit_e_post_lead->Write();
                file->Close();
                delete file;
                */
                //Save plots!
                //
                //Make sure you save each plot as both a .png and a .C file. The .C file allows you to reproduce and edit the plot at a later date without needing to run the entire analysis script again.
                //Also try to save the plots to a local directory as the scratch directory will delete them.
                
                c1->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/showermax_front_E_rate_weighted_100mm.png");
                c1->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/showermax_front_E_rate_weighted_100mm.C");

                c2->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_showermax_front_E_rate_weighted_100mm.png");
                c2->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_showermax_front_E_rate_weighted_100mm.C");

                c3->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_showermax_front_r_rate_weighted_100mm.png");
                c3->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_showermax_front_r_rate_weighted_100mm.C");

                c4->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_thru_showermax_E_rate_weighted_100mm.png");
                c4->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_thru_showermax_E_rate_weighted_100mm.C");

                c5->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_thru_showermax_r_rate_weighted_100mm.png");
                c5->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_thru_showermax_r_rate_weighted_100mm.C");

                c6->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/all_events_exit_hits_rate_weighted_100mm.png");
                c6->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/all_events_exit_hits_rate_weighted_100mm.C");

                c7->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_events_exit_hits_rate_weighted_100mm.png");
                c7->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_events_exit_hits_rate_weighted_100mm.C");
                
                c8->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/electron_net_energy_leaving_showermax_rate_weighted_100mm.png");
                c8->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/electron_net_energy_leaving_showermax_rate_weighted_100mm.C");
                
                c9->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_event_energy_vs_exit_hits_rate_weighted_100mm.png");
                c9->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_event_energy_exit_hits_rate_weighted_100mm.C");
                
                c10->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_event_energy_vs_net_energy_of_exiting_electrons_rate_weighted_100mm.png");
                c10->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_event_energy_vs_net_energy_of_exiting_electrons_rate_weighted_100mm.C");
                
                c11->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/net_energy_incident_on_lead_per_event_rate_weighted_100mm.png");
                c11->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/net_energy_incident_on_lead_per_event_rate_weighted_100mm.C");
                
                c12->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/net_incident_energy_vs_number_of_exiting_electrons_rate_weighted_100mm.png");
                c12->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/net_incident_energy_vs_number_of_exiting_electrons_rate_weighted_100mm.C");
                
                c13->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/incident_electrons_vs_number_of_electrons_exiting_lead_rate_weighted_100mm.png");
                c13->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/incident_electrons_vs_number_of_electrons_exiting_lead_rate_weighted_100mm.C");
                
                c14->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/net_energy_of_electrons_leaving_lead_rate_weighted_100mm.png");
                c14->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/net_energy_of_electrons_leaving_lead_rate_weighted_100mm.C");
                
                c15->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/net_incident_energy_vs_net_energy_of_electrons_leaving_lead_rate_weighted_100mm.png");
                c15->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/net_incident_energy_vs_net_energy_of_electrons_leaving_lead_rate_weighted_100mm.C");
               

                 };
};
