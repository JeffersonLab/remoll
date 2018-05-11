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

// This class is a refactored version of the electrons_thru_lead_analysis_rate_weighted.C that looks at pions instead. Aside from some basic changes to the names of plots and such,
// the actual structure of the analysis needed to change slighlty. Namely, boolean checks were removed as requiring pions to sequentially hit all the detectors to be counted resulted in an
// unrealistic attrition rate due to minor radial drift.

class my_analysis: public remoll {
public:

   // Define histrograms
   TH1D* h_hit_r_moller_det;
   TH1D* h_hit_e_pre_lead;
   TH1D* h_hit_e_showermax_back;
   TH1D* h_hit_e_showermax_front;
   TH1D* h_hit_e_post_lead;
   TH1D* h_hit_r_post_lead;
   TH1D* h_hit_e_post_lead_in_lucite_radial_range;
   TH1D* h_hit_r_pre_lead;
   TH1D* h_hit_e_pre_lead_collimator;
   TH1D* h_hit_e_pre_lead_plug;
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
   TH2D* h_x_y_showermax_incident;
   TH2D* h_x_y_lead_incident;
   TH2D* h_x_y_lead_exiting;
   // Note that for 1D histograms only the X axis needs its scale set, the Y axis will auto-adjust.
   Int_t nbins = 100; 
   my_analysis(const TString& name = "/u/home/mundy/remoll/test_remollout.root"): remoll(name) {
                // Initialize histograms
               
                h_hit_r_moller_det = new TH1D("h_hit_e_moller_det","Moller Virtual Detector Radial Spectrum",nbins,0,0);
                
                h_hit_e_pre_lead = new TH1D("h_hit_e_pre_lead","Pre Lead Complete Energy Spectrum",nbins,0,0);
   
                h_hit_e_post_lead = new TH1D("h_hit_e_post_lead","Post Lead Complete Energy Spectrum",nbins,0,0);

                h_hit_e_showermax_front = new TH1D("h_hit_e_showermax_front","Incident Showermax Energy Spectrum",nbins,0,0);
               
                h_hit_e_showermax_back = new TH1D("h_hit_e_showermax_back","Exiting Showermax Complete Energy Spectrum",nbins,0,0);
                

                h_ev_e_showermax_front = new TH1D("h_ev_e_showermax_front","Energy of incident Primary pi-'s",nbins,0,0);                
                h_ev_r_showermax_front = new TH1D("h_ev_r_showermax_front","Radii of incident Primary pi-'s",nbins,0,0);                

                h_ev_e_showermax_thru = new TH1D("h_ev_e_showermax_thru","Energy distribution of primary pi-'s that hit the showermax and had something go through",nbins,0,0);                
                h_ev_r_showermax_thru = new TH1D("h_ev_r_showermax_thru","Energy distribution of events that had pi-'s hit the showermax",nbins,0,0);                

                h_hit_r_pre_lead = new TH1D("h_hit_r_pre_lead","Pre Lead Radial Distribution",nbins,0,0);
                h_hit_r_post_lead = new TH1D("h_hit_r_post_lead","Post Lead Radial Distribution",nbins,0,0);
               
                h_ev_showermax_exit_hits = new TH1D("h_ev_showermax_exit_hits","Number of exit hits per event",nbins,0,0);
                h_ev_showermax_exit_hits->GetXaxis()->SetTitle("Number of hits exiting showermax for a single event");

                h_ev_showermax_exit_from_primary_hits = new TH1D("h_ev_showermax_exit_from_primary_hits","Number of exit hits per primary pi- incident hit",nbins,0,0);
                h_ev_showermax_exit_from_primary_hits->GetXaxis()->SetTitle("Number of hits exiting showermax from a single entering primary");
              

                h_ev_e_leaving_showermax = new TH1D("h_ev_e_leaving_showermax","Net Energy of pi-'s Exiting Showermax",nbins,0,0);
                h_ev_e_leaving_lead = new TH1D("h_ev_e_leaving_lead","Net Energy of pi-'s Exiting Lead",nbins,0,0);

                h_ev_e_incident_lead = new TH1D("h_ev_e_incident_lead","Net Energy of pi-'s Incident on Lead",nbins,0,0);

                h_ev_e_primary_vs_number_of_hits_showermax = new TH2D("h_ev_e_primary_vs_number_of_hits_showermax","Energy of Primary Pi-'s vs Number of Pi-'s Leaving Showermax",nbins,0,0,nbins,0,0);
                        h_ev_e_primary_vs_number_of_hits_showermax->GetXaxis()->SetTitle("Primary Pi- Energy (MeV)");
                        h_ev_e_primary_vs_number_of_hits_showermax->GetYaxis()->SetTitle("Number of Exiting Pi-'s");

                
                h_ev_e_incident_vs_number_of_hits_lead = new TH2D("h_ev_e_incident_vs_number_of_hits_lead","Sum of Incident Pi-'s Energy from a Single Event vs Number of Pi-'s Leaving Lead from that Event",nbins,0,0,nbins,0,0);
                        h_ev_e_incident_vs_number_of_hits_lead->GetXaxis()->SetTitle("Sum of Incident Pi-'s Energy (MeV)");
                        h_ev_e_incident_vs_number_of_hits_lead->GetYaxis()->SetTitle("Number of Exiting Pi-'s");
                

                h_ev_hits_incident_vs_hits_leaving_lead = new TH2D("h_ev_hits_incident_vs_hits_leaving_lead", "Number of Pi-'s Incident on Lead vs Number of Pi-'s Exiting Lead for a Single Event",nbins,0,0,nbins,0,0);
                        h_ev_hits_incident_vs_hits_leaving_lead->GetXaxis()->SetTitle("Pi-'s Incident on lead");
                        h_ev_hits_incident_vs_hits_leaving_lead->GetYaxis()->SetTitle("Pi-'s Leaving Lead");
                


                h_ev_e_primary_vs_e_leaving_showermax = new TH2D("h_ev_e_primary_vs_e_leaving_showermax","Energy of Primary Pi-'s vs Sum of Pi- Energies Leaving Showermax",nbins,0,0,nbins,0,0);
                        h_ev_e_primary_vs_e_leaving_showermax->GetXaxis()->SetTitle("Primary Pi- Energy (MeV)");
                        h_ev_e_primary_vs_e_leaving_showermax->GetYaxis()->SetTitle("Total Energy of Exiting Pi-'s (MeV)");
                

                
                h_ev_e_incident_vs_e_leaving_lead = new TH2D("h_ev_e_incident_vs_e_leaving_lead","Sum of Pi- Energy Incident on Lead vs Sum of Pi- Energies Leaving Lead",nbins,0,0,nbins,0,0);
                        h_ev_e_incident_vs_e_leaving_lead->GetXaxis()->SetTitle("Sum of Incident Pi- Energy (MeV) from a Single Event");
                        h_ev_e_incident_vs_e_leaving_lead->GetYaxis()->SetTitle("Total Energy of Exiting Pi-s (MeV) from a Single Event");


                h_x_y_showermax_incident = new TH2D("h_x_y_showermax_incident", "X-Y distribution of Pi- Particles Incident on the Showermax",nbins,0,0,nbins,0,0);


                h_x_y_lead_incident = new TH2D("h_x_y_lead_incident", "X-Y distribution of Pi- Particles Incident on the Lead",nbins,0,0,nbins,0,0);


                h_x_y_lead_exiting = new TH2D("h_x_y_lead_exiting", "X-Y distribution of Pi- Particles Exiting the Lead",nbins,0,0,nbins,0,0);
              }  


         // LOOP function
         // Note that looping through millions of events will take several minutes regardless of what you ask it to do with them.
         void Loop(){

         double primary_e_1 = 0;
         double primary_r_1 = 0;
         double exit_count_showermax = 0;
         double exit_count_lead = 0;
         double incident_count = 0;
         double sum_showermax_energy = 0;
         double sum_energy_post_lead = 0;
         double sum_incident_energy = 0;
               
                 if (fChain == 0) return;

                 // Counter used to print progress to terminal while the function is running
                 int events_crunched = 0;
                 
                 // create a stopwatch, which starts timing
                 
                 clock_t start_time = clock();
                  
                 // This for loop iterates through all events, events can register hits but are not themselves hits
                 for (Long64_t event = 0; event < fChain->GetEntries(); fChain->GetEntry(event++)) {
                        events_crunched++;
                        int remainder = events_crunched % 100000;
                        if( remainder == 0){
                                clock_t runtime = clock();
                                clock_t net_time = (runtime - start_time);
                                double  real_time = ((double)net_time)/CLOCKS_PER_SEC;       
                                double avg_per_sec = events_crunched / real_time;
                                std::cout << " " << events_crunched << " events processed at average rate of " << avg_per_sec << " events per second." << std::endl;
                                };
                        // reset doubles
                        primary_e_1 = 0;
                        primary_r_1 = 0;
                        exit_count_showermax = 0;
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
                                 //        // Could include pi+'s however it's probably not even worth the extra check every loop.
                                           if(hit->at(i).pid == -211){
                                
                                                 // Main Detector
                                                 if(hit->at(i).det == 28){
                                                        if(hit->at(i).pz > 0 && hit->at(i).r > 600){
                                                                h_hit_r_moller_det->Fill(hit->at(i).r, rate);
                                                                };
                                                        }
                                                 
                                                 // Front of showermax
                                                 else if(hit->at(i).det == 30 && hit->at(i).pz > 0){
                                                        h_hit_e_showermax_front->Fill(hit->at(i).p, rate);
                                                        // A particle with track id 1 or 2 seems to refer to one of the moller scattered electrons, there are no particles with trid 0, 3, 4, or 5
                                                        // and trid 1 and 2 seem to correspond to mother track id 0 which means they came from the particle gun. This also places a 1 MeV minimum energy constraint
                                                        if((hit->at(i).trid == 1 || hit->at(i).trid == 2) && hit->at(i).p > 1){
                                                                if(hit->at(i).z > 29073.98 && hit->at(i).z < 29074.02){
                                                                                 primary_e_1 = hit->at(i).p;
                                                                                 primary_r_1 = hit->at(i).r;
                                                                                 // Note that an x-y plot was added in this script
                                                                                 h_x_y_showermax_incident->Fill(hit->at(i).x,hit->at(i).y,rate);
                                                                                 
                                                                                
                                                                        }       
                                                                }
                                                        }
                                                 // Back of showermax
                                                 else if(hit->at(i).det == 31 && hit->at(i).pz > 0 && hit->at(i).p > 1){
                                                                // because the booleans have been removed we need to fill the plots at their respective detectors, this would be the typical method for an analysis script 
                                                                exit_count_showermax++; 
                                                                sum_showermax_energy += hit->at(i).p;
                                                                h_ev_e_showermax_front->Fill(primary_e_1,rate);
                                                                h_ev_r_showermax_front->Fill(primary_r_1,rate);
     
                                                        }
                                                 // pre-lead
                                                 else if(hit->at(i).det == 4050 && hit->at(i).pz > 0 && hit->at(i).p > 1){
                                                                // Again, booleans have been removed so plots are filled as they come up
                                                                incident_count++
                                                                sum_incident_energy += hit->at(i).p;
                                                                h_x_y_lead_incident->Fill(hit->at(i).x,hit->at(i).y,rate);
                                                                h_ev_e_showermax_thru->Fill(primary_e_1,rate);
                                                                h_ev_r_showermax_thru->Fill(primary_r_1,rate);
                                                                h_ev_showermax_exit_from_primary_hits->Fill(exit_count_showermax,rate);

                                                                h_ev_e_leaving_showermax->Fill(sum_showermax_energy,rate);
                                                                h_ev_e_primary_vs_number_of_hits_showermax->Fill(primary_e_1, exit_count_showermax, rate);
                                                                h_ev_e_primary_vs_e_leaving_showermax->Fill(primary_e_1, sum_showermax_energy, rate);


                                                        
                                                        
                                                        }
                                                 // post-lead
                                                 else if(hit->at(i).det == 4051 && hit->at(i).pz > 0 && hit->at(i).p > 1){
                                                                exit_count_lead++;
                                                                sum_energy_post_lead += hit->at(i).p;
                                                                h_x_y_lead_exiting->Fill(hit->at(i).x,hit->at(i).y,rate);

                                                                h_ev_e_incident_lead->Fill(sum_incident_energy, rate);
                                                                h_ev_e_incident_vs_number_of_hits_lead->Fill(sum_incident_energy,exit_count_lead, rate);
                                                                h_ev_hits_incident_vs_hits_leaving_lead->Fill(incident_count,exit_count_lead,rate);
                                                                h_ev_e_leaving_lead->Fill(sum_energy_post_lead, rate);
                                                                h_ev_e_incident_vs_e_leaving_lead->Fill(sum_incident_energy,sum_energy_post_lead, rate);   
                                                        }
                                                                
                                                   
                                                };
                                
                                         } 
                                 
                                 }
                                 // The only plot that is filled outside the loop is the number of exit hits, there is no particular reason for it to be here other than that I didn't move it.
                                 h_ev_showermax_exit_hits->Fill(exit_count_showermax,rate);
                                       }
};
        
    
        void Draw(){

                // Create canvases and draw histograms
                Int_t canvas_x = 600;
                Int_t canvas_y = 400;

                 
                TCanvas *c1 = new TCanvas("c1", "showermax_front_pions", canvas_x, canvas_y);
                TCanvas *c2 = new TCanvas("c2", "showermax_front_primary_pions_energy", canvas_x, canvas_y);
                TCanvas *c3 = new TCanvas("c3", "showermax_front_primary_pions_radius", canvas_x, canvas_y);
                TCanvas *c4 = new TCanvas("c4", "primary_pions_with_tracks_thru_showermax_energy", canvas_x, canvas_y);
                TCanvas *c5 = new TCanvas("c5", "primary_pions_with_tracks_thru_showermax_radius", canvas_x, canvas_y);
                TCanvas *c6 = new TCanvas("c6", "pions_exiting_showermax_for_all_events", canvas_x, canvas_y);
                TCanvas *c7 = new TCanvas("c7", "pions_exiting_showermax_for_primary_events", canvas_x, canvas_y);

                TCanvas *c8 = new TCanvas("c8", "sum_of_exiting_pion_energy_for_a_primary", canvas_x, canvas_y);
                TCanvas *c9 = new TCanvas("c9", "primary_pion_energy_vs_number_of_hits", canvas_x, canvas_y);
                TCanvas *c10 = new TCanvas("c10", "primary_pion_energy_vs_sum_of_exiting_pion_energies", canvas_x, canvas_y);

                TCanvas *c11 = new TCanvas("c11", "sum_of_pion_energies_incident_on_lead_for_a_single_event", canvas_x, canvas_y);
                TCanvas *c12 = new TCanvas("c12", "sum_of_incident_pion_energies_vs_number_of_exiting_pions", canvas_x, canvas_y);
                TCanvas *c13 = new TCanvas("c13", "sum_of_incident_hits_vs_exiting_hits_for_a_single_event", canvas_x, canvas_y);
                TCanvas *c14 = new TCanvas("c14", "sum_of_pion_energies_leaving_lead_for_a_single_event", canvas_x, canvas_y);
                TCanvas *c15 = new TCanvas("c15", "sum_of_incident_pion_energies_vs_sum_of_exiting_pion_energies_for_a_single_event", canvas_x, canvas_y);
 
                TCanvas *c16 = new TCanvas("c16", "pion_x_y_incident_on_showermax", canvas_x, canvas_y);
                TCanvas *c17 = new TCanvas("c17", "pion_x_y_incident_on_lead", canvas_x, canvas_y);
                TCanvas *c18 = new TCanvas("c18", "pion_x_y_exiting_lead", canvas_x, canvas_y);

                TCanvas *c19 = new TCanvas("c19", "pions_at_main_det_radial_dist", canvas_x, canvas_y);


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
                
                c16->cd();
                h_x_y_showermax_incident->Draw("COLZ");

                c17->cd();
                h_x_y_lead_incident->Draw("COLZ");

                c18->cd();
                h_x_y_lead_exiting->Draw("COLZ");

                c19->cd();
                gPad->SetLogy();
                h_hit_r_moller_det->Draw();                

                /*
                // Write effective Entries to a text file.
                ofstream my_file;
                myfile.open("/u/home/mundy/remoll/moller_output/100_mm/600_series/energy_spectrum_effective_entries.txt");
                if(my_file.is_open()){}
                        // write stuff
             */

                // Write any plots that may be overlayed into .root files
                
                // Creates a new file, or overwrites an existing one of the same name
               /* TFile* file = new TFile("/u/scratch/mundy/remoll/pion_plots/root_files_for_overlay/post_lead_energy/xs_weighted_electron_E_post_leadrate_weighted_100mm.root","RECREATE");
                
                // The file is opened on creation, and any histograms can be written. Write histograms, not canvases, 
                // multiple histograms can be written, they must be individually requested by a TFile object via ->Get("name")
                h_hit_e_post_lead->Write();
                file->Close();
                delete file;
                */
                //Save plots!
                
                c1->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/showermax_front_Erate_weighted_100mm.png");
                c1->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/showermax_front_Erate_weighted_100mm.C");

                c2->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/primary_showermax_front_Erate_weighted_100mm.png");
                c2->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/primary_showermax_front_Erate_weighted_100mm.C");

                c3->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/primary_showermax_front_rrate_weighted_100mm.png");
                c3->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/primary_showermax_front_rrate_weighted_100mm.C");

                c4->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/primary_thru_showermax_Erate_weighted_100mm.png");
                c4->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/primary_thru_showermax_Erate_weighted_100mm.C");

                c5->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/primary_thru_showermax_rrate_weighted_100mm.png");
                c5->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/primary_thru_showermax_rrate_weighted_100mm.C");

                c6->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/all_events_exit_hitsrate_weighted_100mm.png");
                c6->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/all_events_exit_hitsrate_weighted_100mm.C");

                c7->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/primary_events_exit_hitsrate_weighted_100mm.png");
                c7->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/primary_events_exit_hitsrate_weighted_100mm.C");
                
                c8->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/electron_net_energy_leaving_showermaxrate_weighted_100mm.png");
                c8->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/electron_net_energy_leaving_showermaxrate_weighted_100mm.C");
                
                c9->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/primary_event_energy_vs_exit_hitsrate_weighted_100mm.png");
                c9->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/primary_event_energy_exit_hitsrate_weighted_100mm.C");
                
                c10->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/primary_event_energy_vs_net_energy_of_exiting_pionsrate_weighted_100mm.png");
                c10->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/primary_event_energy_vs_net_energy_of_exiting_pionsrate_weighted_100mm.C");
                
                c11->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/net_energy_incident_on_lead_per_eventrate_weighted_100mm.png");
                c11->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/net_energy_incident_on_lead_per_eventrate_weighted_100mm.C");
                
                c12->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/net_incident_energy_vs_number_of_exiting_pionsrate_weighted_100mm.png");
                c12->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/net_incident_energy_vs_number_of_exiting_pionsrate_weighted_100mm.C");
                
                c13->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/incident_pions_vs_number_of_pions_exiting_leadrate_weighted_100mm.png");
                c13->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/incident_pions_vs_number_of_pions_exiting_leadrate_weighted_100mm.C");
                
                c14->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/net_energy_of_pions_leaving_leadrate_weighted_100mm.png");
                c14->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/net_energy_of_pions_leaving_leadrate_weighted_100mm.C");
                
                c15->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/net_incident_energy_vs_net_energy_of_pions_leaving_leadrate_weighted_100mm.png");
                c15->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/net_incident_energy_vs_net_energy_of_pions_leaving_leadrate_weighted_100mm.C");
               

                c16->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/x_y_plot_pion_incident_on_showermaxrate_weighted_100mm.png");
                c16->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/x_y_plot_pion_incident_on_showermaxrate_weighted_100mm.C");

                c17->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/x_y_plot_pion_incident_on_leadrate_weighted_100mm.png");
                c17->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/x_y_plot_pion_incident_on_leadrate_weighted_100mm.C");

                c18->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/x_y_plot_pion_exiting_leadrate_weighted_100mm.png");
                c18->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/x_y_plot_pion_exiting_leadrate_weighted_100mm.C");

                c19->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/main_det_r_pion_rate_weighted_100mm.png");
                c19->SaveAs("/u/home/mundy/remoll/pion_plots/100_mm/660_series/main_det_r_pion_rate_weighted_100mm.C");
                 };
};
