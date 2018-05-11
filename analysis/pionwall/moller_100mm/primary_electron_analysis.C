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

//This is an older analysis script which looks at primary electrons going through the showermax. This uses cross section weighting rather than rate weighting and the results can be seen in moller elog 94.

class my_analysis: public remoll {
public:

   // Define histrograms
   TH1D* h_hit_e_moller_det;
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
   // Note that for 1D histograms only the X axis needs its scale set, the Y axis will auto-adjust.
   Int_t nbins = 100; 
   my_analysis(const TString& name = "/u/home/mundy/remoll/test_remollout.root"): remoll(name) {
                // Initialize histograms
               
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
               
                h_ev_showermax_exit_hits = new TH1D("h_ev_showermax_exit_hits","Number of exit hits per event",nbins,0,0);
                h_ev_showermax_exit_hits->GetXaxis()->SetTitle("Number of hits exiting showermax for a single event");

                h_ev_showermax_exit_from_primary_hits = new TH1D("h_ev_showermax_exit_from_primary_hits","Number of exit hits per primary electron incident hit",nbins,0,0);
                h_ev_showermax_exit_from_primary_hits->GetXaxis()->SetTitle("Number of hits exiting showermax from a single entering primary");
              
                h_hit_e_pre_lead_collimator = new TH1D("h_hit_e_pre_lead_collimator","Pre lead energy in collimator extension",nbins,0,0);
                h_hit_e_pre_lead_plug = new TH1D("h_hit_e_pre_lead_plug","Pre lead energy in plug extension",nbins,0,0);
              }

         // LOOP function
         // Note that looping through millions of events will take several minutes regardless of what you ask it to do with them.
         void Loop(){

         // These booleans are used to decide whether to add an EVENT (rather than a hit) to a histogram
         bool is_primary = false;
         bool left_showermax = false;
         bool hit_showermax = false;
         double primary_e_1 = 0;
         double primary_r_1 = 0;
         double exit_count = 0;
               
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
                        // reset booleans
                        is_primary = false;
                        left_showermax = false;
                        hit_showermax = false;
                        primary_e_1 = 0;
                        primary_r_1 = 0;
                        exit_count = 0;
                        


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
                                           if(hit->at(i).pid == 11){
                                                if(hit->at(i).det == 30 && hit->at(i).pz > 0){
                                                        h_hit_e_showermax_front->Fill(hit->at(i).p, ev->xs);
                                                        // A particle with track id 1 or 2 seems to refer to one of the moller scattered electrons, there are no particles with trid 0, 3, 4, or 5
                                                        // and trid 1 and 2 seem to correspond to mother track id 0 which means they came from the particle gun. This also places a 1 MeV minimum energy constraint
                                                        if((hit->at(i).trid == 1 || hit->at(i).trid == 2) && hit->at(i).p > 1){
                                                                if(hit->at(i).z > 29073.98 && hit->at(i).z < 29074.02){
                                                                                 hit_showermax = true;
                                                                                 // We are operating on the assumption that only one primary moller electron can hit the showermax for a single event
                                                                                 // Since we may want to use these values in multiple plots depending on what booleans are triggered, we store them into doubles instead
                                                                                 // of directly filling the histograms here.
                                                                                 primary_e_1 = hit->at(i).p;
                                                                                 primary_r_1 = hit->at(i).r;
                                                                                
                                                                                 
                                                                                
                                                                        }       
                                                                }
                                                        }
                                                 else if(hit->at(i).det == 31 && hit->at(i).pz > 0 && hit->at(i).p > 1){
                                                        // We're using an integer counter to count how many electrons exit
                                                        exit_count++;
                                                        if(hit_showermax == true){
                                                                        // since we already have the energy and radius of the primary electron stored we just flag the boolean and move on.
                                                                        left_showermax = true;
                                                                        
                                                                 }
                                                        }
                                                                        
                                                               
                                                                       
                                                   
                                                };
                                
                                         } 
                                 }
                                 // This is where the hit iteration ends and we check the booleans to decide which histograms to fill, before the next event is looped over.
                                 h_ev_showermax_exit_hits->Fill(exit_count,ev->xs);
                                 if(hit_showermax == true){
                                        h_ev_e_showermax_front->Fill(primary_e_1,ev->xs);
                                        h_ev_r_showermax_front->Fill(primary_r_1,ev->xs);
                                        if(left_showermax == true){
                                                h_ev_e_showermax_thru->Fill(primary_e_1,ev->xs);
                                                h_ev_r_showermax_thru->Fill(primary_r_1,ev->xs);
                                                h_ev_showermax_exit_from_primary_hits->Fill(exit_count,ev->xs);
                                                }
                                        }
                         }   
                 };
        
    
        void Draw(){

                // Create canvases and draw histograms
                Int_t canvas_x = 600;
                Int_t canvas_y = 400;

                
                TCanvas *c1 = new TCanvas("c1", "showermax_front_electrons", canvas_x, canvas_y);
                TCanvas *c2 = new TCanvas("c2", "showermax_front_primary_electrons_energy", canvas_x, canvas_y);
                TCanvas *c3 = new TCanvas("c3", "showermax_front_primary_electrons_radius", canvas_x, canvas_y);
                TCanvas *c4 = new TCanvas("c4", "primary_electrons_with_tracks_thru_showermax_energy", canvas_x, canvas_y);
                TCanvas *c5 = new TCanvas("c5", "primary_electrons_with_tracks_thru_showermax_radius", canvas_x, canvas_y);
                TCanvas *c6 = new TCanvas("c6", "electrons_exiting_showermax_for_all_events", canvas_x, canvas_y);
                TCanvas *c7 = new TCanvas("c7", "electrons_exiting_showermax_for_primary_events", canvas_x, canvas_y);
 
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
               
               
                /*
                // Write effective Entries to a text file.
                ofstream my_file;
                myfile.open("/u/home/mundy/remoll/moller_output/100_mm/600_series/energy_spectrum_effective_entries.txt");
                if(my_file.is_open()){
                        // write stuff
             */

                // Write any plots that may be overlayed into .root files
                
                // Creates a new file, or overwrites an existing one of the same name
               /* TFile* file = new TFile("/u/scratch/mundy/remoll/moller_plots/root_files_for_overlay/post_lead_energy/xs_weighted_electron_E_post_lead_100mm.root","RECREATE");
                
                // The file is opened on creation, and any histograms can be written. Write histograms, not canvases, 
                // multiple histograms can be written, they must be individually requested by a TFile object via ->Get("name")
                h_hit_e_post_lead->Write();
                file->Close();
                delete file;
                */
                //Save plots!
                
                c1->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/showermax_front_E_100mm.png");
                c1->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/showermax_front_E_100mm.C");

                c2->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_showermax_front_E_100mm.png");
                c2->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_showermax_front_E_100mm.C");

                c3->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_showermax_front_r_100mm.png");
                c3->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_showermax_front_r_100mm.C");

                c4->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_thru_showermax_E_100mm.png");
                c4->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_thru_showermax_E_100mm.C");

                c5->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_thru_showermax_r_100mm.png");
                c5->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_thru_showermax_r_100mm.C");

                c6->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/all_events_exit_hits_100mm.png");
                c6->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/all_events_exit_hits_100mm.C");

                c7->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_events_exit_hits_100mm.png");
                c7->SaveAs("/u/home/mundy/remoll/moller_plots/100_mm/620_series/primary_events_exit_hits_100mm.C");
                          };
};
