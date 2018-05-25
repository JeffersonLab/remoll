#include "remoll.h"

#include <TH2.h>
#include <TStyle.h>
#include <TCanvas.h>

// Note that you may need to include additional root libraries if you use ones not defined in remoll.h


// This script defines a local analysis class containing member functions Loop() and Draw(),
// since these functions are defined locally, you can have many different files using specific instances of these functions without having to 
// redefine anything. 
//
// Loop() allows the user to define what parameters are looped over and what cuts are placed, filling histograms with the
// relevant information.
//
// Draw() allows the user to define the dimensions, number and names of plots produced as well as save them. This is essentially 
// a method containing a root script.
//
//
//
// Run commands in this order:
// 
// //Start reroot
// 
// build/reroot
//
// //Load in the script, by default named Loop.C 
// 
// .L analysis/<SCRIPT_NAME>
//
// //Create a class object of type my_analysis named t or any other valid name, pass it a string of the file path to your root files,
// //may inlcude a wildcard (i.e. *) at the end to pass in all files in the directory starting with <FILE_NAME> e.g. TEST*.root would pass in TEST_1.root, TEST_2.root, etc.
// 
// my_analysis t("PATH/TO/FILES/<FILE_NAME>*.root")
//
// //Call the Loop() method of the class object to loop over the data and store it in histograms
// 
// t.Loop()
//
// //Call the Draw() method of the class object to draw plots and save histograms
// 
// t.Draw()



//   In a ROOT session, you can also do:
//      root> t.GetEntry(12); // Fill t data members with entry number 12
//      root> t.Show();       // Show values of entry 12
//      root> t.Show(16);     // Read and show values of entry 16



// The local class, my_analysis is defined as a subclass of the class remoll (see remoll.h), the local instance is given access to all the methods defined in
// remoll.h, and also serves to define the specific Loop method and Draw method to a given analysis.


class my_analysis: public remoll {
public:

   // prototype any histograms used in the program here
   TH1D* h_hit_n;
   TH1D* h_hit_e;


   // This defines the constructor for my_analysis class objects, taking a file path as input
   my_analysis(const TString& name = "/u/home/mundy/remoll/test_remollout.root"): remoll(name) {
  

         // Define histograms globally within the class so that Loop() and Draw() have access to them
         h_hit_n = new TH1D("h_hit_n","h_hit_n",1000,0,100000)
         h_hit_e = new TH1D("h_hit_e","h_hit_e",1000,0,12000);
  
         } 


         // LOOP function
         void Loop(){
        
                 if (fChain == 0) return;

                 // This for loop iterates through all events, events can register hits but are not themselves hits
                 for (Long64_t event = 0; event < fChain->GetEntries(); fChain->GetEntry(event++)) {


 
                        // Use integer counters to find number of events, or hits, histograms may be directly filled with counter values
                        
                        // Counter is reset for each event, bins filled with number of hits of each event, not the raw 
                        // number of hits 
                        Int_t counter = 0;     


                         // Use 'if' statements to select what cuts you want on events, actual analysis should be placed in either the Draw() method or its own method,
                         
                         // Checks that the hit vector contains data
                         if( hit->size() > 0){

                                 // Iterates through every hit associated with a given event (this loop is nested inside the one above)
                                 for (size_t i = 0; i < hit->size(); i++) {

                                 // Ideally nest as many statements as possible to
                                 // reduce absolute number of comparisons 

                                         // C++ vectors have a method at(), which is equivalent to using an index, hit->at(i) could be read hit[i]
                                         // Note that hit is actually a vector here, and needs to be indexed to actually access the attributes of a given hit object 
                                         if(hit->at(i).pid == 11){
                                                
                                                // virtual detector before lead is 4050, after lead is 4051
                                                if(hit->at(i).det == 4050){

                                                        // Fill command can be thought of as analogous to vector.push_back, it adds a single data point to the given histogram
                                                        h_hit_e->Fill(hit->at(i).p);
                                                        counter ++;
                                                }
                                
                                         } 
                                 }
                         // number of hits filled outside of nested loop, but within the event for loop
                         h_hit_n->Fill(counter);
                         }   
                 }
        };
    

        // This method can be used to define everything related to actually drawing the histograms, the canvases, draw commands and save commands should be put here.
        // A separate method should be created for numerical analysis of the data such as finding averages (for good coding practice), however if you only want to print
        // something to the screen it can be added in here, so long as Draw() is called after Loop().
        void Draw(){

                // Create canvases and draw histograms
                Int_t canvas_x = 600;
                Int_t canvas_y = 400;

                TCanvas *c1 = new TCanvas("c1", "name_never_shown", canvas_x, canvas_y);
                TCanvas *c2 = new TCanvas("c2", "name_never_shown", canvas_x, canvas_y);

                c1->cd();
                h_hit_n->Draw();
                c2->cd();
                h_hit_e->Draw();

                //Save plots!
                c1->SaveAs("/u/scratch/mundy/remoll/moller_plots/100mm/test_100mm_E.png");
                c2->SaveAs("/u/scratch/mundy/remoll/moller_plots/100mm/test_100mm_n.png");
        };
};
