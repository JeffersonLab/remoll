#include <utility>
#include <vector>
#include <TString.h>
#include "TCut.h"
#include <list>
#include <dirent.h>
#include <unistd.h>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <algorithm>
#include <TSystem.h>
#include <TChain.h>
#include <TFile.h>
#include <TCanvas.h>
#include <TROOT.h>
#include <TH1.h>
#include <TH2.h>
void plotDetPlaneCuts(Double_t r_outer_cut=0, Int_t n_steps = 15, Double_t r_step = 10.0, Double_t z_cut = 27000.0, Int_t detector = 28, TString type = "moller", Int_t radialCut = 1, TString fname = "NULL"){
    /* I want to take a given ROOT output file and plot a specific detector's 
     * hit.x/y/r/theta, etc. distributions (weighted by rate)
     * and using cuts that are set at the z_cut position, cutting out all part.tjr 
     * (calculated radii) tracks that are > r_outer_cut
     *
     * I would also like to have this loop over r_outer_cut from some initial
     * to final in fine steps and make a plot of the number of cut out/in epelastic/moller 
     * particles (weighted by rate) vs. that cut radius
     *
     * I would also like to do a similar scan in z maybe... or at least compare at multiple
     * z cut places                                                                 */
   
    if (r_outer_cut==0) {
        std::cout<<"Usage: Load Data Root File to gDirectory, root> .x plotDetPlaneCuts(r_outer_cut=0, n_steps = 15, r_step = 10.0, z_cut = 27000.0, detector = 28, type = \"moller\")\n\n"<<std::endl;
        return;
    }

    TChain * T = new TChain("T");
    if (fname == "NULL"){
        T = (TChain*)gDirectory->Get("T");
    }
    else {
        T->Add(fname);
    }
    FILE *fp;
    fp = fopen(Form("Output_cuts.txt"),"a");

    Int_t n_events = 100000;
    Int_t start_event_n = 1;
    Double_t z_cut_space = 1.0; // mm

    const Int_t n_canvases = 100;
    const Int_t n_plots = 1;
    Double_t norm_rate_weight_all[n_canvases*n_plots];
    Double_t norm_rate_weight_in[n_canvases*n_plots];
    Double_t norm_rate_weight_out[n_canvases*n_plots];
    TCanvas *canvas[n_canvases];
    TH1F *totalRate[n_canvases*n_plots];
    TH1F *r_inside[n_canvases*n_plots];
    TH1F *r_outside[n_canvases*n_plots];
    TH2F *xy_inside[n_canvases*n_plots];
    TH2F *xy_outside[n_canvases*n_plots];

    for (Int_t n_canvas = 0; n_canvas < n_steps; n_canvas++) {
        r_outer_cut = r_outer_cut - r_step;
        canvas[n_canvas] = new TCanvas(Form("Plot of %s in detector %d hits for z plane %f, r > %f",type.Data(),detector,z_cut,r_outer_cut),Form("Plot of %s in detector %d hits for z plane %f, r > %f",type.Data(),detector,z_cut,r_outer_cut));
        canvas[n_canvas]->Divide(n_plots*2,2);
        for (Int_t n_plot = 0; n_plot<n_plots; n_plot++){
            std::cout<<"Plot number "<<n_canvas*n_plots+n_plot<<", n_canvas "<<n_canvas<<", n_plot "<<n_plot<<std::endl;

            canvas[n_canvas]->cd(n_plot+1);
            T->Draw(Form("hit.r[0]>>h_totalRate_%d",n_canvas*n_plots + n_plot),Form("rate*(hit.det[0] == %d && part.tjz > %f-%f && part.tjz < %f+%f)",detector,z_cut,z_cut_space,z_cut,z_cut_space),"",n_events,start_event_n);
            totalRate[n_canvas*n_plots + n_plot] = (TH1F*)gROOT->FindObject(Form("h_totalRate_%d",n_canvas*n_plots + n_plot));
            norm_rate_weight_all[n_canvas*n_plots + n_plot] = totalRate[n_canvas*n_plots + n_plot]->Integral();
            Printf("Normalization total rate weight = %f",norm_rate_weight_all[n_canvas*n_plots + n_plot]);

            // Radial
            if (radialCut == 1){
              T->Draw(Form("hit.r[0]>>h_r_inside_%d",n_canvas*n_plots + n_plot),Form("rate*(hit.det[0] == %d && part.tjz > %f-%f && part.tjz < %f+%f && sqrt((part.tjx*part.tjx)+(part.tjy*part.tjy)) < %f)",detector,z_cut,z_cut_space,z_cut,z_cut_space,r_outer_cut),"",n_events,start_event_n);
            }
            else {
            // X cut
              T->Draw(Form("hit.r[0]>>h_r_inside_%d",n_canvas*n_plots + n_plot),Form("rate*(hit.det[0] == %d && part.tjz > %f-%f && part.tjz < %f+%f && part.tjx < %f)",detector,z_cut,z_cut_space,z_cut,z_cut_space,r_outer_cut),"",n_events,start_event_n);
            }
            r_inside[n_canvas*n_plots + n_plot] = (TH1F*)gROOT->FindObject(Form("h_r_inside_%d",n_canvas*n_plots + n_plot));
            norm_rate_weight_in[n_canvas*n_plots + n_plot] = r_inside[n_canvas*n_plots + n_plot]->Integral();

            canvas[n_canvas]->cd(n_plot+2);
            // Radial
            if (radialCut == 1){
                T->Draw(Form("hit.r[0]>>h_r_outside_%d",n_canvas*n_plots + n_plot),Form("rate*(hit.det[0] == %d && part.tjz > %f-%f && part.tjz < %f+%f && sqrt((part.tjx*part.tjx)+(part.tjy*part.tjy)) > %f)",detector,z_cut,z_cut_space,z_cut,z_cut_space,r_outer_cut),"",n_events,start_event_n);
            // X cut
            }
            else {
                T->Draw(Form("hit.r[0]>>h_r_outside_%d",n_canvas*n_plots + n_plot),Form("rate*(hit.det[0] == %d && part.tjz > %f-%f && part.tjz < %f+%f && part.tjx > %f)",detector,z_cut,z_cut_space,z_cut,z_cut_space,r_outer_cut),"",n_events,start_event_n);
            }
            r_outside[n_canvas*n_plots + n_plot] = (TH1F*)gROOT->FindObject(Form("h_r_outside_%d",n_canvas*n_plots + n_plot));
            norm_rate_weight_out[n_canvas*n_plots + n_plot] = r_outside[n_canvas*n_plots + n_plot]->Integral();

            canvas[n_canvas]->cd(n_plot+3);
            // Radial
            if (radialCut == 1){
                T->Draw(Form("hit.y[0]:hit.x[0]>>h_xy_inside_%d",n_canvas*n_plots + n_plot),Form("rate*(hit.det[0] == %d && part.tjz > %f-%f && part.tjz < %f+%f && sqrt((part.tjx*part.tjx)+(part.tjy*part.tjy)) < %f)",detector,z_cut,z_cut_space,z_cut,z_cut_space,r_outer_cut),"",n_events,start_event_n);
            }
            else {
            // X cut
                T->Draw(Form("hit.y[0]:hit.x[0]>>h_xy_inside_%d",n_canvas*n_plots + n_plot),Form("rate*(hit.det[0] == %d && part.tjz > %f-%f && part.tjz < %f+%f && part.tjx < %f)",detector,z_cut,z_cut_space,z_cut,z_cut_space,r_outer_cut),"",n_events,start_event_n);
            }
            xy_inside[n_canvas*n_plots + n_plot] = (TH2F*)gROOT->FindObject(Form("h_xy_inside_%d",n_canvas*n_plots + n_plot));

            canvas[n_canvas]->cd(n_plot+4);
            // Radial
            if (radialCut == 1){
                T->Draw(Form("hit.y[0]:hit.x[0]>>h_xy_outside_%d",n_canvas*n_plots + n_plot),Form("rate*(hit.det[0] == %d && part.tjz > %f-%f && part.tjz < %f+%f && sqrt((part.tjx*part.tjx)+(part.tjy*part.tjy)) > %f)",detector,z_cut,z_cut_space,z_cut,z_cut_space,r_outer_cut),"",n_events,start_event_n);
            }
            // X cut
            else {
                T->Draw(Form("hit.y[0]:hit.x[0]>>h_xy_outside_%d",n_canvas*n_plots + n_plot),Form("rate*(hit.det[0] == %d && part.tjz > %f-%f && part.tjz < %f+%f && part.tjx > %f)",detector,z_cut,z_cut_space,z_cut,z_cut_space,r_outer_cut),"",n_events,start_event_n);
            }
            xy_outside[n_canvas*n_plots + n_plot] = (TH2F*)gROOT->FindObject(Form("h_xy_outside_%d",n_canvas*n_plots + n_plot));

            // CAMGUIN feature: writeFile_h takes the name of the variable, the value to set it to, the "run number" which is a unique identifier, the split number (which is another unique identifier), and the number of runs (another one)
            //writeFile_h("r_outer_cut", r_outer_cut, r_outer_cut, z_cut, detector*1.0);
            //writeFile_h("z_cut", z_cut, r_outer_cut, z_cut, detector*1.0);
            //writeFile_h("detector", detector*1.0, r_outer_cut, z_cut, detector*1.0);
            //writeFile_h(Form("%s_norm_rate_weight_all",type.Data()), norm_rate_weight_all[n_canvas*n_plots + n_plot], r_outer_cut, z_cut, detector*1.0);
            //writeFile_h(Form("%s_norm_rate_weight_in",type.Data()), norm_rate_weight_in[n_canvas*n_plots + n_plot], r_outer_cut, z_cut, detector*1.0);
            //writeFile_h(Form("%s_norm_rate_weight_out",type.Data()), norm_rate_weight_out[n_canvas*n_plots + n_plot], r_outer_cut, z_cut, detector*1.0);

            if(fp!=NULL){
                fprintf(fp,"r_outer_cut = %f, z_cut = %f, detector = %d, %s_norm_rate_weight_all = %f, %s_norm_rate_weight_in = %f, %s_norm_rate_weight_out = %f\n",r_outer_cut,z_cut,detector,type.Data(),norm_rate_weight_all[n_canvas*n_plots + n_plot],type.Data(),norm_rate_weight_in[n_canvas*n_plots + n_plot],type.Data(),norm_rate_weight_out[n_canvas*n_plots + n_plot]);
            }
        }
        if (n_canvas == 0 && n_steps != 1){
            canvas[n_canvas]->SaveAs(Form("%s_norm_rate_weight.pdf(",type.Data()));
        }
        else if (n_canvas < n_steps || n_steps==1) {
            canvas[n_canvas]->SaveAs(Form("%s_norm_rate_weight.pdf",type.Data()));
        }
        else {
            canvas[n_canvas]->SaveAs(Form("%s_norm_rate_weight.pdf)",type.Data()));
        }
    }

    fclose(fp);
}
