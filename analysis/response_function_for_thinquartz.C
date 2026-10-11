/*
  This file generate PE yield based on the lookup table for the thin quartz detectors.
  It will generate different kinds of histograms for the PE yield, asymmetry, and theta distribution for the thin quartz detectors.
  for new the polar angle bigger than 4 degree is not in the lookup table, so we will use the polar angle 4 degree to calculate the PE yield for the polar angle bigger than 4 degree.
  
  The mean function is response_function_for_thinquartz() which will generate the histograms for the PE yield, asymmetry, and theta distribution for the thin quartz detectors.
  and to calculate the mean photoelectron (PE) yield based on hit position and energy for the thin quartz detectors.
  ---Important Note---
  You need to include the lookuptable.C and convert_labframe_to_local_thinquartz.C in your ROOT macro or C++ analysis script before using this function.
  you also need to have the lookup table for the thinquartz detectors from Jon

  How to use:
  -----------
  1. Include this file in your ROOT macro or C++ analysis script.
  2. Call the function response_function_for_thinquartz() with the desired file list and ring number:
         response_function_for_thinquartz(filelist, ringnumber);
  3. The function will generate histograms for the PE yield, asymmetry, and theta distribution for the thin quartz detectors, and save them to a ROOT file.
  4. The function will also calculate the mean photoelectron (PE) yield based on hit position and energy for the thin quartz detectors, and print the results to the console.

  All functions are documented below.
*/

#include "lookuptable.C"
#include "convert_labframe_to_local_thinquartz.C"
#include "utils.hh"

#include <cmath>
#include <iostream>
#include <map>
#include <mutex>
#include <string>
#include <vector>

#include <ROOT/RDataFrame.hxx>
#include <ROOT/TThreadedObject.hxx>

#include <TCanvas.h>
#include <TGraph.h>
#include <TH1D.h>
#include <TTree.h>

// -----------------------------------------------------------------------------
// Detector geometry
// -----------------------------------------------------------------------------
//
// Height correction for each detector ring.
//
// The local y coordinate returned by
// convert_labframe_to_local_thinquartz() does not use exactly the same
// coordinate definition as the PE lookup table. Therefore, this offset must
// be applied before querying the lookup table.
//
static const std::map<int, double> detector_height = {
    {1, 15.0},
    {2, 30.0},
    {3, 30.0},
    {4, 60.0},
    {5, 70.0},
    {6, 50.0}
};


// Extract the ring number from the detector ID.
//
// Example:
//     detector ID = ...5...
//                       ^
//                   ring number
//
static inline int GetRing(const RemollHit& hit)
{
    return (hit.det / 10) % 10;
}


// -----------------------------------------------------------------------------
// Main analysis
// -----------------------------------------------------------------------------

void response_function_for_thinquartz(
    const std::string& filelist =
        "/Users/mr-right/simana/sim/output/moller_newmap_2025100214/filelist.txt",
    int ringnumber = 5)
{
    // -------------------------------------------------------------------------
    // Input files
    // -------------------------------------------------------------------------

    const auto files = readlines(filelist);

    if (files.empty()) {
        std::cerr
            << "Error: file list is empty or cannot be read: "
            << filelist << "\n";
        return;
    }

    // Check that a height correction exists for the requested ring.
    const auto height_it = detector_height.find(ringnumber);

    if (height_it == detector_height.end()) {
        std::cerr
            << "Error: no detector-height correction defined for ring "
            << ringnumber << "\n";
        return;
    }

    const double height_correction = height_it->second;


    // -------------------------------------------------------------------------
    // ROOT RDataFrame setup
    // -------------------------------------------------------------------------

    // Enable ROOT implicit multi-threading.
    //
    // TThreadedObject is used below so that each thread gets its own histogram.
    // The histograms are merged after Foreach() finishes.
    ROOT::EnableImplicitMT();

    ROOT::RDataFrame df("T", files);

    auto count = df.Count();

    std::cout
        << "Total number of entries: "
        << *count << "\n";


    // -------------------------------------------------------------------------
    // PE lookup tables
    // -------------------------------------------------------------------------

    // Position/angle-dependent PE lookup table.
    std::vector<tableEntry> lookup =
        LoadLookupTable_electron_position(
            Form("table/r%d_NewLookupTable.csv", ringnumber)
        );

    // Energy-dependent correction to the PE response.
    std::vector<tableEntry_energy> lookup_energy =
        LoadLookupTable_electron_energy(
            Form("table/r%d_EnergyDepTable.csv", ringnumber)
        );


    // -------------------------------------------------------------------------
    // Histograms
    // -------------------------------------------------------------------------

    ROOT::TThreadedObject<TH1D> hAsym_PE(
        "hAsym_PE",
        Form(
            "Asymmetry weighted by PE #times rate in ring %d;"
            "Asymmetry;"
            "Weighted counts",
            ringnumber
        ),
        100, -45, 0
    );

    ROOT::TThreadedObject<TH1D> hAsym_rate(
        "hAsym_rate",
        Form(
            "Asymmetry weighted by rate in ring %d;"
            "Asymmetry;"
            "Weighted counts",
            ringnumber
        ),
        100, -45, 0
    );

    ROOT::TThreadedObject<TH1D> hThCom_PE(
        "hThCom_PE",
        Form(
            "#theta_{COM} weighted by PE #times rate in ring %d;"
            "#theta_{COM} (rad);"
            "Weighted counts",
            ringnumber
        ),
        200, 0.2, 2.8
    );

    ROOT::TThreadedObject<TH1D> hThCom_rate(
        "hThCom_rate",
        Form(
            "#theta_{COM} weighted by rate in ring %d;"
            "#theta_{COM} (rad);"
            "Weighted counts",
            ringnumber
        ),
        200, 0.2, 2.8
    );

    ROOT::TThreadedObject<TH1D> hPE(
        "hPE",
        Form(
            "PE distribution in ring %d weighted by rate;"
            "PE;"
            "Weighted counts",
            ringnumber
        ),
        200, 0, 45
    );

    ROOT::TThreadedObject<TH1D> hPE_event(
        "hPE_event",
        Form(
            "Total PE per event in ring %d weighted by rate;"
            "Total PE;"
            "Weighted counts",
            ringnumber
        ),
        200, 0, 90
    );

    ROOT::TThreadedObject<TH1D> hEnergyMissed(
        "hEnergyMissed",
        Form(
            "Energy distribution for hits with #theta_{local} > 4 rad "
            "in ring %d;"
            "Energy (MeV);"
            "Weighted counts",
            ringnumber
        ),
        1000, 0, 100
    );


    // -------------------------------------------------------------------------
    // Shared quantities
    // -------------------------------------------------------------------------

    // Used to calculate:
    //
    //       sum(rate * PE)
    // <PE> = --------------
    //          sum(rate)
    //
    double sum_rate_pe = 0.0;
    double sum_rate = 0.0;

    // Count how many accepted hits have local polar angle > 4 rad.
    double missed_count = 0.0;
    double total_count = 0.0;

    // Used for the Asymmetry vs theta_COM scatter plot.
    std::vector<double> theta_com_values;
    std::vector<double> asymmetry_values;

    // These variables are shared between RDataFrame worker threads.
    std::mutex sum_mutex;
    std::mutex graph_mutex;


    // -------------------------------------------------------------------------
    // Event loop
    // -------------------------------------------------------------------------

    df.Foreach(
        [&](const hit_list& hits, double rate, const remollEvent_t& ev)
        {
            double total_pe = 0.0;

            for (const auto& hit : hits) {

                // -------------------------------------------------------------
                // Hit selection
                // -------------------------------------------------------------
                //
                // Require:
                //   1. hit belongs to requested ring
                //   2. electron or positron
                //   3. particle is moving in +z direction
                //   4. kinetic energy > 1.1 MeV
                //
                if (GetRing(hit) != ringnumber ||
                    std::abs(hit.pid) != 11 ||
                    hit.pz <= 0.0 ||
                    hit.k <= 1.1) {
                    continue;
                }


                // -------------------------------------------------------------
                // Convert hit to local quartz coordinates
                // -------------------------------------------------------------

                const mainquartz_local_info info =
                    convert_labframe_to_local_thinquartz(hit);

                const bool polar_outside_table =
                    (info.lpolar > 4.0);


                // -------------------------------------------------------------
                // PE calculation
                // -------------------------------------------------------------

                // Correct the PE according to deposited/incident energy.
                const double energy_factor =
                    cal_PE_energy(hit.k, lookup_energy);

                // The PE lookup table only extends to theta_local = 4 rad.
                //
                // Hits above 4 rad are therefore evaluated using the
                // theta = 4 rad boundary value.
                const double lookup_polar =
                    polar_outside_table ? 4.0 : info.lpolar;

                const double position_pe =
                    cal_PE(
                        hit.xl,
                        hit.yl + height_correction,
                        lookup_polar,
                        info.lazimuthal,
                        lookup
                    );

                const double pe =
                    energy_factor * position_pe;


                // Study the energy distribution of hits whose polar angle
                // lies outside the lookup-table range.
                if (polar_outside_table) {
                    hEnergyMissed.Get()->Fill(hit.k, rate);
                }


                // -------------------------------------------------------------
                // Check PE and event weight
                // -------------------------------------------------------------

                const double pe_rate_weight = pe * rate;

                if (!std::isfinite(pe_rate_weight) ||
                    !std::isfinite(rate)) {

                    std::cout
                        << "Non-finite value:"
                        << " PE = " << pe
                        << ", rate = " << rate
                        << "\n";

                    continue;
                }


                // -------------------------------------------------------------
                // Fill hit-level histograms
                // -------------------------------------------------------------

                if (pe > 0.0) {

                    hPE.Get()->Fill(pe, rate);

                    hAsym_PE.Get()->Fill(
                        ev.A,
                        pe_rate_weight
                    );

                    hAsym_rate.Get()->Fill(
                        ev.A,
                        rate
                    );

                    hThCom_PE.Get()->Fill(
                        ev.thcom,
                        pe_rate_weight
                    );

                    hThCom_rate.Get()->Fill(
                        ev.thcom,
                        rate
                    );

                } else {

                    std::cout
                        << "Warning: PE <= 0"
                        << ", PE = " << pe
                        << ", x = " << hit.xl
                        << ", y = " << hit.yl
                        << ", polar = " << info.lpolar
                        << ", on wedge = " << info.is_wedge
                        << "\n";
                }


                // -------------------------------------------------------------
                // Event PE
                // -------------------------------------------------------------

                total_pe += pe;


                // -------------------------------------------------------------
                // Update shared statistics
                // -------------------------------------------------------------

                {
                    std::lock_guard<std::mutex> lock(sum_mutex);

                    sum_rate_pe += rate * pe;
                    sum_rate += rate;

                    ++total_count;

                    if (polar_outside_table) {
                        ++missed_count;
                    }
                }
            }


            // -----------------------------------------------------------------
            // Event-level PE histogram
            // -----------------------------------------------------------------

            if (total_pe > 0.0) {
                hPE_event.Get()->Fill(total_pe, rate);
            }


            // -----------------------------------------------------------------
            // Store event kinematics for scatter plot
            // -----------------------------------------------------------------
            //
            // These vectors are shared between threads, so writes must be
            // protected by a mutex.
            //
            {
                std::lock_guard<std::mutex> lock(graph_mutex);

                theta_com_values.push_back(ev.thcom);
                asymmetry_values.push_back(ev.A);
            }
        },
        {"hit", "rate", "ev"}
    );


    // -------------------------------------------------------------------------
    // Print summary
    // -------------------------------------------------------------------------

    if (sum_rate > 0.0) {
        std::cout
            << "Average PE yield (rate weighted): "
            << sum_rate_pe / sum_rate
            << "\n";
    }

    std::cout
        << "Hits with polar angle > 4 rad: "
        << missed_count
        << " / "
        << total_count
        << "\n";

    if (total_count > 0.0) {
        std::cout
            << "Fraction with polar angle > 4 rad: "
            << missed_count / total_count
            << "\n";
    }


    // -------------------------------------------------------------------------
    // Merge thread-local histograms
    // -------------------------------------------------------------------------

    auto hAsym_PE_m     = hAsym_PE.Merge();
    auto hAsym_rate_m   = hAsym_rate.Merge();

    auto hThCom_PE_m    = hThCom_PE.Merge();
    auto hThCom_rate_m  = hThCom_rate.Merge();

    auto hPE_m           = hPE.Merge();
    auto hPE_event_m     = hPE_event.Merge();

    auto hEnergyMissed_m = hEnergyMissed.Merge();


    // -------------------------------------------------------------------------
    // Asymmetry vs theta_COM graph
    // -------------------------------------------------------------------------

    TGraph* gr = new TGraph(
        theta_com_values.size(),
        theta_com_values.data(),
        asymmetry_values.data()
    );

    gr->SetTitle(
        Form(
            "Asymmetry vs #theta_{COM} (ring %d);"
            "#theta_{COM} (rad);"
            "Asymmetry",
            ringnumber
        )
    );


    // -------------------------------------------------------------------------
    // Draw results
    // -------------------------------------------------------------------------

    TCanvas* c1 =
        new TCanvas("c1", "Asymmetry Response", 1200, 800);
    c1->Divide(2, 1);
    c1->cd(1);
    hAsym_PE_m->SetLineColor(kRed);
    hAsym_PE_m->DrawCopy();
    c1->cd(2);
    hAsym_rate_m->SetLineColor(kBlue);
    hAsym_rate_m->DrawCopy();
    //c1->BuildLegend();
    

    TCanvas* c2 =
        new TCanvas("c2", "Theta COM Response", 1200, 800);
    c2->Divide(2, 1);
    c2->cd(1);
    hThCom_PE_m->SetLineColor(kRed);
    hThCom_PE_m->DrawCopy();
    c2->cd(2);
    hThCom_rate_m->SetLineColor(kBlue);
    hThCom_rate_m->DrawCopy();
    //c2->BuildLegend();


    TCanvas* c3 =
        new TCanvas("c3", "PE Distribution", 1200, 800);
    hPE_m->SetMarkerSize(2);
    hPE_m->DrawClone();


    TCanvas* c4 =
        new TCanvas("c4", "Event PE Distribution", 1200, 800);
    hPE_event_m->SetMarkerSize(2);
    hPE_event_m->DrawClone();


    TCanvas* c5 =
        new TCanvas(
            "c5",
            "Energy of Hits Outside PE Angular Range",
            1200,
            800
        );

    hEnergyMissed_m->DrawClone();


    TCanvas* c6 =
        new TCanvas(
            "c6",
            "Asymmetry vs Theta COM",
            1200,
            800
        );

    gr->Draw("AP");
}