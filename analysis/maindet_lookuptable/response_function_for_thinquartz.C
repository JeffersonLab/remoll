/*
 * Thin quartz PE response analysis.
 *
 * Calculates PE yield using position/angle and energy lookup tables.
 * Produces histograms for PE yield, asymmetry, theta_COM, and hits
 * outside the lookup-table angular range.
 *
 * Notes:
 *   - Requires lookuptable.C and convert_labframe_to_local_thinquartz.C.
 *   - info.lpolar is in degrees.
 *   - For local polar angles > 4 degrees, the PE lookup uses 4 degrees.
 *   - A ring-dependent y offset is applied before the position lookup.
 *
 * Usage:
 *   reroot response_function_for_thinquartz(filelist, ringnumber);
 */

#include "lookuptable.C"
#include "convert_labframe_to_local_thinquartz.C"
//#include "utils.hh"

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


// Detector height correction for each ring.
static const std::map<int, double> detector_height = {
    {1, 15.0},
    {2, 30.0},
    {3, 30.0},
    {4, 60.0},
    {5, 70.0},
    {6, 50.0}
};


// Extract ring number from detector ID.
static inline int GetRing(const RemollHit& hit)
{
    return (hit.det / 10) % 10;
}


void response_function_for_thinquartz(
    const std::string& filelist =
        "/Users/mr-right/simana/sim/output/moller_newmap_2025100214/filelist.txt",
    int ringnumber = 5)
{
    // Input
    const auto files = readlines(filelist);

    if (files.empty()) {
        std::cerr << "Error: cannot read file list: "
                  << filelist << '\n';
        return;
    }

    const auto height_it = detector_height.find(ringnumber); //find the height correction for the ring

    if (height_it == detector_height.end()) {
        std::cerr << "Error: no detector-height correction for ring "
                  << ringnumber << '\n';
        return;
    }

    const double height_correction = height_it->second; // Get the height correction for the specified ring


    // ROOT setup
    ROOT::EnableImplicitMT();
    ROOT::RDataFrame df("T", files);

    auto count = df.Count();

    std::cout << "Total number of entries: "
              << *count << '\n';


    // load PE lookup tables
    auto lookup = LoadLookupTable_electron_position(
        Form("table/r%d_NewLookupTable.csv", ringnumber));

    auto lookup_energy = LoadLookupTable_electron_energy(
        Form("table/r%d_EnergyDepTable.csv", ringnumber));


    // Histograms
    ROOT::TThreadedObject<TH1D> hAsym_PE(
        "hAsym_PE",
        Form("Asymmetry weighted by PE #times rate in ring %d;"
             "Asymmetry;Weighted counts", ringnumber),
        100, -45, 0);

    ROOT::TThreadedObject<TH1D> hAsym_rate(
        "hAsym_rate",
        Form("Asymmetry weighted by rate in ring %d;"
             "Asymmetry;Weighted counts", ringnumber),
        100, -45, 0);

    ROOT::TThreadedObject<TH1D> hThCom_PE(
        "hThCom_PE",
        Form("#theta_{COM} weighted by PE #times rate in ring %d;"
             "#theta_{COM} (rad);Weighted counts", ringnumber),
        200, 0.2, 2.8);

    ROOT::TThreadedObject<TH1D> hThCom_rate(
        "hThCom_rate",
        Form("#theta_{COM} weighted by rate in ring %d;"
             "#theta_{COM} (rad);Weighted counts", ringnumber),
        200, 0.2, 2.8);

    ROOT::TThreadedObject<TH1D> hPE(
        "hPE",
        Form("PE distribution in ring %d weighted by rate;"
             "PE;Weighted counts", ringnumber),
        200, 0, 45);

    ROOT::TThreadedObject<TH1D> hPE_event(
        "hPE_event",
        Form("Total PE per event in ring %d weighted by rate;"
             "Total PE;Weighted counts", ringnumber),
        200, 0, 90);

    ROOT::TThreadedObject<TH1D> hEnergyMissed(
        "hEnergyMissed",
        Form("Energy distribution for hits with #theta_{local} > 4 deg "
             "in ring %d;Energy (MeV);Weighted counts", ringnumber),
        1000, 0, 100);


    // Rate-weighted mean PE = sum(rate * PE) / sum(rate)
    double sum_rate_pe = 0.0;
    double sum_rate = 0.0;

    double missed_count = 0.0;
    double total_count = 0.0;

    std::vector<double> theta_com_values;
    std::vector<double> asymmetry_values;

    std::mutex sum_mutex;
    std::mutex graph_mutex;


    // Event loop
    df.Foreach(
        [&](const hit_list& hits, double rate, const remollEvent_t& ev)
        {
            double total_pe = 0.0;

            for (const auto& hit : hits) {

                // Forward-going electrons/positrons above threshold.
                if (GetRing(hit) != ringnumber ||
                    std::abs(hit.pid) != 11 ||
                    hit.pz <= 0.0 ||
                    hit.k <= 1.1) {
                    continue;
                }

                const mainquartz_local_info info =
                    convert_labframe_to_local_thinquartz(hit);

                // info.lpolar is in degrees.
                const bool polar_outside_table = (info.lpolar > 4.0);

                const double energy_factor =
                    cal_PE_energy(hit.k, lookup_energy);

                // Lookup table stops at 4 degrees.
                const double lookup_polar =
                    std::min(info.lpolar, 4.0);

                const double position_pe = cal_PE(
                    hit.xl,
                    hit.yl + height_correction,
                    lookup_polar,
                    info.lazimuthal,
                    lookup);

                const double pe =
                    energy_factor * position_pe;

                if (polar_outside_table) {
                    hEnergyMissed.Get()->Fill(hit.k, rate);
                }

                const double pe_rate_weight = pe * rate;

                if (!std::isfinite(pe_rate_weight) ||
                    !std::isfinite(rate)) {

                    std::cout << "Non-finite value:"
                              << " PE = " << pe
                              << ", rate = " << rate
                              << '\n';

                    continue;
                }

                if (pe > 0.0) {
                    hPE.Get()->Fill(pe, rate);

                    hAsym_PE.Get()->Fill(
                        ev.A,
                        pe_rate_weight);

                    hAsym_rate.Get()->Fill(
                        ev.A,
                        rate);

                    hThCom_PE.Get()->Fill(
                        ev.thcom,
                        pe_rate_weight);

                    hThCom_rate.Get()->Fill(
                        ev.thcom,
                        rate);

                } else {
                    std::cout << "Warning: PE <= 0"
                              << ", PE = " << pe
                              << ", x = " << hit.xl
                              << ", y = " << hit.yl
                              << ", polar = " << info.lpolar
                              << ", on wedge = " << info.is_wedge
                              << '\n';
                }

                total_pe += pe;

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

            // Event-level PE
            if (total_pe > 0.0) {
                hPE_event.Get()->Fill(total_pe, rate);
            }

            // Store event kinematics for scatter plot.
            {
                std::lock_guard<std::mutex> lock(graph_mutex);

                theta_com_values.push_back(ev.thcom);
                asymmetry_values.push_back(ev.A);
            }
        },
        {"hit", "rate", "ev"}
    );


    // Summary
    if (sum_rate > 0.0) {
        std::cout << "Average PE yield (rate weighted): "
                  << sum_rate_pe / sum_rate
                  << '\n';
    }

    std::cout << "Hits with polar angle > 4 deg: "
              << missed_count
              << " / "
              << total_count
              << '\n';

    if (total_count > 0.0) {
        std::cout << "Fraction with polar angle > 4 deg: "
                  << missed_count / total_count
                  << '\n';
    }


    // Merge histograms
    auto hAsym_PE_m     = hAsym_PE.Merge();
    auto hAsym_rate_m   = hAsym_rate.Merge();
    auto hThCom_PE_m    = hThCom_PE.Merge();
    auto hThCom_rate_m  = hThCom_rate.Merge();
    auto hPE_m           = hPE.Merge();
    auto hPE_event_m     = hPE_event.Merge();
    auto hEnergyMissed_m = hEnergyMissed.Merge();


    // Asymmetry vs theta_COM
    auto* gr = new TGraph(
        theta_com_values.size(),
        theta_com_values.data(),
        asymmetry_values.data());

    gr->SetTitle(
        Form("Asymmetry vs #theta_{COM} (ring %d);"
             "#theta_{COM} (rad);Asymmetry",
             ringnumber));


    // Plots
    auto* c1 = new TCanvas(
        "c1", "Asymmetry Response", 1200, 800);

    c1->Divide(2, 1);

    c1->cd(1);
    hAsym_PE_m->SetLineColor(kRed);
    hAsym_PE_m->DrawCopy();

    c1->cd(2);
    hAsym_rate_m->SetLineColor(kBlue);
    hAsym_rate_m->DrawCopy();


    auto* c2 = new TCanvas(
        "c2", "Theta COM Response", 1200, 800);

    c2->Divide(2, 1);

    c2->cd(1);
    hThCom_PE_m->SetLineColor(kRed);
    hThCom_PE_m->DrawCopy();

    c2->cd(2);
    hThCom_rate_m->SetLineColor(kBlue);
    hThCom_rate_m->DrawCopy();


    auto* c3 = new TCanvas(
        "c3", "PE Distribution", 1200, 800);
    hPE_m->SetMarkerStyle(2);
    hPE_m->SetMarkerSize(2);
    hPE_m->DrawCopy();


    auto* c4 = new TCanvas(
        "c4", "Event PE Distribution", 1200, 800);
    hPE_event_m->SetMarkerStyle(2);
    hPE_event_m->SetMarkerSize(2);
    hPE_event_m->DrawCopy();


    auto* c5 = new TCanvas(
        "c5",
        "Energy of Hits Outside PE Angular Range",
        1200,
        800);

    hEnergyMissed_m->DrawCopy();


    auto* c6 = new TCanvas(
        "c6",
        "Asymmetry vs Theta COM",
        1200,
        800);

    gr->Draw("AP");
}