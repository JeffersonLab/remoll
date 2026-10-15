#include <TFile.h>
#include <TTree.h>
#include <TH1D.h>
#include <TCanvas.h>
#include <TLegend.h>
#include <TString.h>
#include <TStyle.h>

#include <iostream>
#include <vector>
#include <string>
#include <map>

void plot_generators()
{
    TString hist1DWeight = "PE*rate/998/1E9/7 * (energy>1)";
    TString hist2DWeight = "A*PE*rate/998/1E9/7 * (energy>1)";

    // ============================================================
    // Input files
    // ============================================================

    TFile *fElastic   = TFile::Open("../new_file_elastic.root");
    TFile *fInelastic = TFile::Open("../new_file_inelastic.root");
    TFile *fMoller    = TFile::Open("../new_file_moller.root");

    if (!fElastic || fElastic->IsZombie() ||
        !fInelastic || fInelastic->IsZombie() ||
        !fMoller || fMoller->IsZombie()) {

        std::cerr << "Error opening one or more ROOT files."
                  << std::endl;
        return;
    }

    // ============================================================
    // Get TTrees
    // ============================================================

    TTree *tElastic   = (TTree*)fElastic->Get("T");
    TTree *tInelastic = (TTree*)fInelastic->Get("T");
    TTree *tMoller    = (TTree*)fMoller->Get("T");

    if (!tElastic || !tInelastic || !tMoller) {
        std::cerr << "Error: could not find TTree 'T'."
                  << std::endl;
        return;
    }

    // ============================================================
    // Histogram configuration
    //
    // branch : {number of bins, minimum, maximum}
    //
    // Modify these values to suit your physics distributions.
    // ============================================================

    struct HistConfig {
        int    nBins;
        double xmin;
        double xmax;
    };

    std::map<std::string, HistConfig> config = {

        // Branch       bins       min       max
        {"det",        {200,       80000,    81000}},
        {"pid",        {100,       0.0,     50.0}},
        {"trid",       {100,       0.0,     10000.0}},

        {"energy",     {200,       0.0,     4000.0}},

        {"x",          {270,    -1260.0,    -990.0}},
        {"y",          {320,    -160.0,     160.0}},

        {"px",         {200,    -300.0,      50.0}},
        {"py",         {200,     -50.0,         50.0}},
        {"pz",         {200,       0.0,     4000.0}},

        {"p",          {200,       0.0,     4000.0}},

        {"A",          {1000,    -50.0,      5.0}},
        {"Am",         {1000,    -50.0,      5.0}}

        // "rate" is intentionally omitted because it is
        // being used as the event weight.
    };


    // ============================================================
    // Branches to plot
    // ============================================================

    std::vector<std::string> branches = {
        "det",
        "pid",
        //"trid",
        "energy",
        "x",
        "y",
        "px",
        "py",
        "pz",
        "p",
        "A",
        "Am"
    };


    // ============================================================
    // Loop over branches
    // ============================================================

    for (const auto &branch : branches) {

        std::cout << "Processing " << branch << std::endl;

        // --------------------------------------------------------
        // Check that configuration exists
        // --------------------------------------------------------

        if (config.find(branch) == config.end()) {

            std::cerr
                << "Warning: no histogram configuration for "
                << branch << std::endl;

            continue;
        }

        HistConfig cfg = config[branch];

        // --------------------------------------------------------
        // Create histograms
        // --------------------------------------------------------

        TH1D *hElastic = new TH1D(
            Form("hElastic_%s", branch.c_str()),
            "",
            cfg.nBins,
            cfg.xmin,
            cfg.xmax
        );

        TH1D *hInelastic = new TH1D(
            Form("hInelastic_%s", branch.c_str()),
            "",
            cfg.nBins,
            cfg.xmin,
            cfg.xmax
        );

        TH1D *hMoller = new TH1D(
            Form("hMoller_%s", branch.c_str()),
            "",
            cfg.nBins,
            cfg.xmin,
            cfg.xmax
        );

        // --------------------------------------------------------
        // Fill histograms
        //
        // "rate" is the weight.
        //
        // Each event contributes rate [Hz] to its bin.
        // --------------------------------------------------------

        tElastic->Draw(
            Form("%s>>hElastic_%s",
                 branch.c_str(),
                 branch.c_str()),
            Form("%s", hist1DWeight.Data()),
            "goff"
        );

        tInelastic->Draw(
            Form("%s>>hInelastic_%s",
                 branch.c_str(),
                 branch.c_str()),
            Form("%s", hist1DWeight.Data()),
            "goff"
        );

        tMoller->Draw(
            Form("%s>>hMoller_%s",
                 branch.c_str(),
                 branch.c_str()),
            Form("%s", hist1DWeight.Data()),
            "goff"
        );


        // ========================================================
        // Total histogram
        // ========================================================

        TH1D *hSum = (TH1D*)hElastic->Clone(
            Form("hSum_%s", branch.c_str())
        );

        hSum->Add(hInelastic);
        hSum->Add(hMoller);


        // ========================================================
        // Calculate weighted statistics
        // ========================================================

        double meanElastic = hElastic->GetMean();
        double stdElastic  = hElastic->GetStdDev();

        double meanInelastic = hInelastic->GetMean();
        double stdInelastic  = hInelastic->GetStdDev();

        double meanMoller = hMoller->GetMean();
        double stdMoller  = hMoller->GetStdDev();

        double meanSum = hSum->GetMean();
        double stdSum  = hSum->GetStdDev();

        double rateElastic   = hElastic->Integral();
        double rateInelastic = hInelastic->Integral();
        double rateMoller    = hMoller->Integral();
        double rateSum       = hSum->Integral();


        // ========================================================
        // Histogram appearance
        // ========================================================

        hElastic->SetLineColor(kRed);
        hElastic->SetLineWidth(2);

        hInelastic->SetLineColor(kBlue);
        hInelastic->SetLineWidth(2);

        hMoller->SetLineColor(kGreen + 2);
        hMoller->SetLineWidth(2);

        hSum->SetLineColor(kBlack);
        hSum->SetLineWidth(3);
        hSum->SetStats(0);

        hElastic->SetFillStyle(0);
        hInelastic->SetFillStyle(0);
        hMoller->SetFillStyle(0);
        hSum->SetFillStyle(0);


        // ========================================================
        // Canvas
        // ========================================================

        TCanvas *c = new TCanvas(
            Form("c_%s", branch.c_str()),
            branch.c_str(),
            1000,
            750
        );

        c->SetLeftMargin(0.12);
        c->SetRightMargin(0.05);
        c->SetBottomMargin(0.12);
        c->SetTopMargin(0.05);


        // ========================================================
        // Axis titles
        // ========================================================

        hSum->SetTitle(
            Form("%s; %s; PE #times 10^{9}/s",
                 branch.c_str(),
                 branch.c_str())
        );

        hSum->Draw("hist");
        hElastic->Draw("hist same");
        hInelastic->Draw("hist same");
        hMoller->Draw("hist same");


        // ========================================================
        // Legend
        // ========================================================

        TLegend *legend = new TLegend(
            0.52,
            0.55,
            0.93,
            0.90
        );

        legend->SetBorderSize(0);
        legend->SetFillStyle(0);
        legend->SetTextSize(0.025);

        legend->AddEntry(
            hElastic,
            Form("Elastic: #mu = %.3g, #sigma = %.3g",
                 meanElastic,
                 stdElastic),
            "l"
        );

        legend->AddEntry(
            hInelastic,
            Form("Inelastic: #mu = %.3g, #sigma = %.3g",
                 meanInelastic,
                 stdInelastic),
            "l"
        );

        legend->AddEntry(
            hMoller,
            Form("Moller: #mu = %.3g, #sigma = %.3g",
                 meanMoller,
                 stdMoller),
            "l"
        );

        legend->AddEntry(
            hSum,
            Form("Total: #mu = %.3g, #sigma = %.3g",
                 meanSum,
                 stdSum),
            "l"
        );

        legend->Draw();


        // ========================================================
        // Save PNG
        // ========================================================

        c->SaveAs(
            Form("%s.png", branch.c_str())
        );


        // ========================================================
        // Console output
        // ========================================================

        std::cout << "  Elastic:"
                  << " mean = " << meanElastic
                  << ", sigma = " << stdElastic
                  << std::endl;

        std::cout << "  Inelastic:"
                  << " mean = " << meanInelastic
                  << ", sigma = " << stdInelastic
                  << std::endl;

        std::cout << "  Moller:"
                  << " mean = " << meanMoller
                  << ", sigma = " << stdMoller
                  << std::endl;

        std::cout << "  Total:"
                  << " mean = " << meanSum
                  << ", sigma = " << stdSum
                  << std::endl;

        std::cout << std::endl;


        // ========================================================
        // Cleanup
        // ========================================================

        delete legend;
        delete c;

        delete hElastic;
        delete hInelastic;
        delete hMoller;
        delete hSum;
    }

    // ============================================================
    // 2D y:x asymmetry-response-rate-weighted histograms
    // ============================================================

    int nBinsX = config["x"].nBins/10;
    double xMin = config["x"].xmin;
    double xMax = config["x"].xmax;

    int nBinsY = config["y"].nBins/10;
    double yMin = config["y"].xmin;
    double yMax = config["y"].xmax;


    // ------------------------------------------------------------
    // Create 2D histograms
    // ------------------------------------------------------------

    TH2D *hXY_Elastic = new TH2D(
        "hXY_Elastic",
        "Elastic (A x Rate x Response);x [mm];y [mm]",
        nBinsX, xMin, xMax,
        nBinsY, yMin, yMax
    );

    TH2D *hXY_Inelastic = new TH2D(
        "hXY_Inelastic",
        "Inelastic (A x Rate x Response);x [mm];y [mm]",
        nBinsX, xMin, xMax,
        nBinsY, yMin, yMax
    );

    TH2D *hXY_Moller = new TH2D(
        "hXY_Moller",
        "Moller (A x Rate x Response);x [mm];y [mm]",
        nBinsX, xMin, xMax,
        nBinsY, yMin, yMax
    );


    // ------------------------------------------------------------
    // Fill histograms
    // ------------------------------------------------------------

    tElastic->Draw(
        "y:x>>hXY_Elastic",
        Form("%s", hist2DWeight.Data()),
        "goff"
    );

    tInelastic->Draw(
        "y:x>>hXY_Inelastic",
        Form("%s", hist2DWeight.Data()),
        "goff"
    );

    tMoller->Draw(
        "y:x>>hXY_Moller",
        Form("%s", hist2DWeight.Data()),
        "goff"
    );


    // ------------------------------------------------------------
    // Create total histogram
    // ------------------------------------------------------------

    TH2D *hXY_Sum = (TH2D*)hXY_Elastic->Clone(
        "hXY_Sum"
    );

    hXY_Sum->SetTitle(
        "Total (A x Rate x Response);x [mm];y [mm]"
    );

    hXY_Sum->Add(hXY_Inelastic);
    hXY_Sum->Add(hXY_Moller);


    // ------------------------------------------------------------
    // Print total rates
    // ------------------------------------------------------------

    std::cout << "2D XY rates:" << std::endl;

    std::cout << "  Elastic   = "
              << hXY_Elastic->Integral()
              << " asymmetry x PEe+09/s" << std::endl;

    std::cout << "  Inelastic = "
              << hXY_Inelastic->Integral()
              << " asymmetry x PEe+09/s" << std::endl;

    std::cout << "  Moller    = "
              << hXY_Moller->Integral()
              << " asymmetry x PEe+09/s" << std::endl;

    std::cout << "  Total     = "
              << hXY_Sum->Integral()
              << " asymmetry x PEe+09/s" << std::endl;


    // ============================================================
    // Plot Elastic
    // ============================================================

    TCanvas *cXY_Elastic = new TCanvas(
        "cXY_Elastic",
        "Elastic XY",
        900,
        750
    );

    cXY_Elastic->SetRightMargin(0.15);

    hXY_Elastic->SetStats(0);
    hXY_Elastic->Draw("COLZ");

    cXY_Elastic->SaveAs(
        "xy_elastic.png"
    );


    // ============================================================
    // Plot Inelastic
    // ============================================================

    TCanvas *cXY_Inelastic = new TCanvas(
        "cXY_Inelastic",
        "Inelastic XY",
        900,
        750
    );

    cXY_Inelastic->SetRightMargin(0.15);

    hXY_Inelastic->SetStats(0);
    hXY_Inelastic->Draw("COLZ");

    cXY_Inelastic->SaveAs(
        "xy_inelastic.png"
    );


    // ============================================================
    // Plot Moller
    // ============================================================

    TCanvas *cXY_Moller = new TCanvas(
        "cXY_Moller",
        "Moller XY",
        900,
        750
    );

    cXY_Moller->SetRightMargin(0.15);

    hXY_Moller->SetStats(0);
    hXY_Moller->Draw("COLZ");

    cXY_Moller->SaveAs(
        "xy_moller.png"
    );


    // ============================================================
    // Plot Total
    // ============================================================

    TCanvas *cXY_Sum = new TCanvas(
        "cXY_Sum",
        "Total XY",
        900,
        750
    );

    cXY_Sum->SetRightMargin(0.15);

    hXY_Sum->SetStats(0);
    hXY_Sum->Draw("COLZ");

    cXY_Sum->SaveAs(
        "xy_total.png"
    );


    // ============================================================
    // Cleanup 2D objects
    // ============================================================

    delete cXY_Elastic;
    delete cXY_Inelastic;
    delete cXY_Moller;
    delete cXY_Sum;

    delete hXY_Elastic;
    delete hXY_Inelastic;
    delete hXY_Moller;
    delete hXY_Sum;

    // ============================================================
    // Close files
    // ============================================================

    fElastic->Close();
    fInelastic->Close();
    fMoller->Close();

    delete fElastic;
    delete fInelastic;
    delete fMoller;

    std::cout << "Finished." << std::endl;
}