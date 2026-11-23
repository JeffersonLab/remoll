/* 	
    This file provides utility functions to load lookup tables for electron position and energy response, 
    and to calculate the mean photoelectron (PE) yield based on hit position and energy for the thin quartz detectors.

    
    - There are five functions you need to use to calculate the PE yield:
        1. LoadLookupTable_electron_position: Loads the position-based lookup table from a CSV file.
        2. LoadLookupTable_electron_energy: Loads the energy-based lookup table from a CSV file.
        3. GetMeanPE(this is just for testing): Returns the mean PE yield for a given hit position (h, v) using the position lookup table. 
        4. cal_PE: Calculates the PE yield for a given hit position (h, v), polar angle (theta), and azimuthal angle (phi) using the position lookup table.
        5. cal_PE_energy: Returns the energy scaling factor for a given hit energy using the energy lookup table.
        6. loadPositionTables: Loads all position-based lookup tables for rings 1-6 from a specified directory into a vector
        7. loadEnergyTables: Loads all energy-based lookup tables for rings 1-6 from a specified directory into a vector

    - There are two more optional function if one wants to use interpolation instead of direct lookup:
        6. MakePositionLookupMatrix: Converts the position lookup table vector into a 2D matrix for interpolation.
        7. cal_PE_interp: Calculates the PE yield using bilinear interpolation based on the position lookup matrix.

    How to use:
    -----------
    1. Include this file in your ROOT macro or C++ analysis script.
    2. load the lookup tables for the desired ring number:
           std::vector<tableEntry> lookup = LoadLookupTable_electron_position(Form("table/r%d_LookupTable.csv", ringnumber));
           std::vector<tableEntry_energy> lookup_energy = LoadLookupTable_electron_energy(Form("table/r%d_EnergyDepTable.csv", ringnumber));
    3. For each hit (of type RemollHit), call:
           double pe = cal_PE(hit.xl, hit.yl, theta, phi, lookup);
           double energy_scale = cal_PE_energy(hit.e, lookup_energy);
    4. Use the returned values for further analysis or histogramming.       

    How to use interpolation:
    -----------------------
    1. After loading the position lookup table, convert it to a matrix:
           std::vector<std::vector<tableEntry>> lookupMatrix = MakePositionLookupMatrix(lookup);
    2. For each hit, call:
           double pe_interp = cal_PE_interp(hit.xl, hit.yl, theta, phi, lookupMatrix);
    3. Use the returned value for further analysis or histogramming.

    All functions are documented below.
*/

#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <iostream>
#include <stdexcept>
#include <sys/stat.h>


// function prototypes
std::vector<std::vector<tableEntry>> LoadPositionTables(std::string tabledir);
std::vector<std::vector<tableEntry_energy>> LoadEnergyTables(std::string tabledir);
std::vector<tableEntry> LoadLookupTable_electron_position(const std::string& filename);
std::vector<std::vector<tableEntry>> MakePositionLookupMatrix(const std::vector<tableEntry>& lookupTable);
std::vector<tableEntry_energy> LoadLookupTable_electron_energy(const std::string& filename);
double GetMeanPE(double h, double v,vector<tableEntry>& lookupTable);
double cal_PE(double h, double v,double theta,double phi,vector<tableEntry>& lookupTable);
double cal_PE_interp(double h, double v, double theta, double phi, vector<vector<tableEntry>>& lookupMatrix);
double cal_PE_energy(double energy,vector<tableEntry_energy>& lookupTable);

// Structure to hold position-based lookup table entries
struct tableEntry {
    double hmin, hmax;
    double vmin, vmax;
    double pn0, pn1, pn2; // negative theta parameters
    double pt0, pt1, pt2; // positive theta parameters
};

// Structure to hold energy-based lookup table entries
struct tableEntry_energy {
    double energy, scale;
};

// Load a position lookup table for each ring and fill a vector
std::vector<std::vector<tableEntry>> LoadPositionTables(std::string tabledir){
    std::vector<std::vector<tableEntry>> tables;
    struct stat sb;
    for(int nring = 1; nring <= 6; nring++){
        std::string file = Form("%s/CorrectedMeanAndFits_r%d_LookupTable.csv", tabledir.c_str(), nring);
        if(stat(file.c_str(), &sb) != 0)
            throw std::runtime_error("Missing position lookup table: " + file);

        std::vector<tableEntry> lookupTable = LoadLookupTable_electron_position(file);
        if(lookupTable.empty())
            throw std::runtime_error("Position lookup table is empty: " + file);
        tables.push_back(lookupTable);
    }
    return tables;
}

// Load a position lookup table for each ring and fill a vector
std::vector<std::vector<tableEntry_energy>> LoadEnergyTables(std::string tabledir){
    std::vector<std::vector<tableEntry_energy>> tables;
    struct stat sb;
    for(int nring = 1; nring <= 6; nring++){
        std::string file = Form("%s/r%d_EnergyDepTable.csv", tabledir.c_str(), nring);
        if(stat(file.c_str(), &sb) != 0)
            throw std::runtime_error("Missing energy lookup table: " + file);

        std::vector<tableEntry_energy> lookupTable = LoadLookupTable_electron_energy(file);
        if(lookupTable.empty())
            throw std::runtime_error("Energy lookup table is empty: " + file);
        tables.push_back(lookupTable);
    }
    return tables;
}

// Function to load position-based lookup table from CSV file
std::vector<tableEntry> LoadLookupTable_electron_position(const std::string& filename) {

    std::vector<tableEntry> lookupTable;
    lookupTable.clear();
    std::ifstream file(filename);
    std::string line;
    getline(file, line); // skip header
    while (std::getline(file, line)) {
        std::stringstream ss(line);
        tableEntry e;
        std::string tmp;
        std::getline(ss, tmp, ','); e.hmin = atof(tmp.c_str());
        std::getline(ss, tmp, ','); e.hmax = atof(tmp.c_str());
        std::getline(ss, tmp, ','); e.vmin = atof(tmp.c_str());
        std::getline(ss, tmp, ','); e.vmax = atof(tmp.c_str());
        std::getline(ss, tmp, ','); e.pn0 = atof(tmp.c_str());
        std::getline(ss, tmp, ','); e.pn1 = atof(tmp.c_str());
        std::getline(ss, tmp, ','); e.pn2 = atof(tmp.c_str());
        std::getline(ss, tmp, ','); e.pt0 = atof(tmp.c_str());
        std::getline(ss, tmp, ','); e.pt1 = atof(tmp.c_str());
        std::getline(ss, tmp, ','); e.pt2 = atof(tmp.c_str());
        lookupTable.push_back(e);
    }
    std::cout << "Loaded " << lookupTable.size() << " position table entries.\n";
    return lookupTable;
}

// convert 1D vector of positions to 2D matrix for interpolation
std::vector<std::vector<tableEntry>> MakePositionLookupMatrix(const std::vector<tableEntry>& lookupTable) {
    std::vector<std::vector<tableEntry>> matrix;
    for (const auto& entry : lookupTable) {
        if (matrix.empty() ||
            matrix.back().front().hmin != entry.hmin ||
            matrix.back().front().hmax != entry.hmax) {
            matrix.emplace_back();
        }
        matrix.back().push_back(entry);
    }
    return matrix;
}

// Function to load energy-based lookup table from CSV file
std::vector<tableEntry_energy> LoadLookupTable_electron_energy(const std::string& filename) {
    std::vector<tableEntry_energy> lookupTable;
    lookupTable.clear();
    std::ifstream file(filename);
    std::string line;
    getline(file, line); // skip header
    while (std::getline(file, line)) {
        std::stringstream ss(line);
        tableEntry_energy e;
        std::string tmp;
        std::getline(ss, tmp, ','); e.energy = atof(tmp.c_str());
        std::getline(ss, tmp, ','); e.scale = atof(tmp.c_str());
        lookupTable.push_back(e);
    }
    std::cout << "Loaded " << lookupTable.size() << " Energy table entries.\n";
    return lookupTable;
}

// We now return pn0 as meanPE since we don't have explicit meanPe anymore
double GetMeanPE(double h, double v,vector<tableEntry>& lookupTable) {
    for (auto& e : lookupTable) {
        if (h >= e.hmin && h < e.hmax && v >= e.vmin && v < e.vmax)
            return e.pn0;
    }
    return -1; // not found
}

double cal_PE(double h, double v,double theta,double phi,vector<tableEntry>& lookupTable) {
    
    // PE lookup table valid only for local theta range [-4, 4] degrees
    if(theta < -4) theta = -4;
    if(theta > 4) theta = 4;

    for (auto& e : lookupTable) {
        if (h > e.hmin-1e-9 && h < e.hmax+1e-9 && v >= e.vmin-1e-9 && v < e.vmax+1e-9) {
            double theta1 = theta;
            double phi1 = TMath::DegToRad() * phi;
            double px = e.pn0;
            double py;
            // different polynomial fits for negative and positive theta
            if(theta <= 0){
                py = e.pn0+e.pn1*theta1+e.pn2*theta1*theta1;
            } else {
                py = e.pt0+e.pt1*theta1+e.pt2*theta1;
            }
            if(py == 0){
                return 0;
            }
            double PE = px*py/sqrt(pow(px*sin(phi1),2)+pow(py*cos(phi1),2));
            return PE;
        }
    }
    return -1; // not found if position is not in the table, return -1 not to be used for further analysis
}

//  Function to return the energy scaling factor for a given hit energy using the energy lookup table
double cal_PE_energy(double energy,vector<tableEntry_energy>& lookupTable) {
    for (auto& e : lookupTable) {
        if (energy >= e.energy-1 && energy < e.energy+1) {
            return e.scale;
        }
    }
    return 1; // not found
}

// can be balled identically to cal_PE but with bilinear interpolation between adjacent bins
// is a little slower because of the extra calculations, but more accurate
double cal_PE_interp(double h, double v, double theta, double phi, vector<vector<tableEntry>>& lookupMatrix){
    
    if(theta <= -4) theta = -4;
    if(theta >= 4) theta = 4;

    auto calculatePE = [theta, phi](double p0, double p1, double p2) {
        
        const double phiRad = TMath::DegToRad() * phi;
        const double py = p0 + p1 * theta + p2 * theta * theta;
        if (py == 0) {
            return 0.0;
        }
        return p0 * py / std::sqrt(std::pow(p0 * std::sin(phiRad), 2) +
                                   std::pow(py * std::cos(phiRad), 2));
    };

    // find adjancent bins, returns true if on an edge in given direction 
    auto find_adjacent_bins = [](const std::vector<double>& centers, double coord, int& lower, int& upper) {
        if (coord < centers.front() || coord > centers.back()) {
            return true;
        }
        for (int i = 0; i + 1 < centers.size(); ++i) {
            if (coord <= centers[i + 1]) {
                lower = i;
                upper = i + 1;
                return false;
            }
        }
        return true;
    };

    std::vector<double> hCenters;
    for (const auto& col : lookupMatrix) {
        hCenters.push_back((col[0].hmin + col[0].hmax) / 2.0);
    }
    std::vector<double> vCenters;
    for (const auto& entry : lookupMatrix[0]) {
        vCenters.push_back((entry.vmin + entry.vmax) / 2.0);
    }

    int hLower = 0, hUpper = 0, vLower = 0, vUpper = 0;

    const bool h_edge = find_adjacent_bins(hCenters, h, hLower, hUpper);
    const bool v_edge = find_adjacent_bins(vCenters, v, vLower, vUpper);

    if (h_edge || v_edge){
        for (const auto& col : lookupMatrix) {
            for (const auto& e : col) {
                if (h >= e.hmin - 1e-9 && h <= e.hmax + 1e-9 && v >= e.vmin - 1e-9 && v <= e.vmax + 1e-9) {
                    if (theta <= 0) {
                        return calculatePE(e.pn0, e.pn1, e.pn2);
                    } else {
                        return calculatePE(e.pt0, e.pt1, e.pt2);
                    }
                }
            }
        }
        return -1;
    }

    const tableEntry& lowerLeft = lookupMatrix[hLower][vLower];
    const tableEntry& lowerRight = lookupMatrix[hUpper][vLower];
    const tableEntry& upperLeft = lookupMatrix[hLower][vUpper];
    const tableEntry& upperRight = lookupMatrix[hUpper][vUpper];
    
    const double y1 = vCenters[vLower], y2 = vCenters[vUpper], x1 = hCenters[hLower], x2 = hCenters[hUpper];
    // bilinear interpolation from https://en.wikipedia.org/wiki/Bilinear_interpolation

    auto interpolate = [y1, y2, x1, x2, h, v](double pe00, double pe10, double pe01, double pe11) {
        return ((x2 - h)*((y2 - v) * pe00 + (v - y1) * pe01) + (h - x1)*((y2 - v) * pe10 + (v - y1) * pe11)) / ((x2 - x1) * (y2 - y1));
    };

    double pe00, pe01, pe10, pe11;

    if (theta <= 0){
        pe00 = calculatePE(lowerLeft.pn0, lowerLeft.pn1, lowerLeft.pn2);
        pe01 = calculatePE(lowerRight.pn0, lowerRight.pn1, lowerRight.pn2);
        pe10 = calculatePE(upperLeft.pn0, upperLeft.pn1, upperLeft.pn2);
        pe11 = calculatePE(upperRight.pn0, upperRight.pn1, upperRight.pn2);
    } else {
        pe00 = calculatePE(lowerLeft.pt0, lowerLeft.pt1, lowerLeft.pt2);
        pe01 = calculatePE(lowerRight.pt0, lowerRight.pt1, lowerRight.pt2);
        pe10 = calculatePE(upperLeft.pt0, upperLeft.pt1, upperLeft.pt2);
        pe11 = calculatePE(upperRight.pt0, upperRight.pt1, upperRight.pt2);
    }
    
    return interpolate(pe00, pe01, pe10, pe11);
}