/* 	
    This file provides utility functions to load lookup tables for electron position and energy response, 
    and to calculate the mean photoelectron (PE) yield based on hit position and energy for the thin quartz detectors.

    
    - There are five functions you need to use to calculate the PE yield:
        1. LoadLookupTable_electron_position: Loads the position-based lookup table from a CSV file.
        2. LoadLookupTable_electron_energy: Loads the energy-based lookup table from a CSV file.
        3. GetMeanPE(this is just for testing): Returns the mean PE yield for a given hit position (h, v) using the position lookup table. 
        4. cal_PE: Calculates the PE yield for a given hit position (h, v), polar angle (theta), and azimuthal angle (phi) using the position lookup table.
        5. cal_PE_energy: Returns the energy scaling factor for a given hit energy using the energy lookup table.
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

    All functions are documented below.
*/
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <iostream>

// Structure to hold position-based lookup table entries
struct tableEntry {
    double hmin, hmax;
    double vmin, vmax;
    double meanPE;
    double langauPE;
    double RMS, Resolution;
    double p0, p1, p2; // fit parameters
};
// Structure to hold energy-based lookup table entries
struct tableEntry_energy {
    double energy, scale;
};

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
        std::getline(ss, tmp, ','); e.meanPE = atof(tmp.c_str());
        std::getline(ss, tmp, ','); e.langauPE = atof(tmp.c_str());
        std::getline(ss, tmp, ','); e.RMS = atof(tmp.c_str());
        std::getline(ss, tmp, ','); e.Resolution = atof(tmp.c_str());
        std::getline(ss, tmp, ','); e.p0 = atof(tmp.c_str());
        std::getline(ss, tmp, ','); e.p1 = atof(tmp.c_str());
        std::getline(ss, tmp, ','); e.p2 = atof(tmp.c_str());
        lookupTable.push_back(e);
    }
    std::cout << "Loaded " << lookupTable.size() << " New table entries.\n";
    return lookupTable;
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

// Function to get mean PE yield for a given hit position (h, v) using the position lookup table
double GetMeanPE(double h, double v,vector<tableEntry>& lookupTable) {
    for (auto& e : lookupTable) {
        if (h >= e.hmin && h < e.hmax && v >= e.vmin && v < e.vmax)
            return e.meanPE;
    }
    return -1; // not found
}
// Function to calculate PE yield for a given hit position (h, v), polar angle (theta), and azimuthal angle (phi) using the position lookup table
double cal_PE(double h, double v,double theta,double phi,vector<tableEntry>& lookupTable) {
    for (auto& e : lookupTable) {
        if (h > e.hmin-1e-9 && h < e.hmax+1e-9 && v >= e.vmin-1e-9 && v < e.vmax+1e-9) {
            double theta1 = theta;
            double phi1 = TMath::DegToRad() * phi;
            double px = e.p0;
            double py = e.p0+e.p1*theta1+e.p2*theta1*theta1;
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
    return 1; // not found if energy is not in the table, return 1 (no scaling)
}