#ifndef SHOWER_MAX_RESPONSE_LOOKUP_H
#define SHOWER_MAX_RESPONSE_LOOKUP_H

#include <fstream>
#include <iostream>
#include <system_error>
#include <vector>
#include <string>
#include <sstream>
#include <map>
#include <memory>
#include <utility>
#include <stdexcept>

#include "RtypesCore.h"
#include "TF2.h"

# include "remollToQsim.hh"

namespace ShowermaxLookup{


/* ============================================================
   Particle definitions
   ============================================================ */

constexpr int nParticles = 8;

// PID convention (you may adjust to match Geant4 if needed)
inline const int particlePID[nParticles] = {
    11,     // e-
    -11,    // e+
    22,     // gamma
    13,     // mu-
    -13,    // mu+
    211,    // pi-
    -211,   // pi+
    2112    // neutron
};

inline const char* particleName[nParticles] = {
    "e-",
    "e+",
    "gamma",
    "mu-",
    "mu+",
    "pi-",
    "pi+",
    "neutron"
};

/* ============================================================
   Energy grid
   ============================================================ */

inline const Double_t energyList[] = {
    5, 10, 50, 100, 500,
    1000, 2000, 3000, 4000,
    5000, 6000, 7000, 8000, 9000
};

constexpr int nEnergies =
    sizeof(energyList) / sizeof(energyList[0]);

/* ============================================================
   Utility: PID → particle index
   ============================================================ */

inline int getParticleIndexFromPid(int pid)
{
    for (int i = 0; i < nParticles; ++i) {
        if (particlePID[i] == pid)
            return i;
    }
    throw std::runtime_error("Unsupported particle PID");
}

/* ============================================================
   Utility: energy bounds
   ============================================================ */

inline std::pair<Double_t, Double_t>
getEnergyBounds(Double_t energy)
{
    Double_t low  = energyList[0];
    Double_t high = energyList[nEnergies - 1];

    for (int i = 0; i < nEnergies - 1; ++i) {
        if (energy >= energyList[i] &&
            energy <  energyList[i + 1]) {
            low  = energyList[i];
            high = energyList[i + 1];
            break;
        }
    }
    return {low, high};
}

/* ============================================================
   CSV loader (called once per particle)
   ============================================================ */

inline std::vector<std::vector<Double_t>>
load_fit_data(const std::string& particle)
{
    std::string filename =
        Form("./data/fit_param_xy_%s_ifarm.csv",
             particle.c_str());

    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Cannot open file: " + filename);
    }

    std::vector<std::vector<Double_t>> data;
    std::string line;

    // Skip header
    std::getline(file, line);

    while (std::getline(file, line)) {
        if (line.empty() || line[0] == '#') continue;

        std::stringstream ss(line);
        std::string token;
        std::vector<Double_t> row;

        while (std::getline(ss, token, ',')) {
            row.push_back(std::stod(token));
        }
        data.push_back(row);
    }
    return data;
}

/* ============================================================
   Per-particle TF2 cache
   ============================================================ */

class ShowerMaxParticleCache
{
public:
    explicit ShowerMaxParticleCache(const std::string& particle)
        : fit_data_(load_fit_data(particle))
    {}

    const TF2* getFit(Double_t energy) const
    {
        auto it = fit_cache_.find(energy);
        if (it != fit_cache_.end())
            return it->second.get();

        return build_fit(energy);
    }

private:
    const TF2* build_fit(Double_t energy) const
    {
        static const char* formula =
            "[0]*([1] + x*[2]) + [3]*([4] + y*[5] + y*y*[6])";

        auto fit = std::make_unique<TF2>(
            Form("shmax_%g", energy),
            formula
        );

        constexpr Double_t xWeight = 0.5;
        constexpr Double_t yWeight = 0.5;

        for (const auto& row : fit_data_) {
            if (row[0] == energy) {
                fit->SetParameters(
                    xWeight,
                    row[1], row[2],
                    yWeight,
                    row[3], row[4], row[5]
                );
                break;
            }
        }

        auto* ptr = fit.get();
        fit_cache_[energy] = std::move(fit);
        return ptr;
    }

private:
    std::vector<std::vector<Double_t>> fit_data_;
    mutable std::map<Double_t, std::unique_ptr<TF2>> fit_cache_;
};

/* ============================================================
   Global cache manager (one per particle)
   ============================================================ */

inline ShowerMaxParticleCache&
getParticleCache(int pid)
{
    static std::map<int, ShowerMaxParticleCache> caches;

    auto it = caches.find(pid);
    if (it != caches.end())
        return it->second;

    int idx = getParticleIndexFromPid(pid);
    caches.emplace(
        pid,
        ShowerMaxParticleCache(particleName[idx])
    );

    return caches.at(pid);
}

/* ============================================================
   Public API — safe for hit loops
   ============================================================ */

inline Double_t
getPeResponseWithoutLP(
    int      pid,
    Double_t energy,
    Double_t x,
    Double_t y
)
{
    std::pair<double, double> qsimxy = ConvertRemollToQsim(x,y);

    auto& cache = getParticleCache(pid);
    auto [eLow, eHigh] = getEnergyBounds(energy);

    const TF2* fLow  = cache.getFit(eLow);
    const TF2* fHigh = cache.getFit(eHigh);

    const Double_t vLow  = fLow->Eval(qsimxy.first, qsimxy.second);
    const Double_t vHigh = fHigh->Eval(qsimxy.first, qsimxy.second);

    Double_t value = vLow + (vHigh - vLow)
    * (energy - eLow) / (eHigh - eLow);

    if (value < 0.0) value = 0.0;
    return value;
}

/*============================================================
Return long-pass preserve factor based on angular sector
Return values:
    open sector        : 0.22 with 450 nm LP filter
    closed/transition  : 0.33 with 425 nm LP filter
============================================================*/
inline Double_t getLongPassPreserveFactor(Double_t x, Double_t y) {
    constexpr double PI = M_PI;
    std::cout << "Pi = " << PI << std::endl;

    Double_t phi = atan2(y, x);
    std::cout << "phi = " << phi * 180 / PI << std::endl;

    // Convert to [0, 2*pi)
    if (phi < 0.0)
        phi += 2.0 * PI;

    // ---------------------------------------------------------
    // Sector definitions
    // ---------------------------------------------------------
    constexpr Int_t    nSectors    = 28;
    constexpr Double_t sectorWidth = 2.0 * PI / nSectors;
    constexpr Double_t startPhi    = -sectorWidth / 2.0;

    // Shift phi relative to sector start
    Double_t shiftedPhi = phi + startPhi;

    std::cout << "shiftedPhi = " << shiftedPhi * 180 / PI << std::endl;

    // Wrap into [0, 2*pi)
    while (shiftedPhi < 0.0)
        shiftedPhi += 2.0 * PI;

    while (shiftedPhi >= 2.0 * PI)
        shiftedPhi -= 2.0 * PI;

    // Determine sector index: 0 -> 27
    Int_t sector = static_cast<int>(shiftedPhi / sectorWidth);
    
    std::cout << "sector = " << sector << std::endl;

    // ---------------------------------------------------------
    // Pattern: 0 is closed, 1 is transition, 2 is open and 3 is transition
    // ---------------------------------------------------------
    Int_t pattern = sector % 4;

    std::cout << "pattern = " << pattern << std::endl;

    // Open sector
    if (pattern == 2)
        return 0.22;

    // Closed + transition sectors
    return 0.33;
}

/*============================================================
 * Return long-pass preserve factor based on angular sectors (stackplanes det Num)
 * Return values: 0.22 for open sector, 0.33 for closed/transition sector
============================================================*/
inline Double_t calc_LP_preserve(Int_t detNum)
{
    // Expected detector range: 73001 - 73028 (28 stackplanes)
    constexpr Int_t detMin = 73001;
    constexpr Int_t detMax = 73028;

    // Validate detector number
    if (detNum < detMin || detNum > detMax) {
        std::cerr << "Invalid detector number: " << detNum << std::endl;
        return -1.0;
    }

    // Convert to sector index: 1 -> 28
    Int_t sector = detNum - 73000;
    std::cout << "sector = " << sector << std::endl;

    // Sector pattern:
    //   1 : closed
    //   2 : transition
    //   3 : open
    //   4 : transition
    // repeated every 4 sectors
    //
    // Open sectors preserve 22% with 450 nm LP filter
    // Closed + transition sectors preserve 33% with 425 nm LP filter

    Double_t LP_preserve = (sector % 4 == 3) ? 0.22 : 0.33;
    std::cout << "LP_preserve = " << LP_preserve << std::endl;

    return LP_preserve;
}

/* ============================================================
   Public API — safe for hit loops
   ============================================================ */

inline Double_t getPeResponse(Int_t detNum, Int_t pid, Double_t energy, Double_t x, Double_t y) {
    std::pair<double, double> qsimxy = ConvertRemollToQsim(x,y);
    // Double_t lpFactor = getLongPassPreserveFactor(x, y);
    Double_t lpFactor = calc_LP_preserve(detNum);

    auto& cache = getParticleCache(pid);
    auto [eLow, eHigh] = getEnergyBounds(energy);

    const TF2* fLow  = cache.getFit(eLow);
    const TF2* fHigh = cache.getFit(eHigh);

    const Double_t vLow  = fLow->Eval(qsimxy.first, qsimxy.second);
    const Double_t vHigh = fHigh->Eval(qsimxy.first, qsimxy.second);

    Double_t value = vLow + (vHigh - vLow) * (energy - eLow) / (eHigh - eLow);
    Double_t valueLP = value * lpFactor;

    if (valueLP < 0.0) valueLP = 0.0;
    return valueLP;
}
};
#endif  // SHOWER_MAX_RESPONSE_LOOKUP_H

