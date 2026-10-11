#ifndef REMOLL_TO_QSIM_HH
#define REMOLL_TO_QSIM_HH

#include <cmath>
#include <vector>
#include <TMath.h>

// Constants for the qsim system
const double RADIUS_CENTER = 1100.0; // mm, radial distance from remoll center to the center of qsim planes
const double QSIM_PLANE_WIDTH = 160.0;  // mm, radial dimension of qsim planes
const double QSIM_PLANE_HEIGHT = 265.0; // mm, azimuthal straight dimension of qsim planes
const int N_PLANES = 28;                // Number of qsim planes

// Function to calculate the azimuthal angle for each qsim plane
inline std::vector<double> CalculateQsimPlaneAngles() {
    std::vector<double> planeAngles;
    double deltaPhi = 2 * TMath::Pi() / N_PLANES;
    for (int i = 0; i < N_PLANES; ++i) {
        planeAngles.push_back(i * deltaPhi);
    }
    return planeAngles;
}

// Function to convert remoll (x, y) coordinates to qsim plane coordinates
inline std::pair<double, double> ConvertRemollToQsim(double x_remoll, double y_remoll) {
    // Calculate the polar coordinates (r, phi) in the remoll system
    double r_remoll = std::sqrt(x_remoll * x_remoll + y_remoll * y_remoll);
    double phi_remoll = std::atan2(y_remoll, x_remoll); // Angle in radians

    // Adjust 

    // Adjust phi_remoll to be in the range [0, 2pi)
    if (phi_remoll < 0) phi_remoll += 2 * TMath::Pi();

    // Calculate qsim plane angles
    std::vector<double> qsimAngles = CalculateQsimPlaneAngles();

    // Determine the closest qsim plane
    int closestPlane = 0;
    double minDeltaPhi = TMath::Pi(); // Initialize to the largest possible value

    for (size_t i = 0; i < qsimAngles.size(); ++i) {
        double deltaPhi = std::fabs(phi_remoll - qsimAngles[i]);
        deltaPhi = std::min(deltaPhi, 2 * TMath::Pi() - deltaPhi); // Account for circular nature

        if (deltaPhi < minDeltaPhi) {
            minDeltaPhi = deltaPhi;
            closestPlane = i;
        }
    }

    // Calculate the local coordinates on the closest qsim plane
    double planeAngle = qsimAngles[closestPlane];
    double x_plane_center = RADIUS_CENTER * std::cos(planeAngle);
    double y_plane_center = RADIUS_CENTER * std::sin(planeAngle);

    // Transform remoll coordinates to the local coordinate system of the qsim plane
    double x_local = (x_remoll - x_plane_center) * std::cos(planeAngle) + (y_remoll - y_plane_center) * std::sin(planeAngle);
    double y_local = - (x_remoll - x_plane_center) * std::sin(planeAngle) + (y_remoll - y_plane_center) * std::cos(planeAngle);

    // Output the results
    // std::cout << "Remoll coordinates: (" << x_remoll << ", " << y_remoll << ")\n";
    // std::cout << "Closest qsim plane: " << closestPlane + 1 << "\n";
    // std::cout << "Qsim local coordinates on plane " << closestPlane + 1 << ": (" << x_local << ", " << y_local << ")\n";

    return {x_local, y_local};
}

#endif // REMOLL_TO_QSIM_HH

