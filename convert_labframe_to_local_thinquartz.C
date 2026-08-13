/* 	
    This file provides utility functions to convert detector hit data from the lab frame
    to the local coordinate system of the main detectors (thin quartz).
    
    - The main function is `convert_labframe_to_local_thinquartz`, which takes a single hit
      and returns a `mainquartz_local_info` struct with local coordinates, rotated momentum,
      local angles, wedge information, and detector ID.
    - Several helper functions are provided for coordinate and cut calculations.
    - The detector geometry parameters are stored in `maindetector_detid_map`.
	---Important Note---
	This function can only be used for the thin quartz detectors which my naming system and the hits on the front plane or wedge plane of the detector!
	if you want to use it for other names or other detectors, please modify the map and the cut function accordingly.
	if you want to include the back face of the detector, please comment out the is_edge part in the main function.

    How to use:
    -----------
    1. Include this file in your ROOT macro or C++ analysis script.
    2. For each hit (of type RemollHit), call:
           mainquartz_local_info info = convert_labframe_to_local_thinquartz(hit);
    3. Use the returned struct for further analysis or histogramming.

    All functions are documented below.
*/
#include <string>
#include <vector>
#include <map>
#include <tuple>
#include <cmath>
#include <TVector3.h>
#include <TRotation.h>

bool equal(double a, double b, double epsilon = 1e-4) {
	return std::fabs(a - b) < epsilon;
}

// Type definitions for detector hit and hit list
typedef remollGenericDetectorHit_t RemollHit;
typedef std::vector<RemollHit> hit_list;

// Structure to hold processed hit information
struct mainquartz_local_info {
    double lpx;     // Rotated px
    double lpy;     // Rotated py
    double lpz;     // Rotated pz
    double lthx;    // Local theta x
    double lthy;    // Local theta y
    double lazimuthal;  // Local azimuthal angle momentum direction with respect to the beamline
	double lpolar;  // Local polar angle momentum direction in xy plane. 
	bool is_wedge; // Is hit on wedge 
	bool is_edge;  // Is hit on front face
    double detid;   // Detector ID
};

// Detector parameter map: key is detector ID, value is tuple of parameters
// you can change the parameters here to match your detector setup
// Currently we have angleX, angleZ, height, thickness
std::map <int,std::tuple<double,double,double,double,double>> maindetector_detid_map{
	{150151, {0.0000, -2.9976, 90.0000,70 ,8.5 }},
	{150153, {0.0000, -2.9976, 90.0000,70 ,8.5 }},
	{150152, {0.0000, -2.9976, 90.0000,70 ,8.5 }},
	{150140, {0.0000, -2.9971, 90.0000,60 ,10 }},
	{150130, {0.0000, -3.0022, 90.0000,30 ,10 }},
	{150120, {0.0000, -3.0022, 90.0000,30 ,10 }},
	{150110, {0.0000, -2.9923, 90.0000,5 ,10 }},
	{150160, {0.0000, -2.9997, 90.0000,50 ,10 }},
	{110152, {0.6687, -2.9257, 77.1193,70 ,8.5 }},
	{110153, {0.6687, -2.9256, 77.1193,70 ,8.5 }},
	{110151, {0.6688, -2.9259, 77.1193,70 ,8.5 }},
	{110140, {0.6686, -2.9259, 77.1224,60 ,10 }},
	{110130, {0.6672, -2.9197, 77.1229,30 ,10 }},
	{110120, {0.6672, -2.9198, 77.1230,30 ,10 }},
	{110110, {0.6692, -2.9282, 77.1209,5 ,10 }},
	{110160, {0.6683, -2.9245, 77.1226,50 ,10 }},
	{150251, {1.3015, -2.7005, 64.2563,70 ,8.5 }},
	{150253, {1.3015, -2.7004, 64.2563,70 ,8.5 }},
	{150252, {1.3015, -2.7005, 64.2563,70 ,8.5 }},
	{150240, {1.3015, -2.7001, 64.2536,60 ,10 }},
	{150230, {1.3035, -2.7046, 64.2551,30 ,10 }},
	{150220, {1.3035, -2.7047, 64.2560,30 ,10 }},
	{150210, {1.2992, -2.6956, 64.2550,5 ,10 }},
	{150260, {1.3025, -2.7026, 64.2557,50 ,10 }},
	{110252, {1.8723, -2.3461, 51.3913,70 ,8.5 }},
	{110253, {1.8726, -2.3456, 51.3813,70 ,8.5 }},
	{110251, {1.8724, -2.3459, 51.3869,70 ,8.5 }},
	{110240, {1.8724, -2.3459, 51.3871,60 ,10 }},
	{110230, {1.8685, -2.3410, 51.3873,30 ,10 }},
	{110220, {1.8688, -2.3414, 51.3875,30 ,10 }},
	{110210, {1.8739, -2.3478, 51.3876,5 ,10 }},
	{110260, {1.8715, -2.3448, 51.3872,50 ,10 }},
	{150351, {2.3444, -1.8683, 38.5321,70 ,8.5 }},
	{150353, {2.3443, -1.8686, 38.5366,70 ,8.5 }},
	{150352, {2.3443, -1.8686, 38.5366,70 ,8.5 }},
	{150340, {2.3441, -1.8683, 38.5346,60 ,10 }},
	{150330, {2.3481, -1.8714, 38.5339,30 ,10 }},
	{150320, {2.3481, -1.8714, 38.5340,30 ,10 }},
	{150310, {2.3402, -1.8651, 38.5333,5 ,10 }},
	{150360, {2.3462, -1.8698, 38.5331,50 ,10 }},
	{110352, {2.7046, -1.3013, 25.6759,70 ,8.5 }},
	{110353, {2.7045, -1.3016, 25.6823,70 ,8.5 }},
	{110351, {2.7045, -1.3016, 25.6823,70 ,8.5 }},
	{110340, {2.7044, -1.3015, 25.6814,60 ,10 }},
	{110330, {2.6989, -1.2988, 25.6808,30 ,10 }},
	{110320, {2.6989, -1.2989, 25.6813,30 ,10 }},
	{110310, {2.7072, -1.3028, 25.6806,5 ,10 }},
	{110360, {2.7033, -1.3010, 25.6809,50 ,10 }},
	{150451, {2.9226, -0.6667, 12.8396,70 ,8.5 }},
	{150453, {2.9223, -0.6670, 12.8466,70 ,8.5 }},
	{150452, {2.9226, -0.6666, 12.8380,70 ,8.5 }},
	{150440, {2.9219, -0.6666, 12.8409,60 ,10 }},
	{150430, {2.9269, -0.6679, 12.8431,30 ,10 }},
	{150420, {2.9273, -0.6678, 12.8398,30 ,10 }},
	{150410, {2.9173, -0.6656, 12.8410,5 ,10 }},
	{150460, {2.9248, -0.6672, 12.8390,50 ,10 }},
	{110452, {3.0011, 0.0000, 0.0000,70 ,8.5 }},
	{110453, {3.0013, 0.0000, 0.0000,70 ,8.5 }},
	{110451, {3.0011, 0.0000, 0.0000,70 ,8.5 }},
	{110440, {3.0012, 0.0001, -0.0027,60 ,10 }},
	{110430, {2.9951, 0.0000, 0.0000,30 ,10 }},
	{110420, {2.9951, 0.0000, 0.0000,30 ,10 }},
	{110410, {3.0038, 0.0002, -0.0034,5 ,10 }},
	{110460, {2.9997, 0.0000, 0.0000,50 ,10 }},
	{150551, {2.9226, 0.6667, -12.8396,70 ,8.5 }},
	{150553, {2.9226, 0.6666, -12.8380,70 ,8.5 }},
	{150552, {2.9225, 0.6666, -12.8381,70 ,8.5 }},
	{150540, {2.9222, 0.6667, -12.8409,60 ,10 }},
	{150530, {2.9271, 0.6678, -12.8401,30 ,10 }},
	{150520, {2.9270, 0.6676, -12.8367,30 ,10 }},
	{150510, {2.9173, 0.6655, -12.8384,5 ,10 }},
	{150560, {2.9248, 0.6672, -12.8390,50 ,10 }},
	{110552, {2.7043, 1.3017, -25.6854,70 ,8.5 }},
	{110553, {2.7043, 1.3017, -25.6854,70 ,8.5 }},
	{110551, {2.7044, 1.3020, -25.6888,70 ,8.5 }},
	{110540, {2.7044, 1.3017, -25.6850,60 ,10 }},
	{110530, {2.6989, 1.2991, -25.6848,30 ,10 }},
	{110520, {2.6988, 1.2991, -25.6855,30 ,10 }},
	{110510, {2.7066, 1.3027, -25.6837,5 ,10 }},
	{110560, {2.7033, 1.3012, -25.6838,50 ,10 }},
	{150651, {2.3446, 1.8682, -38.5265,70 ,8.5 }},
	{150653, {2.3443, 1.8686, -38.5366,70 ,8.5 }},
	{150652, {2.3445, 1.8684, -38.5321,70 ,8.5 }},
	{150640, {2.3441, 1.8682, -38.5325,60 ,10 }},
	{150630, {2.3481, 1.8713, -38.5320,30 ,10 }},
	{150620, {2.3482, 1.8713, -38.5315,30 ,10 }},
	{150610, {2.3402, 1.8651, -38.5333,5 ,10 }},
	{150660, {2.3462, 1.8698, -38.5313,50 ,10 }},
	{110652, {1.8723, 2.3458, -51.3869,70 ,8.5 }},
	{110653, {1.8722, 2.3461, -51.3913,70 ,8.5 }},
	{110651, {1.8721, 2.3460, -51.3913,70 ,8.5 }},
	{110640, {1.8721, 2.3460, -51.3926,60 ,10 }},
	{110630, {1.8684, 2.3413, -51.3915,30 ,10 }},
	{110620, {1.8684, 2.3413, -51.3920,30 ,10 }},
	{110610, {1.8737, 2.3480, -51.3923,5 ,10 }},
	{110660, {1.8713, 2.3450, -51.3921,50 ,10 }},
	{150751, {1.3015, 2.7004, -64.2563,70 ,8.5 }},
	{150753, {1.3018, 2.7004, -64.2498,70 ,8.5 }},
	{150752, {1.3015, 2.7006, -64.2563,70 ,8.5 }},
	{150740, {1.3015, 2.7000, -64.2524,60 ,10 }},
	{150730, {1.3038, 2.7048, -64.2511,30 ,10 }},
	{150720, {1.3038, 2.7050, -64.2531,30 ,10 }},
	{150710, {1.2992, 2.6953, -64.2535,5 ,10 }},
	{150760, {1.3026, 2.7024, -64.2528,50 ,10 }},
	{110752, {0.6683, 2.9260, -77.1279,70 ,8.5 }},
	{110753, {0.6683, 2.9260, -77.1279,70 ,8.5 }},
	{110751, {0.6684, 2.9259, -77.1263,70 ,8.5 }},
	{110740, {0.6683, 2.9260, -77.1276,60 ,10 }},
	{110730, {0.6670, 2.9202, -77.1287,30 ,10 }},
	{110720, {0.6669, 2.9203, -77.1292,30 ,10 }},
	{110710, {0.6692, 2.9288, -77.1242,5 ,10 }},
	{110760, {0.6681, 2.9246, -77.1269,50 ,10 }},
	{150851, {0.0000, 2.9976, -90.0000,70 ,8.5 }},
	{150853, {0.0000, 2.9974, -90.0000,70 ,8.5 }},
	{150852, {0.0000, 2.9976, -90.0000,70 ,8.5 }},
	{150840, {0.0001, 2.9971, -89.9973,60 ,10 }},
	{150830, {0.0002, 3.0022, -89.9970,30 ,10 }},
	{150820, {0.0000, 3.0022, -90.0000,30 ,10 }},
	{150810, {0.0000, 2.9917, -90.0000,5 ,10 }},
	{150860, {0.0001, 2.9997, -89.9978,50 ,10 }},
	{110852, {-0.6684, 2.9259, -102.8737,70 ,8.5 }},
	{110853, {-0.6686, 2.9257, -102.8791,70 ,8.5 }},
	{110851, {-0.6684, 2.9259, -102.8737,70 ,8.5 }},
	{110840, {-0.6685, 2.9259, -102.8750,60 ,10 }},
	{110830, {-0.6670, 2.9198, -102.8742,30 ,10 }},
	{110820, {-0.6672, 2.9201, -102.8770,30 ,10 }},
	{110810, {-0.6690, 2.9283, -102.8758,5 ,10 }},
	{110860, {-0.6682, 2.9246, -102.8752,50 ,10 }},
	{150951, {-1.3015, 2.7005, -115.7437,70 ,8.5 }},
	{150953, {-1.3016, 2.7004, -115.7468,70 ,8.5 }},
	{150952, {-1.3012, 2.7007, -115.7373,70 ,8.5 }},
	{150940, {-1.3013, 2.7002, -115.7428,60 ,10 }},
	{150930, {-1.3035, 2.7048, -115.7421,30 ,10 }},
	{150920, {-1.3035, 2.7047, -115.7426,30 ,10 }},
	{150910, {-1.2991, 2.6956, -115.7435,5 ,10 }},
	{150960, {-1.3025, 2.7026, -115.7433,50 ,10 }},
	{110952, {-1.8724, 2.3459, -128.6131,70 ,8.5 }},
	{110953, {-1.8722, 2.3461, -128.6087,70 ,8.5 }},
	{110951, {-1.8722, 2.3461, -128.6087,70 ,8.5 }},
	{110940, {-1.8724, 2.3461, -128.6112,60 ,10 }},
	{110930, {-1.8685, 2.3412, -128.6104,30 ,10 }},
	{110920, {-1.8685, 2.3413, -128.6100,30 ,10 }},
	{110910, {-1.8736, 2.3476, -128.6098,5 ,10 }},
	{110960, {-1.8714, 2.3449, -128.6093,50 ,10 }},
	{151051, {-2.3444, 1.8683, -141.4679,70 ,8.5 }},
	{151053, {-2.3443, 1.8686, -141.4634,70 ,8.5 }},
	{151052, {-2.3442, 1.8685, -141.4623,70 ,8.5 }},
	{151040, {-2.3439, 1.8683, -141.4637,60 ,10 }},
	{151030, {-2.3480, 1.8715, -141.4638,30 ,10 }},
	{151020, {-2.3481, 1.8714, -141.4660,30 ,10 }},
	{151010, {-2.3402, 1.8652, -141.4646,5 ,10 }},
	{151060, {-2.3461, 1.8700, -141.4638,50 ,10 }},
	{111052, {-2.7044, 1.3016, -154.3177,70 ,8.5 }},
	{111053, {-2.7045, 1.3016, -154.3177,70 ,8.5 }},
	{111051, {-2.7044, 1.3016, -154.3177,70 ,8.5 }},
	{111040, {-2.7046, 1.3016, -154.3186,60 ,10 }},
	{111030, {-2.6990, 1.2990, -154.3179,30 ,10 }},
	{111020, {-2.6989, 1.2991, -154.3145,30 ,10 }},
	{111010, {-2.7064, 1.3025, -154.3193,5 ,10 }},
	{111060, {-2.7032, 1.3009, -154.3191,50 ,10 }},
	{151151, {-2.9226, 0.6667, -167.1604,70 ,8.5 }},
	{151153, {-2.9225, 0.6667, -167.1604,70 ,8.5 }},
	{151152, {-2.9226, 0.6667, -167.1604,70 ,8.5 }},
	{151140, {-2.9219, 0.6668, -167.1565,60 ,10 }},
	{151130, {-2.9273, 0.6680, -167.1563,30 ,10 }},
	{151120, {-2.9269, 0.6679, -167.1570,30 ,10 }},
	{151110, {-2.9172, 0.6656, -167.1583,5 ,10 }},
	{151160, {-2.9247, 0.6674, -167.1567,50 ,10 }},
	{111152, {-3.0013, 0.0000, -180.0000,70 ,8.5 }},
	{111153, {-3.0013, 0.0000, -180.0000,70 ,8.5 }},
	{111151, {-3.0013, 0.0000, -180.0000,70 ,8.5 }},
	{111140, {-3.0012, 0.0000, -180.0000,60 ,10 }},
	{111130, {-2.9951, 0.0000, -180.0000,30 ,10 }},
	{111120, {-2.9951, 0.0000, -180.0000,30 ,10 }},
	{111110, {-3.0038, 0.0000, -180.0000,5 ,10 }},
	{111160, {-3.0000, 0.0000, -180.0000,50 ,10 }},
	{151251, {-2.9225, -0.6662, 167.1689,70 ,8.5 }},
	{151253, {-2.9226, -0.6666, 167.1620,70 ,8.5 }},
	{151252, {-2.9225, -0.6663, 167.1673,70 ,8.5 }},
	{151240, {-2.9222, -0.6665, 167.1623,60 ,10 }},
	{151230, {-2.9271, -0.6676, 167.1628,30 ,10 }},
	{151220, {-2.9270, -0.6677, 167.1609,30 ,10 }},
	{151210, {-2.9175, -0.6654, 167.1623,5 ,10 }},
	{151260, {-2.9249, -0.6671, 167.1636,50 ,10 }},
	{111252, {-2.7043, -1.3017, 154.3146,70 ,8.5 }},
	{111253, {-2.7043, -1.3017, 154.3146,70 ,8.5 }},
	{111251, {-2.7045, -1.3016, 154.3177,70 ,8.5 }},
	{111240, {-2.7046, -1.3017, 154.3186,60 ,10 }},
	{111230, {-2.6990, -1.2990, 154.3179,30 ,10 }},
	{111220, {-2.6989, -1.2991, 154.3145,30 ,10 }},
	{111210, {-2.7069, -1.3028, 154.3179,5 ,10 }},
	{111260, {-2.7033, -1.3010, 154.3182,50 ,10 }},
	{151351, {-2.3447, -1.8682, 141.4735,70 ,8.5 }},
	{151353, {-2.3447, -1.8682, 141.4735,70 ,8.5 }},
	{151352, {-2.3446, -1.8681, 141.4735,70 ,8.5 }},
	{151340, {-2.3440, -1.8679, 141.4691,60 ,10 }},
	{151330, {-2.3483, -1.8711, 141.4723,30 ,10 }},
	{151320, {-2.3480, -1.8710, 141.4710,30 ,10 }},
	{151310, {-2.3404, -1.8649, 141.4715,5 ,10 }},
	{151360, {-2.3464, -1.8698, 141.4700,50 ,10 }},
	{111352, {-1.8722, -2.3460, 128.6087,70 ,8.5 }},
	{111353, {-1.8724, -2.3459, 128.6131,70 ,8.5 }},
	{111351, {-1.8724, -2.3459, 128.6131,70 ,8.5 }},
	{111340, {-1.8723, -2.3461, 128.6091,60 ,10 }},
	{111330, {-1.8685, -2.3412, 128.6109,30 ,10 }},
	{111320, {-1.8685, -2.3412, 128.6105,30 ,10 }},
	{111310, {-1.8740, -2.3478, 128.6146,5 ,10 }},
	{111360, {-1.8715, -2.3451, 128.6097,50 ,10 }},
	{151451, {-1.3017, -2.7002, 115.7502,70 ,8.5 }},
	{151453, {-1.3017, -2.7002, 115.7502,70 ,8.5 }},
	{151452, {-1.3017, -2.7002, 115.7502,70 ,8.5 }},
	{151440, {-1.3016, -2.7000, 115.7488,60 ,10 }},
	{151430, {-1.3038, -2.7046, 115.7489,30 ,10 }},
	{151420, {-1.3038, -2.7044, 115.7512,30 ,10 }},
	{151410, {-1.2996, -2.6958, 115.7496,5 ,10 }},
	{151460, {-1.3026, -2.7024, 115.7472,50 ,10 }},
	{111452, {-0.6688, -2.9259, 102.8807,70 ,8.5 }},
	{111453, {-0.6687, -2.9259, 102.8791,70 ,8.5 }},
	{111451, {-0.6683, -2.9258, 102.8721,70 ,8.5 }},
	{111440, {-0.6685, -2.9259, 102.8750,60 ,10 }},
	{111430, {-0.6671, -2.9202, 102.8742,30 ,10 }},
	{111420, {-0.6671, -2.9202, 102.8739,30 ,10 }},
	{111410, {-0.6689, -2.9283, 102.8725,5 ,10 }},
	{111460, {-0.6682, -2.9246, 102.8752,50 ,10 }}
};





/**
 * @brief Read all lines from a text file.
 * @param filename Path to the file.
 * @return Vector of strings, each string is a line from the file.
 *
 * Usage: auto files = readlines("filelist.txt");
 */
std::vector<std::string> readlines(std::string filename){
	std::vector<std::string> all_lines;
	std::string line; std::ifstream fileobj(filename);
	while(std::getline(fileobj,line)){
		all_lines.push_back(std::move(line));
	}
	return all_lines;
}


/**
 * @brief Apply wedge cut to a hit. defined if hits hitting on the wedge of the main detector or not. 
 * @param height Detector height.
 * @param hit Detector hit object.
 * @return True if hit is on the wedge, false otherwise.
 */
bool def_wedge(RemollHit& hit,double height){
	 
	if((hit.det/10)%10 == 1){
		if(hit.yl < 5){ // this is specific for ring 1 detector, becasue the the mean part is from -15 to 5 mm based on the new design.
			return false;
		}else{
			return true;
		}
	}else{
		if (hit.yl < height){
			return false;
		}else{
			return true;
		}
	}
}


/**
 * @brief Apply front cut to a hit. we are only interested in hits hitting on the front of the main detector.
 * @param thickness Detector thickness.
 * @param height Detector height.
 * @param hit Detector hit object.
 * @return True if hit passes front cut, false otherwise.
 */
bool cut_front(RemollHit& hit,double height,double thickness){ //dx means the width of the detector
	if(def_wedge(hit,height)){
		double b = height + thickness - 0.5;
		if(hit.yl > hit.zl + b){
			return true;
		}else{
			return false;
		}
	}else{
		if(hit.zl < 0.5 - thickness ){
			return true;
		}else{
			return false;
		}
	}
		
}

/**
 * @brief Rotate a 3D vector by given Z and X angles. basicly rotate the coordinate system for our detector.
 * @param point Input vector (TVector3).
 * @param rotateZ Rotation angle around Z axis (degrees).
 * @param rotateX Rotation angle around X axis (degrees).
 * @return Rotated vector (TVector3).
 */
TVector3 cal_newP(TVector3 point, double rotateX, double rotateY, double rotateZ){
	TRotation rotation;
	
   

	// Rotate around the X-axis
    rotation.RotateX(rotateX * TMath::DegToRad());

	// Rotate around the Y-axis
	rotation.RotateY(rotateY * TMath::DegToRad());

	// Rotat around the Z-axis
	rotation.RotateZ(rotateZ * TMath::DegToRad());


	// Apply the rotation to the point
	return rotation * point;
}

/**
 * @brief Convert a single hit from lab frame to local detector coordinates.
 *        Returns a struct with all relevant local and global information.
 * @param hit The detector hit (RemollHit).
 * @return mainquartz_local_info struct with local coordinate angle, rotated momentum, wedge information and detector ID.
 *
 * Usage example:
 *     mainquartz_local_info info = convert_labframe_to_local_thinquartz(hit);
 */
mainquartz_local_info convert_labframe_to_local_thinquartz(RemollHit hit){
	mainquartz_local_info info;
	auto [angleX, angleY, angleZ, height, thickness] = maindetector_detid_map[hit.det];
	double rotate_z = angleZ;
	double rotate_x = angleX;
	double rotate_y = angleY;

	bool is_edge = cut_front(hit,height,thickness);
	//if you want to include the back face, just comment out the is_edge part
	if(!is_edge){
		//std::cerr << "Error: hit not in front face or wedge face" << std::endl;
        //return mainquartz_local_info{};
	}
	//bool is_wedge = def_wedge(hit,height);
	//if (info.lx != hit.xl) std::cout << "lx incorrect" << std::endl;

	//wstd::cout << hit.yl << " " << info.ly << std::endl;
	TVector3 point(hit.px, hit.py, hit.pz);
	TVector3 rotatedMomentum = cal_newP(point,rotate_x,rotate_y,rotate_z);
	//all the angle here is the angle between the momentum and the z axis

	info.lpx = rotatedMomentum.X(); 
	info.lpy = rotatedMomentum.Y();
	info.lpz = rotatedMomentum.Z();
	info.lthx = atan2(rotatedMomentum.Z(),rotatedMomentum.X())*180.0/M_PI;
	info.lthy = atan2(rotatedMomentum.Z(),rotatedMomentum.Y())*180.0/M_PI;
	info.lpolar = atan2(sqrt(rotatedMomentum.X()*rotatedMomentum.X()+rotatedMomentum.Y()*rotatedMomentum.Y()),rotatedMomentum.Z())*180.0/M_PI;
	info.lazimuthal = atan2(rotatedMomentum.Y(),rotatedMomentum.X())*180.0/M_PI;
	info.is_wedge = def_wedge(hit,height);
	info.is_edge = is_edge;
	info.detid = hit.det;
	return info;
}	

