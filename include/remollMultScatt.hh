#ifndef __REMOLLMULTSCATT_HH
#define __REMOLLMULTSCATT_HH

/*
   -------------------------------------------------
   remollMultScatt

   Multiple scattering distribution calculation
   class.

   Seamus Riordan
   sriordan@physics.umass.edu

   August 24, 2011
   -------------------------------------------------

   The general use of this class is to load up some
   materials and generate multiple scattering events
   or to calculate what the (non-normalized) theta 
   distribution is for a given material.

   All of this is based on Hans Bethe's paper over
   Molière scattering

   H. A. Bethe, Phys. Rev. 89, 1256 (1953)

   Units we use:

   momentum [GeV], thickness [g/cm2], angle [rad]

   For the former, one can set the particle and 
   materials in a few ways.  These take the following 
   arguments:

   p - electron momentum, [GeV]
   t - Thickness [g/cm2]
   A - Mass number
   Z - Atomic number

   1)  No materials at instantiation, later call
       Init for single or multiple materials
   2)  Single or multiple materials at instantiation

   Once these are set, one can call GenerateMS()
   to start getting a distribution equal to one given
   by all the combined materials.

   One can also call GenerateMS with the same
   arguments fed to Init.  This is slower  since it has 
   to calculate a bunch of parameters requiring
   integrals over the distribution.  Sometimes you can't
   avoid this necessity, such as if you are doing MS
   through parts of a long target.

   To access the numeric theta distributions, you can 
   call CalcMSDist with the angle and optionally the 
   material parameters as you would call GenerateMS.

   -------------------------------------------------
*/

#include <vector>
#include <tuple>

class remollMultScatt {

    public:
	remollMultScatt();
	remollMultScatt(double p, const std::tuple<double,double,double>& m);
	remollMultScatt(double p, const std::vector<std::tuple<double,double,double>>& m);

	void   Init(double p, const std::tuple<double,double,double>& m);
	void   Init(double p, const std::vector<std::tuple<double,double,double>>& m);

	virtual ~remollMultScatt() {;}

	double J0(double x);
	double CalcMSDistPlane( double theta);
	double CalcMSDistPlane( double theta, double p, const std::vector<std::tuple<double,double,double>>& m);
	double CalcMSDistPlane( double theta, double p, const std::tuple<double,double,double>& m);

	double CalcMSDist(double theta);
	double CalcMSDist(double theta, double p, const std::vector<std::tuple<double,double,double>>& m);
	double CalcMSDist(double theta, double p, const std::tuple<double,double,double>& m);

	double GenerateMS();
	double GenerateMS(double p, const std::tuple<double,double,double>& m);
	double GenerateMS(double p, const std::vector<std::tuple<double,double,double>>& m);
	double GenerateMSPlane();
	double GenerateMSPlane(double p, const std::tuple<double,double,double>& m);
	double GenerateMSPlane(double p, const std::vector<std::tuple<double,double,double>>& m);

	double GetPDGTh(){ return fthpdg; }

    private:

	bool   fInit, fReturnZero;
	double fErf2sig;

	double fp;
	std::vector<std::tuple<double,double,double>> fm;

	double fth;
	double fthpdg;
	double fB;
	double fchi2;

	// exponential parameters
	double fl;
	double fC;
	double fDt;
	double ftailprob;

	double solvelogeq(double b);
	double fn_integrand( double u, double th, int n );
	double intsimpson_fn( double th, int n );

	void   InitInternal();
};

#endif//__REMOLLMULTSCATT_HH
