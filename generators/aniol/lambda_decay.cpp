//  lambda_decay.cpp
#include <iostream>
#include <cstdlib>
#include <ctime>
#include <cmath>
#include <fstream>
#include <cctype>
#include <cstring>
#include "hyperon_decay.h"

#include <stdio.h>		/* printf, NULL */
#include <stdlib.h>		/* strtod */
#include <iomanip>
#include <string>
#include <sstream>

double randf ();
double Lorentz (double pL[], double LT[][4], double);

int main (int argc, char** argv)
{
  if (argc <= 1) exit(-1);

  int decay = strtoul(argv[1], 0, 0);

  double cnts, r, x, theta, sntheta, rad = 57.295, phi, beta, gamma;
  double kmag, ka[4], con;
  double xv, yv, zv, weight;
  double pL0[4], kgam[4], kcm, cstheta;	// use this line for pi0 decay

  std::string name;
  double alpha, Sr, h, frac, mhyp, ma, mb, sign;
  switch (decay) {
  case 0:
    // setup for lambda -> proton + piminus
    name = "lambda-pim.dat";
    alpha = 0.645;
    Sr = 0.75;
    h = -1.;
    frac = 0.64;
    mhyp = mLambda;
    ma = mpion;
    mb = mproton;
    sign = -1.;
    break;
  case 1:
    // setup for lambda -> neutron + pi0
    name = "lambda-pi0.dat";
    alpha = 0.645;
    Sr = 0.75;
    h = -1.;
    frac = 0.36;
    mhyp = mLambda;
    ma = mpi0;
    mb = mneutron;
    sign = -1.;
    break;
  default:
    exit(-1);
  }

  char str1[40], str2[40], str3[40], str4[40], str5[40], str6[40], str7[40];
  char str8[40], str9[40], str10[40], str11[40];
  char *pEnd;
  std::string nan = ("nan");
  char bl = ' ', eol = '\n';
  int icnts = 0, j, ncnts = 10000;

  std::ifstream hyperons("lambda.dat");
  std::ofstream ofile(name);
  hyperons >> str1 >> str2 >> str3 >> str4 >> str5 >> str6 >> str7 >> str8 >>
    str9 >> str10;
  std::
    cout << str1 << bl << str2 << bl << str3 << bl << str4 << bl << str5 << bl
    << str6 << bl << str7 << bl << str8 << bl << str9 << bl << str10 << eol;
  hyperons >> str1 >> str2 >> str3 >> str4 >> str5 >> str6 >> str7 >> str8 >>
    str9;
  std::
    cout << str1 << bl << str2 << bl << str3 << bl << str4 << bl << str5 << bl
    << str6 << bl << str7 << bl << str8 << bl << str9 << eol;

//evaluate hyperon center of mass momentum
  kmag =
    sqrt (((ma * ma + mb * mb - mhyp * mhyp) * (ma * ma + mb * mb -
						mhyp * mhyp) -
	   4. * ma * ma * mb * mb) / 4. / mhyp / mhyp);
  std::cout << "kmag = " << kmag << eol;
  while (!hyperons.eof ())
    {
      while (!(hyperons >> xv >> yv >> zv >> pL[1] >> pL[2] >> pL[3] >> weight)) {
        hyperons.clear();
        std::string line;
        std::getline(hyperons, line);
        if (hyperons.eof()) break;
      }
//setup hyperon Lab 4 momentum
      pL[0] =
	sqrt (pL[1] * pL[1] + pL[2] * pL[2] + pL[3] * pL[3] + mhyp * mhyp);

      beta = Lorentz (pL, LT, sign);	//setup Lorentz transformation for cm->Lab

// evaluate particle a center of mass momenta
      r = randf ();
      con = alpha * Sr * h;
      x =
	h * sqrt (2. * (2. * r + con / 2. - 1.) / con + 1. / con / con) -
	1. / con;
      sntheta = sqrt (1. - x * x);
      ka[3] = kmag * x;
      phi = twopi * randf ();
      ka[1] = kmag * sntheta * cos (phi);
      ka[2] = kmag * sntheta * sin (phi);
      ka[0] = sqrt (kmag * kmag + ma * ma);

// use LT to go from cm to Lab and find pion lab 4 momentum
      for (j = 0; j < 4; j++)
	pL[j] = 0.;		// zero out the 4 momentum 
      for (j = 0; j < 4; j++)
	pL[0] = pL[0] + LT[0][j] * ka[j];
      for (j = 0; j < 4; j++)
	pL[1] = pL[1] + LT[1][j] * ka[j];
      for (j = 0; j < 4; j++)
	pL[2] = pL[2] + LT[2][j] * ka[j];
      for (j = 0; j < 4; j++)
	pL[3] = pL[3] + LT[3][j] * ka[j];

//generate photons from pi0 decay
      if (ma == mpi0)
	{
	  for (j = 0; j < 4; j++)
	    pL0[j] = pL[j];	//store pi0 lab momentum
	  kcm = mpi0 / 2.;	// photon momentum in pi0 cm
	  phi = twopi * randf ();
	  cstheta = 2. * randf () - 1.;
	  theta = acos (cstheta);
	  sntheta = sin (theta);
	  kgam[0] = kcm;
	  kgam[1] = kcm * sntheta * cos (phi);
	  kgam[2] = kcm * sntheta * sin (phi);
	  kgam[3] = kcm * cstheta;
//calculate Lorentz transformation from pi0->Lab frame
	  sign = -1.;
	  beta = Lorentz (pL0, LT, sign);
	  for (j = 0; j < 4; j++)
	    pL[j] = 0.;
	  for (j = 0; j < 4; j++)
	    pL[0] = pL[0] + LT[0][j] * kgam[j];
	  for (j = 0; j < 4; j++)
	    pL[1] = pL[1] + LT[1][j] * kgam[j];
	  for (j = 0; j < 4; j++)
	    pL[2] = pL[2] + LT[2][j] * kgam[j];
	  for (j = 0; j < 4; j++)
	    pL[3] = pL[3] + LT[3][j] * kgam[j];
	}

      weight = weight * frac;	//renormalize for this channel particle a
      ofile << xv << bl << yv << bl << zv << bl << pL[1] << bl << pL[2] << bl
	<< pL[3] << bl << weight << eol;
      icnts = icnts + 1;
      if ((icnts / ncnts) * ncnts == icnts)
	std::cout << "icnts " << icnts << eol;
    }
  std::cout << "icnts = " << icnts << eol;
}

// Here is the Lorentz transformation
//  calculate LT from lab to rest frame for sign = +1
// use sign = -1 restframe -> lab frame
double
Lorentz (double pL[], double LT[][4], double sign)
{
  double beta, gamma, betax, betay, betaz, beta2, fact, rad = 57.295;
  double eL, pL0 = 0.;
  int i, j;
  eL = pL[0];
  pL0 = sqrt (pL[1] * pL[1] + pL[2] * pL[2] + pL[3] * pL[3]);
  beta = pL0 / eL;
  beta2 = beta * beta;
  betax = sign * beta * (pL[1] / pL0);
  betay = sign * beta * (pL[2] / pL0);
  betaz = sign * beta * (pL[3] / pL0);
  gamma = 1 / sqrt (1. - beta2);
  fact = (gamma - 1.) / beta2;
  LT[0][0] = gamma;
  LT[0][1] = -gamma * betax;
  LT[0][2] = -gamma * betay;
  LT[0][3] = -gamma * betaz;
  LT[1][0] = -gamma * betax;
  LT[1][1] = 1 + fact * betax * betax;
  LT[1][2] = fact * betax * betay;
  LT[1][3] = fact * betax * betaz;
  LT[2][0] = -gamma * betay;
  LT[2][1] = fact * betax * betay;
  LT[2][2] = 1 + fact * betay * betay;
  LT[2][3] = fact * betay * betaz;
  LT[3][0] = -gamma * betaz;
  LT[3][1] = fact * betax * betaz;
  LT[3][2] = fact * betay * betaz;
  LT[3][3] = 1 + fact * betaz * betaz;

  return beta;
}


//*** home made random number generator ****
// Introduce a user derived function randf() which calls the library
// random number generator rand(). The parameter y in randf() serves
// to bring the random number within the range 0<z<1.
double
randf ()
{
  double x, z;
  int j;
  static double y;		// keep the value of y fixed from call to call of randf()
  static int ifirst (0);	// ifirst = 0 on first entry but then is reset to 1
  if (ifirst == 0)		// only enter this calculation on first call to randf()
    {
      y = pow (2., 31.);	// this assumes the maximum integer for a 32 bit machine
      ifirst = 1;		// we only want to calculate y on the first call
      srand (time (0));		// seed random number generator with current clock time since midnight Jan 1 1970.
      x = rand ();
    }
  x = rand ();			// the function rand() is part of the standard c++ math library
  z = x / y;
  if (z == 0.0)
    z = 0.00001;
  if (z >= 1.0)
    z = 0.9999;
  return z;
}


//*** convert double to string to check for "nan" entries
std::string doubleToString (double x)
{
  std::ostringstream out;
  out << std::fixed << x;
  return out.str ();
}
