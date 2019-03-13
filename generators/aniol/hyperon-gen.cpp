//hyperon-gen.cpp using Wiser data to generate lambdas
#include <iostream>
#include <cstdlib>
#include <ctime>
#include <cmath>
#include <fstream>
#include <cctype>
#include <cstring>
#include "hyperon-gen.h"

#include <stdio.h>		/* printf, NULL */
#include <stdlib.h>		/* strtod */
#include <iomanip>
#include <string>
#include <sstream>

double Vertex (long int, double vertex[]);
double KaonMomentum (double pK[]);
double LabFrame (double pL[], double pR[], double polvL[], double mass[],
		 double Asym[]);
double Lorentz (double pL[], double LT[][4], int);
double Displacement (double pL[], double);
double SigInv (double pK[], double vertex[]);
double HyperonMomentum (double pL[], double pK[], double, double);
double randf ();
std::string doubleToString (double);

int main (int argc, char **argv)
{
  if (argc <= 1) exit(-1);

  hyp = strtoul(argv[1], 0, 0);

  double var, pLmag, range, dEQsiginv, weight, beta, gamma;
  double cstheta, theta, sntheta, pKz, phi, x, dTprotons;
  double mp, mkplus, pTmax, dgamma, kgamma, sumgamma;
  long int i, icnt = 0, j, k, nPt, nPk, iPt, iPk, gdweight = 0, totevents = 0;
  std::string pos0, pos1, pos2, pL1, pL2, pL3, wt;
  char bl = ' ', eol = '\n';

  std::string filename = name[hyp];
  std::ofstream ofile(filename + ".dat");
  if (Ngamma > 100)
    Ngamma = 100;		// Photons[100] is set in hyperon.h
  mp = mproton;
  mkplus = mkaon;
  s11 = (mp + k0) * (mp + k0) - k0 * k0;	// s for k0=11 GeV
  x =
    s11 * s11 + (mkplus * mkplus - mhyp[hyp] * mhyp[hyp]) * (mkplus * mkplus -
						   mhyp[hyp] * mhyp[hyp]);
  x = (x - 2. * (mkplus * mkplus + mhyp[hyp] * mhyp[hyp]) * s11) / 4. / s11;
  pcmax11 = sqrt (x);		//maximum cm momentum for k0=11 GeV
  pKmax =
    gamma11 * (pcmax11 + beta11 * sqrt (mkplus * mkplus + pcmax11 * pcmax11));
  dTprotons = Dt * rho_p;	// number of protons/cm2 in the width Dt
  Nvertex = tgtLength / Dt;
/*
// calculate pKmin
kgamma = ((mhyp[hyp]+mkaon)*(mhyp[hyp]+mkaon)-mproton*mproton)/2./mproton;
beta=kgamma/(kgamma+mproton);
gamma = 1./sqrt(1.-beta*beta);
pKmin = gamma*beta*mkaon;
*/

  std::cout << "mass hyperon = " << mhyp[hyp] << eol;
  std::cout << "pKmax " << pKmax << " pKmin " << pKmin << eol;
  std::cout << "pcmax11 " << pcmax11 << eol;
  std::cout << "Nvertex " << Nvertex << eol;

  ofile << "pKmax = " << pKmax << " GeV/c" << bl << "pTmax = " << bl <<
    pcmax11 << bl << "mass_hyperon = " << mhyp[hyp] << eol;
  ofile <<
    "(x,y,z) in cm,     (px,py,pz) in GeV/c,      hyperons per electron" <<
    eol;

  for (i = 0; i < Nvertex; i++)
    {
      var = Vertex (i, vertex);	//select the kaon lab vertex
//std::cout<<"i vertex "<<i<<bl<<vertex[0]<<bl<<vertex[1]<<bl<<vertex[2]<<eol;
      nPt = (pcmax11 - 0.1) / dPt;	// start sum over transverse kaon momentum
//std::cout<<"nPt = "<<nPt<<eol;
      pKtrans = dPt;		// pKtrans = 0 gives zero weight
      for (iPt = 0; iPt < nPt; iPt++)
	{
//std::cout<<"iPt pKtrans "<<iPt<<bl<<pKtrans<<eol;
	  pKmag = pKmin;
	  if (pKmag < pKtrans)
	    pKmag = pKtrans;	// pKmag can't be smaller than pKtrans
	  nPk = (sqrt (pKmax * pKmax - pKtrans * pKtrans) - pKmag) / dPk;	//start sum over kaon lab momentum
//std::cout<<"nPk pKtrans pKmag "<<nPk<<bl<<pKtrans<<bl<<pKmag<<eol;

	  for (iPk = 0; iPk < nPk; iPk++)
	    {
//std::cout<<"iPk pKmag "<<iPk<<bl<<pKmag<<eol;
	      var = KaonMomentum (pK);	//select the kaon 4 momenta

	      dEQsiginv = SigInv (pK, vertex);
	      theta = asin (pKtrans / pKmag);
	      weight =
		dTprotons * microbarn * twopi * dPt * dPk * pKtrans / (pK[0] *
								       cos
								       (theta))
		* dEQsiginv;
	      weight = Frac[hyp] * weight;	// modify weight in ratio 3/2/1 for lambda/sigmap/sigma0 production

//std::cout<<"Kaon 4 vector "<<pK[0]<<bl<<pK[1]<<bl<<pK[2]<<bl<<pK[3]<<eol;
//std::cout<<"dEQsiginv weight "<<dEQsiginv<<bl<<weight<<eol;
//std::cout<<" "<<eol;

	      dgamma = (k0 - Kmin) / Ngamma;	// step size for gamma energy
	      kgamma = Kmin;	//starting gamma energy
	      pKz = pK[3];
	      sumgamma = 0;
// select the photon energy between Kmin and k0
	      var = randf ();
	      for (j = 0; j < Ngamma; j++)
		{
		  sumgamma = sumgamma + Photons[j];
//std::cout<<"var sumgamma kgamma "<<var<<bl<<sumgamma<<bl<<kgamma<<eol;
		  if (sumgamma > var)
		    break;
		  kgamma = kgamma + dgamma;
		}

	      kgamma =
		kgamma - dgamma + (var -
				   (sumgamma -
				    Photons[j])) * dgamma / Photons[j];

	      mx = HyperonMomentum (pL, pK, kgamma, pKz);	//select the hyperon momentum

//std::cout<<"pKmag pKtrans Kmin kgamma mx "<<pKmag<<bl<<pKtrans<<bl<<Kmin<<bl<<kgamma<<bl<<mx<<eol;

	      range = Displacement (pL, ctau[hyp]);	// determine the range to the decay point of hyperon
	      pLmag = sqrt (pL[1] * pL[1] + pL[2] * pL[2] + pL[3] * pL[3]);
	      pos[0] = vertex[0] + range * pL[1] / pLmag;
	      pos[1] = vertex[1] + range * pL[2] / pLmag;
	      pos[2] = vertex[2] + range * pL[3] / pLmag;
//std::cout<<"Lambda 4 vector "<<pL[0]<<bl<<pL[1]<<bl<<pL[2]<<bl<<pL[3]<<" weight "<<weight<<eol;
//std::cout<<"range posx posy posz "<<range<<bl<<pos[0]<<bl<<pos[1]<<bl<<pos[2]<<eol;
	      totevents = totevents + 1;
	      pos0 = doubleToString (pos[0]);
	      pos1 = doubleToString (pos[1]);
	      pos2 = doubleToString (pos[2]);
	      pL1 = doubleToString (pL[1]);
	      pL2 = doubleToString (pL[2]);
	      pL3 = doubleToString (pL[3]);
	      wt = doubleToString (weight);
	      if ((wt != "nan") && (pos0 != "nan") && (pos1 != "nan")
		  && (pos2 != "nan") && (pL1 != "nan") && (pL2 != "nan")
		  && (pL3 != "nan"))
		{
		  ofile << pos[0] << bl << pos[1] << bl << pos[2] << bl <<
		    pL[1] << bl << pL[2] << bl << pL[3] << bl << weight <<
		    eol;
		  gdweight = gdweight + 1;
		}
	      pKmag = pKmag + dPk;
	    }
	  pKtrans = pKtrans + dPt;
	}
      k = i / Ncnt;
      if (k * Ncnt == i)
	std::cout << "vertex number " << i << eol;

    }
  std::cout << "gdweight totevents " << gdweight << bl << totevents << eol;

}

//************
double
Vertex (long int j, double vertex[])
{
  double zlo;
  zlo = j * Dt - tgtLength / 2.;
  vertex[0] = rastx * (randf () - 0.5);
  vertex[1] = rasty * (randf () - 0.5);
  vertex[2] = zlo + Dt * randf ();
  return vertex[2];
}

//******************************************
// select kaon PKtrans, pKlong, pKmag
double
KaonMomentum (double pK[])
{
  double cstheta, theta, sntheta, pKz, phi, LHS, pKtransmax;
  double mp, ek;
  char bl = ' ', eol = '\n';
  sntheta = pKtrans / pKmag;
  theta = asin (sntheta);
  pKz = pKmag * cos (theta);
  phi = twopi * randf ();	//select transverse momentum
  pK[3] = pKz;
  pK[2] = pKtrans * sntheta * sin (phi);
  pK[1] = pKtrans * sntheta * cos (phi);
  pK[0] = sqrt (pKmag * pKmag + mkaon * mkaon);
  pKlong = gamma11 * (pKz - beta11 * pK[0]);	//pK long momentum in cm of gamma+p cm

}

// Here is the Displacement of the decay from the point of creation
double
Displacement (double pL[], double ctau)
{
  double gamma, range, z, pLmag, beta;
  pLmag = sqrt (pL[1] * pL[1] + pL[2] * pL[2] + pL[3] * pL[3]);
  beta = pLmag / sqrt (pLmag * pLmag + mhyp[hyp] * mhyp[hyp]);
  gamma = 1. / sqrt (1. - beta * beta);
  z = randf ();			// select random number between 0 and 1
  range = -beta * gamma * ctau * log (1. - z);
  return range;
}


// SigInv kaon production calculation from Wiser thesis
double
SigInv (double pK[], double vertex[])
{
  double t1, t2a, t2, t3, t4, siginv;
  double x, mpi, mkplus, mp;
  double a1 = 368., a2 = 1.91, a3 = 1.91, a4 = 1.15, a5 = -5.91, a6 = -1.74;
  double k, e0, t, r, kIapprox, z1, z2;
  double Fapprox, dk, sum = 0., norm, dEQ, dEQsiginv;
  double pLab, pT, ML, xR, E, pLcm, pcm, pLabL, kmin =
    0., theta, ekplus, abst;
  int nk = 50, i;

  char bl = ' ', eol = '\n';

  nk = Ngamma;
  if (nk < 50)
    nk = 50;
  mp = mproton;
  mkplus = mkaon;
  t = (vertex[2] + tgtLength / 2.) * rhoLH2 / X0;	// fractional radiation length
  pLab = pKmag;			// kaon lab momentum
  pT = pKtrans;			// kaon transverse momentum
  pLcm = pKlong;
//
  pcm = sqrt (pT * pT + pLcm * pLcm);
  xR = pcm / pcmax11;
  ML = sqrt (pT * pT + mkplus * mkplus);
  E = pK[0];
  ekplus = pK[0];
  theta = asin (pT / pLab);
  kmin =
    (mhyp[hyp] * mhyp[hyp] - mkplus * mkplus - mp * mp + 2. * mp * ekplus) / 2. / (mp -
									 ekplus
									 +
									 pLab
									 *
									 cos
									 (theta));
  Kmin = kmin;
  t1 = a1 + a2 / sqrt (s11);
  t2a = 1. - xR + a3 * a3 / s11;
  t2 = pow (t2a, a4);
  t3 = exp (a5 * ML);
  t4 = exp (a6 * pT * pT / E);
  siginv = t1 * t2 * t3 * t4;
//std::cout<<"siginv = "<<siginv<<" ub/GeV^2 for end point energy =  "<<k0<<" GeV"<<eol;

  dk = k0 / nk;
  k = 0.;
  e0 = k0;
  z1 = 4. * t / 3.;
  z2 = 7. * t / 9.;
  for (i = 0; i < nk - 1; i++)
    {
      r = 1. - k / e0;
      kIapprox = (pow (r, z1) - exp (-z2)) / (7. / 9. + 4. / 3. * log (r));
      sum = sum + kIapprox;
      k = k + dk;
    }
  sum = sum * dk;
  norm = k0 / sum;
  dEQ = sum / k0;
  sum = 0.;
  dk = (k0 - kmin) / nk;
  k = kmin;
  for (i = 0; i < nk - 1; i++)
    {
      r = 1. - k / e0;
      kIapprox = (pow (r, z1) - exp (-z2)) / (7. / 9. + 4. / 3. * log (r));
      Photons[i] = kIapprox / k;
      sum = sum + Photons[i];;
      k = k + dk;
    }
  PhotonSum = sum;

//normalize Photons[i]
  for (i = 0; i < nk - 1; i++)
    Photons[i] = Photons[i] / PhotonSum;

  siginv = siginv * norm * sum * dk;
//std::cout<<"pLab pT pLcm kmin siginv "<<pLab<<bl<<pT<<bl<<pLcm<<bl<<kmin<<bl<<siginv<<eol;
//std::cout<<"number of photons between kmin and k0  per electron = "<<sum*dk<<eol;
//std::cout<<"dEQ per electron = "<<dEQ<<eol;
  dEQsiginv = dEQ * siginv;
  return dEQsiginv;
}


// Select Hyperon momentum *****************************
double
HyperonMomentum (double pL[], double pK[], double kgamma, double pKz)
{
  double e0, ek, mx2, px[4], sx, pxmag, var, pLmag;
  double betax, gammax, mQ, mQmax;
  double phiX, csthetaX, thetaX, pcmx, pLX[4];
  double pL0[4], ksig0[4], kcm;	// use this for sigma0 lab momentum, sigma0 will decay rapidly
  int sign = -1, i, j;		//sign = +1 for Lab->X, sign = -1 for X->Lab
  e0 = kgamma + mproton;
  ek = sqrt (pKmag * pKmag + mkaon * mkaon);
  mx2 =
    (e0 - ek) * (e0 - ek) - kgamma * kgamma - pKmag * pKmag +
    2. * kgamma * pKz;
  sx = mx2;
  mx = sqrt (mx2);		// missing mass
// px[i] are the lab values of the missing momentum
  px[0] = e0 - ek;
  px[1] = -pK[1];
  px[2] = -pK[2];
  px[3] = kgamma - pK[3];
  pxmag = sqrt (px[1] * px[1] + px[2] * px[2] + px[3] * px[3]);
  betax = pxmag / px[0];
  gammax = 1. / sqrt (1. - betax * betax);

// check X center of mass values, X = Lambda + Q
  mQmax = mx - mhyp[hyp];
  mQ = randf () * mQmax;	// randomly select mQ in X cm
//here is the Xcm momentum
  pcmx =
    sqrt (((mhyp[hyp] * mhyp[hyp] + mQ * mQ - sx) * (mhyp[hyp] * mhyp[hyp] + mQ * mQ - sx) -
	   4. * mhyp[hyp] * mhyp[hyp] * mQ * mQ) / 4. / sx);
  phiX = twopi * randf ();
  csthetaX = 2. * randf () - 1.;
  thetaX = acos (csthetaX);
// Hyperon Xcm 4 momentum
  pLX[1] = pcmx * sin (thetaX) * cos (phiX);
  pLX[2] = pcmx * sin (thetaX) * sin (phiX);
  pLX[3] = pcmx * csthetaX;
  pLX[0] =
    sqrt (mhyp[hyp] * mhyp[hyp] + pLX[1] * pLX[1] + pLX[2] * pLX[2] + pLX[3] * pLX[3]);
//Calculate the Lorentz transformation from X -> Lab frame
  sign = -1;			// sign = -1 for X->Lab frame
  var = Lorentz (px, LT, sign);
  for (i = 0; i < 4; i++)
    pL[i] = 0.;
  for (j = 0; j < 4; j++)
    pL[0] = pL[0] + LT[0][j] * pLX[j];
  for (j = 0; j < 4; j++)
    pL[1] = pL[1] + LT[1][j] * pLX[j];
  for (j = 0; j < 4; j++)
    pL[2] = pL[2] + LT[2][j] * pLX[j];
  for (j = 0; j < 4; j++)
    pL[3] = pL[3] + LT[3][j] * pLX[j];

  if (mhyp[hyp] == 1.192642)
    {
      for (i = 0; i < 4; i++)
	pL0[i] = pL[i];		// store sigma0 lab momentum
//go into sigma0 center of mass
      kcm = (mhyp[hyp] * mhyp[hyp] - mlambda * mlambda) / 2. / mhyp[hyp];	// lambda momentum in sigma0 cm
      phiX = twopi * randf ();
      csthetaX = 2. * randf () - 1.;
      thetaX = acos (csthetaX);
// lambda 4 momentum in the sigma0 cm
      ksig0[1] = kcm * sin (thetaX) * cos (phiX);
      ksig0[2] = kcm * sin (thetaX) * sin (phiX);
      ksig0[3] = kcm * csthetaX;
      ksig0[0] =
	sqrt (mlambda * mlambda + ksig0[1] * ksig0[1] + ksig0[2] * ksig0[2] +
	      ksig0[3] * ksig0[3]);
//Calculate the Lorentz transformation from X -> Lab frame
      sign = -1;		// sign = -1 for sigma0->Lab frame
      var = Lorentz (pL0, LT, sign);
      for (i = 0; i < 4; i++)
	pL[i] = 0.;
      for (j = 0; j < 4; j++)
	pL[0] = pL[0] + LT[0][j] * ksig0[j];
      for (j = 0; j < 4; j++)
	pL[1] = pL[1] + LT[1][j] * ksig0[j];
      for (j = 0; j < 4; j++)
	pL[2] = pL[2] + LT[2][j] * ksig0[j];
      for (j = 0; j < 4; j++)
	pL[3] = pL[3] + LT[3][j] * ksig0[j];
    }
  return mx;
}

// Here is the Lorentz transformation
//  calculate LT from lab to rest frame for sign = +1
// use sign = -1 restframe -> lab frame
double
Lorentz (double pL[], double LT[][4], int sign)
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
