#include "remollMultScatt.hh"

#include "Randomize.hh"

#include <math.h>
#include <assert.h>
#include <stdio.h>
#include <time.h>
#include <stdlib.h>

#include "G4SystemOfUnits.hh"
#include "G4PhysicalConstants.hh"

remollMultScatt::remollMultScatt() {
    InitInternal();

    return;
}

remollMultScatt::remollMultScatt( double p, int nmat, double t[], double A[], double Z[] ){
    /*
       p    - electron momentum
       nmat - number of materials
       t    - Thickness
       A    - Mass number
       Z    - Atomic number
       */
    InitInternal();
    remollMultScatt();
    Init( p, nmat, t, A, Z );
    return;
}

remollMultScatt::remollMultScatt( double p, double t, double A, double Z ){
    /*
       p    - electron momentum
       nmat - number of materials
       t    - Thickness
       A    - Mass number
       Z    - Atomic number
       */
    InitInternal();
    Init( p, t, A, Z );

    return;
}

void remollMultScatt::InitInternal(){
    fInit = false;
    fErf2sig = erf(2.0/sqrt(2.0));
    fNmat = 0;
}

void remollMultScatt::Init( double p, int nmat, double t[], double A[], double Z[] ){
    /* 
       Load materials and calculate necessary
       variables to generate distributions

       p    - electron momentum
       nmat - number of materials
       t    - Thickness
       A    - Mass number
       Z    - Atomic number
    */

    int i;

    if( nmat >= MAT_MAX ){
	fprintf(stderr, "%s %s line %d: Too many materials.  Limited by MAT_MAX (%d)\n", 
		__FILE__, __FUNCTION__, __LINE__, MAT_MAX );
	return;
    }

    fNmat = nmat;

    fReturnZero = false;
    fInit       = false;

    fp = p;

    for( i = 0; i < nmat; i++ ){
	assert( !std::isnan(t[i]) && !std::isinf(t[i]) &&
		!std::isnan(A[i]) && !std::isinf(A[i]) &&
		!std::isnan(Z[i]) && !std::isinf(Z[i]) );

	ft[i] = t[i];
	fA[i] = A[i];
	fZ[i] = Z[i];
    }

    double radsum = 0.0;
    double X0;

    for( i = 0; i < nmat; i++ ){
	X0  = (716.4*g/cm2)*A[i]/(Z[i]*(Z[i]+1.0)*log(287.0/sqrt(Z[i])));
	radsum += t[i]/X0;
    }

    // First work out characteristic gaussian spread.
    // this is the PDG number, which is different from
    // the Moliere f0 width.  I think this number
    // accounts for the higher order terms in the sum,
    // so it's what we should use.
    assert(radsum > 0.0);
    assert(p != 0);
    double thpdg  = 13.6*MeV*sqrt(radsum)*(1.0 + 0.038*log(radsum))/p;
    fthpdg = thpdg;

    // First calculate b

    
    double expb_num, expb_den;
    double bsum = 0.0;

    for( i = 0; i < fNmat; i++ ){
	expb_num = (6680.0*cm2/g)*ft[i]*(fZ[i]+1.0)*pow(fZ[i],1.0/3.0);
	expb_den = fA[i]*(1.0+3.34*pow(fZ[i]/137.0,2.0));

	bsum += expb_num/expb_den;
    }

    // Check to see if we have a relevant amount of material
    // otherwise don't bother
    if( fNmat == 0 || log(bsum) < 1.0 ){
	fInit = true;
	fReturnZero = true;

	/*
	fprintf(stderr, "%s line %d: WARNING sum of b is %f\n", __FILE__, __LINE__, bsum );
	for( i = 0; i < fNmat; i++ ){
	    fprintf(stderr, "\tThickness mat %d: %f g/cm2\n", i, ft[i] );
	}
	fprintf(stderr, "Too little material - disabling MS for this configuration\n");
	*/

	return;
    }

    double b = log( bsum );
    assert( b > 1.0 );

    if( std::isnan(b) || std::isinf(b) || b <= 0.0 ){
	fprintf(stderr, "%s line %d: ERROR  sum of b is %f\n", __FILE__, __LINE__, bsum );
	exit(1);
    }

    // Need to solve
    // B - log(B) = b
    // Use Newtons's method

    fB = solvelogeq(b);

    /////////////////////////////////////////

    // Change of variables

    double chi2, chi2_num, chi2_den;

    chi2 = 0.0;

    for( i = 0; i < fNmat; i++ ){
	chi2_num = 4.0*3.14159*e_squared*e_squared*ft[i]*fZ[i]*(fZ[i]+1.0);
	chi2_den = fp*fp*(A[i]*g/mole);
	chi2  += chi2_num/chi2_den;
    }

    fchi2 = chi2;


    // This is the number from Moliere
    // double th  = sqrt(fchi2*fB/2.0);

    fth = thpdg;

    assert( !std::isnan(fth));
    double v0  = CalcMSDistPlane( 0.0    );
    double v2  = CalcMSDistPlane( 2.0*fth);
    double v10 = CalcMSDistPlane(10.0*fth);

    // Area under 2sigma gaussian
    double Agaus = v0*fErf2sig*sqrt(2.0*3.14159)*fth;

    // exponential parameters
    double l = log( v2/v10 )/(8.0*fth);
    fl = l;
    double C = v2*exp(l*2.0*fth);
    fC = C;

    // Area under tail envelope
    double Dt =  exp(-l*2.0*fth) - exp(-l*10.0*fth);
    fDt    = Dt;
    double Atail = C*Dt/l;

    ftailprob = Atail/(Atail+Agaus);

    fInit = true;

    return;
}

void   remollMultScatt::Init( double p, double t, double A, double Z ){
    /*
       p    - electron momentum, [GeV]
       nmat - number of materials
       t    - Thickness [g/cm2]
       A    - Mass number
       Z    - Atomic number
       */

    double tt[] = {t};
    double tA[] = {A};
    double tZ[] = {Z};

    Init( p, 1, tt, tA, tZ );
    return;
}

double remollMultScatt::J0(double x) {
    // Returns J0 for any real x
    // Stolen from ROOT in TMath.cxx
    
    double ax,z;
    double xx,y,result,result1,result2;
    const double p1  = 57568490574.0, p2  = -13362590354.0, p3 = 651619640.7;
    const double p4  = -11214424.18,  p5  = 77392.33017,    p6 = -184.9052456;
    const double p7  = 57568490411.0, p8  = 1029532985.0,   p9 = 9494680.718;
    const double p10 = 59272.64853,   p11 = 267.8532712;

    const double q1  = 0.785398164;
    const double q2  = -0.1098628627e-2,  q3  = 0.2734510407e-4;
    const double q4  = -0.2073370639e-5,  q5  = 0.2093887211e-6;
    const double q6  = -0.1562499995e-1,  q7  = 0.1430488765e-3;
    const double q8  = -0.6911147651e-5,  q9  = 0.7621095161e-6;
    const double q10 =  0.934935152e-7,   q11 = 0.636619772;

    ax = fabs(x);

    if (ax < 8) {
	y=x*x;
	result1 = p1 + y*(p2 + y*(p3 + y*(p4  + y*(p5  + y*p6))));
	result2 = p7 + y*(p8 + y*(p9 + y*(p10 + y*(p11 + y))));
	result  = result1/result2;
    } else {
	z  = 8/ax;
	y  = z*z;
	xx = ax-q1;
	result1 = 1  + y*(q2 + y*(q3 + y*(q4 + y*q5)));
	result2 = q6 + y*(q7 + y*(q8 + y*(q9 - y*q10)));
	result  = sqrt(q11/ax)*(cos(xx)*result1-z*sin(xx)*result2);
    }

    if( std::isnan(result) || std::isinf(result) ){
	fprintf(stderr, "ERROR %s line %d: %s failed\n", __FILE__, __LINE__, __FUNCTION__ );
	fprintf(stderr, "Tried to find J0(x) for x = %g\n", x );
    }

    assert( !std::isnan(result) && !std::isinf(result) );

    return result;
}

double remollMultScatt::solvelogeq(double b){
    // Newton's method to solve B - log(B) = b
    const double err = 1e-4;
    const int MAX_ITER = 500;
    const int MAX_CORR = 50;

    assert(b > 0.0);

    double thisB = b;
    double lastB = 1e9;

    int n = 0;

    double f, df;
    
    // Fix at 100 iterations
    while( n < MAX_ITER && fabs(thisB-lastB)>err ){
	assert(thisB > 0.0);

	f  = thisB - log(thisB) - b;
	df = 1.0 - 1.0/thisB;

	lastB  = thisB;
	thisB -= f/df;

	// Overshot...
	if( !(thisB > 0.0) ){
	    /*
	    fprintf(stderr, "WARNING %s\n\t%s:  Newton's method produced negative value\n", __FILE__, __PRETTY_FUNCTION__);
	    fprintf(stderr, "\tlastB = %f  thisB = %f   f = %f  df = %f\n", lastB, thisB, f, df);
	    */
	    int iter = 1;
	    while( thisB < 0.0 && iter < MAX_CORR ){
		thisB = lastB - f/df/pow(2.0, iter);
		iter++;
	    }
	    assert( iter < MAX_CORR );
	}

	n++;
    }
    if( n == MAX_ITER ){
	fprintf(stderr, "WARNING %s\n\t%s:  Newton's method did not converge\n", __FILE__, __PRETTY_FUNCTION__);
	fprintf(stderr, "\treturning %f with error %f ( %f - %f ~= %f )\n", thisB, thisB - log(thisB) - b, thisB, log(thisB), b );
    }

    return thisB;
}


double remollMultScatt::fn_integrand( double u, double th, int n ){
    // Check for bad values of logarithm

    if( !(u > 0.0) ) return 0.0;

    assert( !std::isnan(log(u)) && !std::isinf(log(u)) ); 
    assert( !std::isnan(th) && !std::isinf(th) ); 

    double retval = u*J0(u*th)*exp(-0.25*u*u)*pow(0.25*u*u*log(0.25*u*u),n);

    assert( !std::isnan(retval) );
    return retval;
}

double remollMultScatt::intsimpson_fn( double th, int n ){
    if( n >= 5 ) {fprintf(stderr, "%s %s: %d:  Warning, integrating over integrand terms that are of too large n\n", 
	    __FILE__, __FUNCTION__, __LINE__ ); }

    assert( !std::isnan(th) && !std::isinf(th) );

    /* Simpson's method of integration.
       We will choose the integration step
       to be dynamically generated based on th.
     */

    //  Zeros for J0 are spaced apart by at least  ~2.4
    //  so we will integrate at most in steps of 2.4/2

    double bess_step = 2.4/2.0/th;

    //  We want to integrate over the gaussian term at 
    //  most in 0.2 unit steps

    double gauss_step = 1.0;

    // Take the minimum
    double step = (bess_step < gauss_step? bess_step : gauss_step );

    // Integrate over to 8
    //  This should be good enough for n<3
    int  maxstep = (int) (8.0/step);
    int    nstep = 0;

    double stepsum;
    double sum = 0.0;

    for( nstep = 0; nstep < maxstep; nstep++ ){
	stepsum = (step/6.0)*(
	  	      fn_integrand(step*nstep, th, n)
	         +4.0*fn_integrand(step*(nstep+0.5), th, n)
	         +    fn_integrand(step*(nstep+1.0), th, n)
	       );

	sum += stepsum;
    }

    double fact = 1.0;
    int i;
    for( i = 1; i <= n; i++ ){
	fact *= i;
    }

    if( std::isnan( sum ) || std::isnan(fact) ){
	fprintf(stderr, "%s line %d:  %s failed\n", __FILE__, __LINE__, __FUNCTION__ );
	fprintf(stderr, "sum  = %g\nfact = %g\n", sum, fact );
    }
    return sum/fact;
}

double remollMultScatt::CalcMSDistPlane( double theta, double p, double t, double A, double Z ){
    Init( p, t, A, Z );
    return CalcMSDistPlane(theta);
}

double remollMultScatt::CalcMSDistPlane( double theta, double p, int nmat, double t[], double A[], double Z[] ){
    Init( p, nmat, t, A, Z );
    return CalcMSDistPlane(theta);
}

double remollMultScatt::CalcMSDistPlane( double theta){
    /*
       p    - electron momentum
       nmat - number of materials
       t    - Thickness
       A    - Mass number
       Z    - Atomic number
       */

    if( fReturnZero ) return 0.0;

    assert(! std::isnan(theta));
    double th = fabs(theta)/sqrt(fchi2*fB);

    if( std::isnan(th) ){
	fprintf(stderr, "%s line %d: %s failed\n", __FILE__, __LINE__, __FUNCTION__ );
	fprintf(stderr, "theta = %f\nfchi2 = %f\nfB = %f\n\n", theta, fchi2, fB);
    }

    // Separately, we do three integrals from Moillere
    // Let's use Simpson's rule
    double f0 = 2.0*exp(-th*th);
    double f1 = intsimpson_fn( th, 1 );
    double f2 = intsimpson_fn( th, 2 );
//    double f3 = intsimpson_fn( th, 3 );
    double f3 = 0.0;

    double retval = f0 + f1/fB + f2/pow(fB,2.0) + f3/pow(fB,3.0);

    if( std::isnan(retval) ){
	fprintf(stderr, "%s line %d: %s failed\n", __FILE__, __LINE__, __FUNCTION__ );
	fprintf(stderr, "f0 = %f\nf1 = %f\nf2 = %f\nf3 = %f\n\n", f0, f1, f2, f3);
    }

    assert( !std::isnan(retval) );
    return retval;
}

double remollMultScatt::CalcMSDist( double theta, double p, double t, double A, double Z ){
    Init( p, t, A, Z );
    return CalcMSDist(theta);
}

double remollMultScatt::CalcMSDist( double theta, double p, int nmat, double t[], double A[], double Z[] ){
    Init( p, nmat, t, A, Z );
    assert( !std::isnan(theta));
    return CalcMSDist(theta);
}

double remollMultScatt::CalcMSDist( double theta){
    assert( !std::isnan(theta));
    return CalcMSDistPlane(theta)*sin(fabs(theta));
}

double remollMultScatt::GenerateMSPlane(){
    /*
       Generate an event for a single momentum in a
       "plane projected" distribution (i.e. what you
       would measure if you took the angle in the a 
       single direction).


       We will assume perfect Gaussian up to 2
       sigma of that Gaussian.  This part is very fast

       For the tail events, we will use the sample/check
       method.  The envelope we sample is a decaying
       exponential from 2 - 10 sigma.  
     
       All the relevant parameters for this were set
       in Init()

     */
    if( !fInit || fReturnZero ){
	return 0.0;
    }

    double trialv;

    // start rolling dice

    if( G4UniformRand() > ftailprob ){
	// Gaussian
	// Make sure we don't take more than two sigma here
	do {
	    trialv = sin(2.0*3.14159*G4UniformRand())*sqrt(-2.0*log(G4UniformRand()));
	}
	while( fabs(trialv) > 2.0 );

	assert( !std::isnan(trialv*fth) );
	return trialv*fth;
    } else {
	//  Now we have our long tail
	//  here we use sample and reject sampling from an exponential
	//  envelope
	//  We'll work with just positive for now and set the sign at
	//  the end
	//
	//  This has an efficiency of ~0.5, which is probably 
	//  pretty good since this are only 5% of the distribution
	do {
	    trialv = -log(exp(-fl*2.0*fth) - fDt*G4UniformRand()) /fl;
	    assert( !std::isnan(trialv));
	} 
	while ( G4UniformRand() > CalcMSDistPlane( trialv )*
		exp(fl*trialv)/fC);


	// Choose side
	if( G4UniformRand() < 0.5 ){
	    trialv *= -1.0;
	}

	assert( !std::isnan(trialv) );
	return trialv;
    }

    return -1e9;
}

double remollMultScatt::GenerateMSPlane( double p, int nmat, double t[], double A[], double Z[] ){
    /*
       p    - electron momentum, [GeV]
       nmat - number of materials
       t    - Thickness [g/cm2]
       A    - Mass number
       Z    - Atomic number
       */
    Init(p, nmat, t, A, Z);
    return GenerateMSPlane();
}

double remollMultScatt::GenerateMSPlane( double p, double t, double A, double Z ){
    /*
       p    - electron momentum
       nmat - number of materials
       t    - Thickness
       A    - Mass number
       Z    - Atomic number
       */
    Init(p, t, A, Z);
    return GenerateMSPlane();
}


double remollMultScatt::GenerateMS(){
    // This returns the polar coordinate
    // theta for a single event

    // Generate to ten sigma out
    double thmax = 10.0*fth;

    // Weight by sin(th)
    double thisth = GenerateMSPlane();
    while( sin(thisth) < G4UniformRand()*sin(thmax) ){
	thisth = GenerateMSPlane();
    }

    return thisth;
}

double remollMultScatt::GenerateMS( double p, int nmat, double t[], double A[], double Z[] ){
    /*
       p    - electron momentum
       nmat - number of materials
       t    - Thickness 
       A    - Mass number
       Z    - Atomic number
       */
    Init(p, nmat, t, A, Z);
    return GenerateMS();
}

double remollMultScatt::GenerateMS( double p, double t, double A, double Z ){
    /*
       p    - electron momentum
       nmat - number of materials
       t    - Thickness
       A    - Mass number
       Z    - Atomic number
       */
    Init(p, t, A, Z);
    return GenerateMS();
}
