//hyperon_decay.h setup parameters for hyperon-decay.cpp

double pL[4];			//hyperon lab 4 momentum
double sign;			// sign of Lorentz transformation, sign=+1 Lab->rest frame, sign=-1 rest frame->Lab
double ctauLambda = 7.89;	//needed for range of hyperon from vertex to decay point in Lab
double ctauSigmap = 2.404, ctau;
double pos[3], xL, yL, zL;	//decay point in lab of hyperon
double sR[4], sR0 = 0., sRx, sRy, sRz;	//hyperon polarization 4 vector in rest frame
double mproton = 0.938272, me = 0.000511, mgamma = 0.;
double mpion = 0.13957, mpi0 = 0.13498, mneutron = 0.939565;
double mhyp, mLambda = 1.115683, mSigmap = 1.18937, mSigma0 = 1.192642;
double pi = 3.14159, twopi = 6.283185;
double LT[4][4];		// Lorentz transformation
