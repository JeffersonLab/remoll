#ifndef __ANACONST_H
#define __ANACONST_H

/*
  global constants needed for analysiS
*/

const int nSpecies=6;
const string spTit[nSpecies]={"e-/pi-","e/#pi E>1","#gamma","neutron","primary e E>1","e+/pi+"};
const int spCls[nSpecies]={kRed,kOrange+7,kBlack,kBlue,kCyan+1,kViolet};
const string spH[nSpecies]={"em","e1","g","n","eP1","ep"};
std::map<int,int> spM {{11,1},{-211,1},{22,3},{2112,4},{-11,6},{211,6}};

const int nErange=4; //all, E<=0.1MeV; 0.1<E<=10MeV; 10MeV<E;
const string eRgTit[nErange]={"all E","E<=0.1","0.1<E<=10","10<E"};
const double eRanges[nErange]={0,0.1,10,1e6};

const int nDmg=3;
const string dmgTit[nDmg]={"rate","rate*E","rate*NIEL"};

const int nFB=3; //Pz cut; 
const string fbH[nFB]={"allPZ","pzG0","pzL0"};

const int nZcut=11;
const string zCutTit[nZcut]={"tgt region", "coll1", "coll2", "US", "coll4+Pb", "lintel+coll5", "collar1", "drift pipe", "collar2", "air+pipe","SM+Absorber"};
const int zCuts[nZcut][2]={
  {-5125,-1800},
  {175,675},
  {750,900},
  {1000,3000},
  {3225,6850},
  {7800,7900},
  {12200,12500},
  {12500,18700},
  {18700,19000},
  {19000,22000},
  {22700,23700}};

const double pi=acos(-1);
#endif //__ANACONST_H
