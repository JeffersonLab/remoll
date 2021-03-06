// [process][ring][sector]
const int nMatrix=4;
const int nProcDef=8;
const int nProc=8;
double A[nProcDef][6][3],rate[nProc][6][3];
string procNm[nProcDef]={
  "moller",
  "ep Elastic",
  "ep Inelastic",
  "eAl Elastic",
  "eAl Quasielastic",
  "eAl Inelastic",
  "pi minus",
  "neutral bknd"
};

int verbose = 0;

const double neutralBkgndFactor = 1.;
const double neutralBkgndRate[6]={1e9,1e9,1e9,1e9,1e9,1e9};
//const double neutralBkgndRate[6]={5e7,8e7,11e7,7e7,33e7,3e7};

void analyzeOne(int ring, int sect);
void readSim(string fnm,int proc, int addBkgnd);
void printAll();

void yield_error_matrix_bkg(){

  for(int i=0;i<nProc;i++)
    for(int j=0;j<6;j++)
      for(int k=0;k<3;k++){
        A[i][j][k]=0;
        rate[i][j][k]=0;
      }

  /// optimal positioning
  string fnms[nProcDef]={
		      "o_ee_6825000_bkgAna.root",
		      "o_ep-elastic_8680000_bkgAna.root",
		      "o_ep-inelastic_8860000_bkgAna.root",
		      "o_eAl-elastic_8395000_bkgAna.root",
		      "o_eAl-quasielastic_2e7_bkgAna.root",
		      "o_eAl-inelastic_18635000_bkgAna.root",
		      "o_piMinus_259e4_bkgAna.root",
		      "byHand"
  };

  for(int i=0;i<nProc;i++)
    readSim(fnms[i],i,0);

  printAll();
  
  // for(int i=0;i<6;i++)
  //   for(int j=0;j<3;j++)
  //     analyzeOne(i,j);
  analyzeOne(4,2);
}

void analyzeOne(int ring, int sect){
  cout<<"\n\n\nprocessing Ring "<<ring<<" Sector "<<sect<<endl;

  TMatrixD F(nMatrix, nMatrix);
  for(int i=0;i<nMatrix;i++)
    for(int j=0;j<nMatrix;j++)
      F(i,j)=0;
  double B[nMatrix]={0};
  if(verbose){
    cout<<"\tAsym and rate\n";
    for(int i=0;i<nProc;i++)
      cout<<"\t\t"<<procNm[i]<<"\t"<<A[i][ring][sect]<<"\t"<<rate[i][ring][sect]<<endl;
  }

  const double polarization = 0.8;
  const double beamDays = 235 + 95 + 14;
  const double days2seconds = 24*60*60;

  for(int i=0;i<6;i++)
    for(int j=0;j<3;j++){
      double rateTot(0);
      //for(int k=0;k<nMatrix;k++)
      for(int k=0;k<nProc;k++)
        rateTot += rate[k][i][j];
      double sigmaAm = 1/sqrt(rateTot);

      double Ami(0),f[nMatrix];
      for(int k=0;k<nMatrix;k++){
	if(rate[k][i][j]==0 || A[k][ring][sect]==0){
	  f[k]=0;
	  if(verbose)
	    cout<<"\ti/j/k/rate\t"<<i<<"\t"<<j<<"\t"<<k<<"\t"<<rate[k][i][j]<<"\t"
		<<A[k][ring]<<" "<<f[k]<<endl;
	}else{
	  f[k] = rate[k][i][j]/rateTot * A[k][i][j]/A[k][ring][sect];
	  if(verbose)
	    cout<<"\ti/j/k/rate**\t"<<i<<"\t"<<j<<"\t"<<k<<"\t"<<rate[k][i][j]<<"\t"<<f[k]<<endl;
	}
        Ami += f[k]*A[k][ring][sect];
	if(verbose)
	  cout<<"\t i/j/f/f*A/Ami\t"<<i<<"\t"<<j<<"\t"<<f[k]<<"\t"<<f[k]*A[k][ring][sect]
	      <<"\t"<<Ami<<endl;
      }

      for(int k1=0;k1<nMatrix;k1++){
        for(int k2=0;k2<nMatrix;k2++)
          F(k1,k2) += f[k1]*f[k2]/(sigmaAm*sigmaAm);
        B[k1] += Ami * f[k1]/(sigmaAm*sigmaAm);
      }
    }

  F.Invert();
  if(verbose){
    cout<<"\n\tDet: "<<F.Determinant()<<endl;
    cout<<"\tF matrix\n\t";
    for(int i=0;i<nMatrix;i++){
      for(int j=0;j<nMatrix;j++)
	cout<<"\t"<<F(i,j);
      cout<<"\n\t";
    }
    cout<<"B vector\n\t";
    for(int j=0;j<nMatrix;j++)
	cout<<"\t"<<B[j];
    cout<<endl;
    cin.ignore();
  }

  cout<<endl<<"\t\t\tAsymmetry extractor\n";
  for(int i=0;i<nMatrix;i++){
    double asym(0);
    for(int j=0;j<nMatrix;j++)
      asym += F(i,j) * B[j];
    cout<<"Asymmetry "<<procNm[i]<<"\t"<<asym<<endl;
  }

  cout<<endl<<endl<<"\t\t\toverall\nName\tAsymmetry\tuncert[ppb]\trelative uncer[ppb]\n";
  double sigma[nProcDef];
  for(int i=0;i<nMatrix;i++){
    sigma[i] = sqrt( F(i,i) ) / ( 0.8 * sqrt(beamDays * days2seconds) ) * 1e9;
    cout<<procNm[i]<<"\t"<<A[i][ring][sect]<<"\t"<<sigma[i]<<"\t"<<sigma[i]/A[i][ring][sect]<<endl;
  }

  cout<<endl<<"\t\t\tin the selected bin\n";
  double totRate(0);
  for(int i=0;i<nProc;i++)
    totRate += rate[i][ring][sect];
  double stat = totRate / rate[0][ring][sect] /
    ( sqrt(totRate) * polarization * sqrt(beamDays * days2seconds)) *1e9;
  cout<<"stat moller\t"<< stat << "\t" << stat/33<<endl;

  double syst[nProcDef];
  double uncert[nProcDef]={0,0,0,0,1,1,0.1,1};
  for(int i=1;i<nProc;i++){
    if(i<nMatrix)
      syst[i] = rate[i][ring][sect]/rate[0][ring][sect] * sigma[i];
    else
      syst[i] = rate[i][ring][sect]/rate[0][ring][sect] * fabs(A[i][ring][sect]) * uncert[i] ;
    cout<<"syst "<<procNm[i]<<"\t"<<syst[i]<<"\t"<<syst[i]/33<<endl;
  }

}

void printAll(){

  const double polarization = 0.8;
  const double beamDays = 235 + 95 + 14;
  const double days2seconds = 24*60*60;

  for(int i=0;i<6;i++)
    for(int j=0;j<3;j++){
      double rateTot(0);
      for(int k=0;k<nProc;k++)
        rateTot += rate[k][i][j];
      double sigmaAm = 1/sqrt(rateTot);

      double Ami2(0),f2[nProcDef];
      cout<<i<<" "<<j;
      for(int k=0;k<nProc;k++){
        Ami2 += rate[k][i][j]/rateTot * A[k][i][j];
	cout<<" "<<A[k][i][j]<<" "<<rate[k][i][j]/rateTot;
      }
      cout<<" "<<Ami2<<" "<<sigmaAm / ( 0.8 * sqrt(beamDays * days2seconds) ) * 1e9<< " " << rateTot <<endl;

    }
}

void readSim(string fnm,int proc, int addBkgnd){

  if(proc>=7){
  for(int i=0;i<6;i++)
    for(int j=0;j<3;j++)
      if(proc==7){
	rate[proc][i][j] = neutralBkgndRate[i]/3*neutralBkgndFactor;
	A[proc][i][j] = 0;
      }else{
	rate[proc][i][j] = 0;
	A[proc][i][j] = 0;
      }

    return;
  }
  
  if(verbose) cout<<"reading "<<fnm<<"\t"<<proc<<endl;
  TFile *fin=TFile::Open(fnm.c_str(),"READ");
  TH1D *hRate=(TH1D*)fin->Get("hRate");

  const double rateFactor = 65./85;
  double gfFactor = 1;
  if(proc == 3 || proc == 4)
    gfFactor = 1e6;
  //double minRate(1e9);
  for(int i=0;i<6;i++)
    for(int j=0;j<3;j++){
      TH1D *hA=(TH1D*)fin->Get(Form("hAsym_R%d_S%d",i+1,j));
      if(proc==6)
	A[proc][i][j] = -1660;
      else
	A[proc][i][j] = hA->GetMean()*gfFactor;
      
      rate[proc][i][j] = hRate->GetBinContent(i*3+j+1) * rateFactor;
      // if(rate[proc][i][j]<min && rate[proc][i][j]>0)
      // 	minRate=rate[proc][i][j];
      if(verbose)
	cout<<"\tR/S\t"<<i<<"\t"<<j<<"\t"<<A[proc][i][j]<<"\t"<<rate[proc][i][j]<<endl;
    }

  fin->Close();
  if(verbose)
    cin.ignore();
}


