// [process][ring][sector]
double A[7][6][3],rate[7][6][3];
const int nMatrix=4;
const int nProc=7;
string procNm[7]={
  "moller",
  "ep Elastic",
  "ep Inelastic",
  "eAl Elastic",
  "eAl Quasielastic",
  "eAl Inelastic",
  "pi minus"
};

int verbose = 0;

void analyzeOne(int ring, int sect);
void readSim(string fnm,int proc);

void yield_error_matrix_bkg(){

  for(int i=0;i<nProc;i++)
    for(int j=0;j<6;j++)
      for(int k=0;k<3;k++){
        A[i][j][k]=0;
        rate[i][j][k]=0;
      }

  // /// optimal positioning
  // string fnms[nProc]={
  //   "remollout_moller_srcIncCCYZ_bkgAna.root",
  //   "remollout_epelastic_bkgAna.root",
  //   "remollout_epinelastic_bkgAna.root",
  //   // "remollout_eAlelastic_YZoutput_bkgAna.root",
  //   // "remollout_eAlquasielastic_YZoutput_bkgAna.root",
  //   // "remollout_eAlinelastic_YZoutput_bkgAna.root",
  //   "remollout_eAlelastic_srcIncCCyzKKclusterV2_bkgAna.root",
  //   "remollout_eAlquasielastic_srcIncCCyzKKclusterV2_bkgAna.root",
  //   "remollout_eAlinelastic_srcIncCCyzKKclusterV2_bkgAna.root",
  //   // "remollout_eAlelastic_srcIncCCyz_bkgAna.root",
  //   // "remollout_eAlquasielastic_srcIncCCyz_bkgAna.root",
  //   //"remollout_eAlinelastic_srcIncCCyz_bkgAna.root",
  //   "remollout_pion_srcIncCCYZ_bkgAna.root"
  // };

  /// offsetDistributions
  string fnms[nProc]={
    "remollout_moller_srcIncCCYZ",
    "remollout_epelastic",
    "remollout_epinelastic",
    "remollout_eAlelastic_srcIncCCyzKKclusterV2",
    "remollout_eAlquasielastic_srcIncCCyzKKclusterV2",
    "remollout_eAlinelastic_srcIncCCyzKKclusterV2",
    "remollout_pion_srcIncCCYZ"
  };
  const float ofR(-1),ofP(0);

  for(int i=0;i<nProc;i++)
    readSim(Form("%s_bkgAnaTst_offR_%fmm_offPhi_%fmm.root",fnms[i].c_str(),ofR,ofP),i);
  //readSim(fnms[i],i);

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
      for(int k=0;k<nMatrix;k++)
        rateTot += rate[k][i][j];
      double sigmaAm = 1/sqrt(rateTot);

      double Ami(0),f[nMatrix];
      for(int k=0;k<nMatrix;k++){
	if(rate[k][i][j]==0 || A[k][ring][sect]==0){
	  f[k]=0;
	  //cout<<"\ti/j/k/rate\t"<<i<<"\t"<<j<<"\t"<<k<<"\t"<<rate[k][i][j]<<"\t"<<A[k][ring]f[k]<<endl;
	}else{
	  f[k] = rate[k][i][j]/rateTot * A[k][i][j]/A[k][ring][sect];
	  //cout<<"\ti/j/k/rate**\t"<<i<<"\t"<<j<<"\t"<<k<<"\t"<<rate[k][i][j]<<"\t"<<f[k]<<endl;
	}
        Ami += f[k]*A[k][ring][sect];
	//cout<<"\t i/j/f/f*A/Ami\t"<<i<<"\t"<<j<<"\t"<<f[k]<<"\t"<<f[k]*A[k][ring][sect]<<"\t"<<Ami<<endl;
      }

      double Ami2(0),f2[nProc];
      cout<<i<<" "<<j;
      for(int k=0;k<nProc;k++){
	if(rate[k][i][j]==0 || A[k][ring][sect]==0){
	  f2[k]=0;
	  //cout<<"\ti/j/k/rate\t"<<i<<"\t"<<j<<"\t"<<k<<"\t"<<rate[k][i][j]<<"\t"<<A[k][ring]f[k]<<endl;
	}else{
	  f2[k] = rate[k][i][j]/rateTot * A[k][i][j]/A[k][ring][sect];
	  //cout<<"\ti/j/k/rate**\t"<<i<<"\t"<<j<<"\t"<<k<<"\t"<<rate[k][i][j]<<"\t"<<f[k]<<endl;
	}
        Ami2 += f2[k]*A[k][ring][sect];
	//cout<<"\t i/j/f/A/Ami\t"<<i<<"\t"<<j<<"\t"<<f2[k]<<"\t"<<A[k][ring][sect]<<"\t"<<Ami<<endl;
	cout<<" "<<A[k][i][j]<<" "<<rate[k][i][j]/rateTot;
      }
      cout<<" "<<Ami2<<" "<<sigmaAm / ( 0.8 * sqrt(beamDays * days2seconds) ) * 1e9<<endl;

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

  cout<<endl<<"\t\t\tAsymmetry estimator\n";
  for(int i=0;i<nMatrix;i++){
    double asym(0);
    for(int j=0;j<nMatrix;j++)
      asym += F(i,j) * B[j];
    cout<<"Asymmetry "<<procNm[i]<<"\t"<<asym<<endl;
  }

  cout<<endl<<endl<<"\t\t\toverall\nName\tAsymmetry\tuncert[ppb]\trelative uncer[ppb]\n";
  double sigma[nProc];
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

  double syst[nProc];
  double uncert[7]={0,0,0,0,1,1,0.1};
  for(int i=1;i<nProc;i++){
    if(i<nMatrix)
      syst[i] = rate[i][ring][sect]/rate[0][ring][sect] * sigma[i];
    else
      syst[i] = rate[i][ring][sect]/rate[0][ring][sect] * fabs(A[i][ring][sect]) * uncert[i] ;
    cout<<"syst "<<procNm[i]<<"\t"<<syst[i]<<"\t"<<syst[i]/33<<endl;
  }

}

void readSim(string fnm,int proc){
  if(verbose) cout<<"reading "<<fnm<<"\t"<<proc<<endl;
  TFile *fin=TFile::Open(fnm.c_str(),"READ");
  TH1D *hRate=(TH1D*)fin->Get("hRate");

  //double minRate(1e9);
  for(int i=0;i<6;i++)
    for(int j=0;j<3;j++){
      TH1D *hA=(TH1D*)fin->Get(Form("hAsym_R%d_S%d",i+1,j));
      if(proc==6)
	A[proc][i][j] = -1660;
      else
	A[proc][i][j] = hA->GetMean();
      rate[proc][i][j] = hRate->GetBinContent(i*3+j+1);
      // if(rate[proc][i][j]<min && rate[proc][i][j]>0)
      // 	minRate=rate[proc][i][j];
      if(verbose)
	cout<<"\tR/S\t"<<i<<"\t"<<j<<"\t"<<A[proc][i][j]<<"\t"<<rate[proc][i][j]<<endl;
    }

  fin->Close();
  if(verbose)
    cin.ignore();
}





