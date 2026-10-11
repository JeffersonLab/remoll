#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <TCanvas.h>

TF1* func = NULL;
TF1* func1 = NULL;
TF1* func2 = NULL;
TF1* func3 = NULL;

const double pi = TMath::ACos(-1);
const double Me = 0.511;
const double Me2 = Me*Me;

const int nholes = 21;
const int nsectors = 7;
double angle_lo[nsectors] = {-999};
double angle_up[nsectors] = {-999};
TH1D* radial[nholes] = {NULL};
TH1D* rprime[nholes] = {NULL};
TH1D* phiprime[nholes] = {NULL};
TH1D* phi[nholes] = {NULL};
TH2D* hist_rphi = {NULL};
TH2D* hist_rrprime = {NULL};
TH2D* hist_phi_phiprime = {NULL};

TH2D* h2d_r_phi[nholes] = {NULL};
TH2D* h2d_rprime_phi[nholes] = {NULL};
TH2D* h2d_phiprime_phi[nholes] = {NULL};

double phi_lo[nholes] = {0,       2.3*pi/21, 4*pi/21, 6*pi/21,  8.5*pi/21, 9.7*pi/21, 12*pi/21, 14*pi/21, 16*pi/21, 18*pi/21,  20.32*pi/21, 22*pi/21, 24*pi/21, 26.5*pi/21, 27.65*pi/21, 30*pi/21,  32*pi/21, 33.75*pi/21, 36*pi/21, 38*pi/21, 40*pi/21};
double phi_hi[nholes] = {2.3*pi/21, 4*pi/21, 6*pi/21, 8.5*pi/21, 9.7*pi/21, 12*pi/21, 14*pi/21, 16*pi/21, 18*pi/21, 20.32*pi/21,  22*pi/21, 24*pi/21, 26.5*pi/21, 27.65*pi/21, 30*pi/21, 32*pi/21,  33.75*pi/21, 36*pi/21, 38*pi/21, 40*pi/21, 42*pi/21};

string holeNames[nholes] = {"12", "11", "13", "21", "23", "22", "33", "31", "32", "41", "42", "43", "51", "53", "52", "63", "61", "62", "73", "71", "72"};
double central_sieve_r[nholes] = {58.0, 35.0, 84.5, 50.0, 75.0, 56.0, 80.0, 39.0, 63.0, 50.0, 60.0, 70.0, 56.0, 80.0, 56.0, 75.0, 39.0, 60.0, 84.5, 44.0, 68.0};

bool IsInside(TEllipse* el, double x, double y) {

  double xt = x - el->GetX1();
  double yt = y - el->GetY1();

  double cos_angle = cos(-el->GetTheta() * pi / 180); 
  double sin_angle = sin(-el->GetTheta() * pi / 180);
  double xr = xt * cos_angle - yt * sin_angle;
  double yr = xt * sin_angle + yt * cos_angle;
    
  double term1 = pow(xr / el->GetR1(), 2);
  double term2 = pow(yr / el->GetR2(), 2);
  double result = term1 + term2;

    
  return result <= 1.0;
}

void generating_csv_files(string infile, const int cut = 0){

  TFile* f1 = new TFile(infile.c_str(), "READ");
  TTree* T1 = (TTree*)f1->Get("newT");

  int nevents = T1->GetEntries();

  for(int j=0; j<nholes; j++){ 

    phi[j] = new TH1D(Form("phi_%s", holeNames[j].c_str()),"gem_phi_distribution;phi[rad];Counts",200, 0, 2*pi);
    phiprime[j] = new TH1D(Form("phiprime_%s", holeNames[j].c_str()),"gem_phi_prime_distribution;phi[rad];Counts",200, -0.02, 0.02);
    rprime[j] = new TH1D(Form("rprime_%s", holeNames[j].c_str()),"rprime_distribution;phi[rad];Counts",300, 0.02, 0.08);
    h2d_r_phi[j] = new TH2D(Form("r_phi_%s", holeNames[j].c_str()),"r_phi_distribution;phi[rad];Counts",500, 600, 1100, 200, 0, 2*pi); 
    h2d_rprime_phi[j] = new TH2D(Form("rprime_phi_%s", holeNames[j].c_str()),"rprime_phi_distribution;phi[rad];Counts", 500, 0 , 0.1, 200, 0, 2*pi);
    h2d_phiprime_phi[j] = new TH2D(Form("phiprime_phi_%s", holeNames[j].c_str()),"phiprime_phi_distribution;phi[rad];Counts", 500, -0.02 , 0.02, 200, 0, 2*pi);
    
  } 

  hist_rrprime = new TH2D("hist_rrprime", "GEM r-r' Distribtuion", 500, 0, 0.1, 500, 0, 7);
  hist_phi_phiprime = new TH2D("hist_phi_phiprime", "GEM phi-phi' Distribtuion", 500, -0.02, 0.02, 500, 0, 7);

  double energy_cut = -999;
  string targetName;
  string passName;

  if(infile.find("opticsDS") != std::string::npos) targetName = "C12_opticsDS";
  else if(infile.find("opticsMS") != std::string::npos) targetName = "C12_opticsMS";
  else if(infile.find("opticsMD") != std::string::npos) targetName = "C12_opticsMD";
  else if(infile.find("opticsUM") != std::string::npos) targetName = "C12_opticsUM";
  else if(infile.find("opticsUS") != std::string::npos) targetName = "C12_opticsUS";

  cout << targetName << endl;

  for(int ii = 0; ii < nsectors; ii++){

    angle_lo[ii] = ii*2*pi/7;
    angle_up[ii] = (ii+1)*2*pi/7;

  }


  if(infile.find("p1") != std::string::npos){ 

    energy_cut = 2200.;
    passName = "_p1";
    hist_rphi = new TH2D("hist_rphi", "GEM r-#phi Distribtuion", 600, 800, 1100, 500, 0, 7);

    for(int j=0; j<nholes; j++){ if(infile.find("p1") != std::string::npos) radial[j] = new TH1D(Form("r_%s", holeNames[j].c_str()),"gem_r_distribution;r[mm];Counts",600, 800, 1100); }

  }

  else if(infile.find("p2") != std::string::npos){ 

    energy_cut = 4400.;
    passName = "_p2";
    hist_rphi = new TH2D("hist_rphi", "GEM r-#phi Distribtuion", 800, 700, 1100, 500, 0, 7);

    for(int j=0; j<nholes; j++){ radial[j] = new TH1D(Form("r_%s", holeNames[j].c_str()),"gem_r_distribution;r[mm];Counts",800, 700, 1100); }

  } 

  else if(infile.find("p3") != std::string::npos){ 

    energy_cut = 6600.;
    passName = "_p3";
    hist_rphi = new TH2D("hist_rphi", "GEM r-#phi Distribtuion", 600, 600, 900, 500, 0, 7);

    for(int j=0; j<nholes; j++){ radial[j] = new TH1D(Form("r_%s", holeNames[j].c_str()),"gem_r_distribution;r[mm];Counts",600, 600, 900); }

  }

  else if(infile.find("p4") != std::string::npos){ 

    energy_cut = 8800.;
    passName = "_p4";
    hist_rphi = new TH2D("hist_rphi", "GEM r-#phi Distribtuion", 400, 600, 800, 500, 0, 7);

    for(int j=0; j<nholes; j++){ radial[j] = new TH1D(Form("r_%s", holeNames[j].c_str()),"gem_r_distribution;r[mm];Counts",400, 600, 800); }

  }
  
  hist_rphi->Sumw2();
  hist_rrprime->Sumw2();
  hist_phi_phiprime->Sumw2();

  double sieve_r, gem_r, gem_ph, rate, tg_th, tg_p, tg_ph, tg_vz, gem_px, gem_py, gem_pz, gem_x, gem_y;

  T1->SetBranchAddress("sieve_r",&sieve_r);
  T1->SetBranchAddress("gem1_r",&gem_r);
  T1->SetBranchAddress("gem1_ph",&gem_ph);
  T1->SetBranchAddress("gem1_px",&gem_px);
  T1->SetBranchAddress("gem1_py",&gem_py);
  T1->SetBranchAddress("gem1_pz",&gem_pz);
  T1->SetBranchAddress("gem1_x",&gem_x);
  T1->SetBranchAddress("gem1_y",&gem_y);
  T1->SetBranchAddress("rate",&rate);
  T1->SetBranchAddress("tg_th",&tg_th);
  T1->SetBranchAddress("tg_ph",&tg_ph);
  T1->SetBranchAddress("tg_p",&tg_p);
  T1->SetBranchAddress("tg_vz",&tg_vz);

  for(int j=0; j<nevents; j++){

    T1->GetEntry(j);
    int index_hole = -999;

    rate = rate/200; 

    if(sieve_r < 26.5) continue;
    double gem_k = sqrt(gem_px*gem_px + gem_py*gem_py + gem_pz*gem_pz + Me2);
    if(cut && fabs(gem_k - energy_cut) > 2) continue;
    if(gem_ph<0) gem_ph += 2*pi;
    
    for(int l=0; l<nholes; l++){  if(gem_ph > phi_lo[l] && gem_ph < phi_hi[l]) index_hole = l; }

    double r_prime = (gem_x*gem_px + gem_y*gem_py)/(gem_r*gem_pz);
    double phi_prime = (-gem_y*gem_px+gem_x*gem_py)/(gem_r*gem_pz);

    radial[index_hole]->Fill(gem_r, rate);
    rprime[index_hole]->Fill(r_prime, rate);
    phi[index_hole]->Fill(gem_ph, rate); 
    phiprime[index_hole]->Fill(phi_prime, rate); 

    hist_rphi->Fill(gem_r, gem_ph);
    hist_rrprime->Fill(r_prime, gem_ph);
    hist_phi_phiprime->Fill(phi_prime, gem_ph);


  }

  TCanvas* c1 = new TCanvas();
  c1->cd();
  hist_rphi->Draw("colz");

  TCanvas* c2 = new TCanvas();
  c2->cd();
  hist_rrprime->Draw("colz");

  TCanvas* c3 = new TCanvas();
  c3->cd();
  hist_phi_phiprime->Draw("colz");

  std::ofstream csvFile_merged;
  string fileName_merged = targetName + passName + "_merged.csv";
  csvFile_merged.open(fileName_merged, std::ofstream::app);

  for(int ihole = 0; ihole < nholes; ihole++){

    std::ofstream csvFile;
    string fileName = targetName + passName + "_" + holeNames[ihole] + ".csv";
    csvFile.open(fileName, std::ofstream::app);
    csvFile << "tg_th" << "," << "tg_ph" << "," << "tg_p" << "," << "tg_vz" << "," << "sieve_r" << "," << "central_sieve_r" << "," << "gem_r" << "," << "r_prime" << "," << "gem_ph" << "," << "phi_prime" << "," << "phi_local" << endl; 

    if(radial[ihole]->GetEntries() < 100) continue;
    
    if(cut){ 

     radial[ihole]->GetXaxis()->SetRangeUser(radial[ihole]->GetMean()-5*radial[ihole]->GetRMS(), radial[ihole]->GetMean()+5*radial[ihole]->GetRMS());
     func = new TF1("func", "gaus",radial[ihole]->GetMean()-2.0*radial[ihole]->GetRMS(),radial[ihole]->GetMean()+2.0*radial[ihole]->GetRMS());
      
    }

    else{ 

      radial[ihole]->GetXaxis()->SetRangeUser(radial[ihole]->GetMean()-2.5*radial[ihole]->GetRMS(), radial[ihole]->GetMean()+1.0*radial[ihole]->GetRMS());
      rprime[ihole]->GetXaxis()->SetRangeUser(rprime[ihole]->GetMean()-2.5*rprime[ihole]->GetRMS(), rprime[ihole]->GetMean()+1.0*rprime[ihole]->GetRMS());
      func = new TF1("func", "gaus",radial[ihole]->GetMean()-2.5*radial[ihole]->GetRMS(),radial[ihole]->GetMean()+1.5*radial[ihole]->GetRMS()); 

    }

    radial[ihole]->Fit(func,"RQN");
    for(int itr = 0; itr < 5; itr++) radial[ihole]->Fit(func, "RQN","",func->GetParameter(1)-2.0*func->GetParameter(2),func->GetParameter(1)+2.0*func->GetParameter(2));

    func1 = new TF1("func1", "gaus", phi_lo[ihole], phi_hi[ihole]);  // For fitting phi
    phi[ihole]->Fit(func1,"RQN");

    func2 = new TF1("func2", "gaus", rprime[ihole]->GetMean()-2.5*rprime[ihole]->GetRMS(), rprime[ihole]->GetMean()+1.0*rprime[ihole]->GetRMS());
    for(int itr = 0; itr < 5; itr++) rprime[ihole]->Fit(func2, "RQN","",func2->GetParameter(1)-2.0*func2->GetParameter(2),func2->GetParameter(1)+2.0*func2->GetParameter(2));

    func3 = new TF1("func3", "gaus", phiprime[ihole]->GetMean()-2.5*phiprime[ihole]->GetRMS(), phiprime[ihole]->GetMean()+1.0*phiprime[ihole]->GetRMS());  // For fitting phi_prime
    phiprime[ihole]->Fit(func3,"RQN");

    double lower_r = func->GetParameter(1) - 2*func->GetParameter(2);
    double upper_r = func->GetParameter(1) + 2*func->GetParameter(2);

    double lower_phi = func1->GetParameter(1) - 2*func1->GetParameter(2);
    double upper_phi = func1->GetParameter(1) + 2*func1->GetParameter(2);
    
    double lower_rprime = func2->GetParameter(1) - 2*func2->GetParameter(2);
    double upper_rprime = func2->GetParameter(1) + 2*func2->GetParameter(2);

    double lower_phiprime = func3->GetParameter(1) - 2*func3->GetParameter(2);
    double upper_phiprime = func3->GetParameter(1) + 2*func3->GetParameter(2);

    for(int j=0; j<nevents; j++){

      T1->GetEntry(j);
      if(sieve_r < 26.5) continue;

      double gem_k = sqrt(gem_px*gem_px + gem_py*gem_py + gem_pz*gem_pz + Me2);
      if(cut && fabs(gem_k - energy_cut) > 2) continue;  

      if(gem_ph<0) gem_ph += 2*pi;

      double r_prime = (gem_x*gem_px + gem_y*gem_py)/(gem_r*gem_pz);
      double phi_prime = (-gem_y*gem_px+gem_x*gem_py)/(gem_r*gem_pz);

      if( (gem_r > lower_r && gem_r < upper_r) && (gem_ph > lower_phi && gem_ph < upper_phi) ){  h2d_r_phi[ihole]->Fill(gem_r, gem_ph, rate); }
      if( (r_prime > lower_rprime && r_prime < upper_rprime) && (gem_ph > lower_phi && gem_ph < upper_phi) ){  h2d_rprime_phi[ihole]->Fill(r_prime, gem_ph, rate); }
      if( (phi_prime > lower_phiprime && phi_prime < upper_phiprime) && (gem_ph > lower_phi && gem_ph < upper_phi) ){  h2d_phiprime_phi[ihole]->Fill(phi_prime, gem_ph, rate); }

    }

    double mean_x_rphi = h2d_r_phi[ihole]->GetMean(1);
    double mean_y_rphi = h2d_r_phi[ihole]->GetMean(2);

    double cov_xx_rphi = h2d_r_phi[ihole]->GetCovariance(1, 1);
    double cov_yy_rphi = h2d_r_phi[ihole]->GetCovariance(2, 2);
    double cov_xy_rphi = h2d_r_phi[ihole]->GetCovariance(1, 2);

    TMatrixDSym cov_matrix_rphi(2);
    cov_matrix_rphi(0, 0) = cov_xx_rphi;
    cov_matrix_rphi(1, 1) = cov_yy_rphi;
    cov_matrix_rphi(0, 1) = cov_xy_rphi;
    cov_matrix_rphi(1, 0) = cov_xy_rphi;

    TVectorD eigenvalues_rphi;
    TMatrixD eigenvectors_rphi = cov_matrix_rphi.EigenVectors(eigenvalues_rphi);

    int indexMax_rphi = eigenvalues_rphi[0] > eigenvalues_rphi[1] ? 0 : 1;

    double angle_rphi = (TMath::ATan2(eigenvectors_rphi(1, indexMax_rphi), eigenvectors_rphi(0, indexMax_rphi)))*180/pi;

    double semi_major_rphi = TMath::Sqrt(eigenvalues_rphi[indexMax_rphi]);
    double semi_minor_rphi = TMath::Sqrt(eigenvalues_rphi[1-indexMax_rphi]);

    TEllipse* ellipse_rphi = new TEllipse(mean_x_rphi, mean_y_rphi, 2.0*semi_major_rphi, 2.0*semi_minor_rphi, 0, 360, angle_rphi);
    ellipse_rphi->SetFillStyle(0);
    ellipse_rphi->SetLineColor(kRed);
    ellipse_rphi->SetLineWidth(3);
    c1->cd();
    ellipse_rphi->Draw("same");

    double mean_x_rrprime = h2d_rprime_phi[ihole]->GetMean(1);
    double mean_y_rrprime = h2d_rprime_phi[ihole]->GetMean(2);

    double cov_xx_rrprime = h2d_rprime_phi[ihole]->GetCovariance(1, 1);
    double cov_yy_rrprime = h2d_rprime_phi[ihole]->GetCovariance(2, 2);
    double cov_xy_rrprime = h2d_rprime_phi[ihole]->GetCovariance(1, 2);

    TMatrixDSym cov_matrix_rrprime(2);
    cov_matrix_rrprime(0, 0) = cov_xx_rrprime;
    cov_matrix_rrprime(1, 1) = cov_yy_rrprime;
    cov_matrix_rrprime(0, 1) = cov_xy_rrprime;
    cov_matrix_rrprime(1, 0) = cov_xy_rrprime;

    TVectorD eigenvalues_rrprime;
    TMatrixD eigenvectors_rrprime = cov_matrix_rrprime.EigenVectors(eigenvalues_rrprime);

    int indexMax_rrprime = eigenvalues_rrprime[0] > eigenvalues_rrprime[1] ? 0 : 1;

    double angle_rrprime = (TMath::ATan2(eigenvectors_rrprime(1, indexMax_rrprime), eigenvectors_rrprime(0, indexMax_rrprime)))*180/pi;

    double semi_major_rrprime = TMath::Sqrt(eigenvalues_rrprime[indexMax_rrprime]);
    double semi_minor_rrprime = TMath::Sqrt(eigenvalues_rrprime[1-indexMax_rrprime]);

    TEllipse* ellipse_rrprime = new TEllipse(mean_x_rrprime, mean_y_rrprime, 2.0*semi_major_rrprime, 2.0*semi_minor_rrprime, 0, 360, angle_rrprime);
    ellipse_rrprime->SetFillStyle(0);
    ellipse_rrprime->SetLineColor(kRed);
    ellipse_rrprime->SetLineWidth(3);
    c2->cd();
    ellipse_rrprime->Draw("same");

    double mean_x_phi_phiprime = h2d_phiprime_phi[ihole]->GetMean(1);
    double mean_y_phi_phiprime = h2d_phiprime_phi[ihole]->GetMean(2);

    double cov_xx_phi_phiprime = h2d_phiprime_phi[ihole]->GetCovariance(1, 1);
    double cov_yy_phi_phiprime = h2d_phiprime_phi[ihole]->GetCovariance(2, 2);
    double cov_xy_phi_phiprime = h2d_phiprime_phi[ihole]->GetCovariance(1, 2);

    TMatrixDSym cov_matrix_phi_phiprime(2);
    cov_matrix_phi_phiprime(0, 0) = cov_xx_phi_phiprime;
    cov_matrix_phi_phiprime(1, 1) = cov_yy_phi_phiprime;
    cov_matrix_phi_phiprime(0, 1) = cov_xy_phi_phiprime;
    cov_matrix_phi_phiprime(1, 0) = cov_xy_phi_phiprime;

    TVectorD eigenvalues_phi_phiprime;
    TMatrixD eigenvectors_phi_phiprime = cov_matrix_phi_phiprime.EigenVectors(eigenvalues_phi_phiprime);

    int indexMax_phi_phiprime = eigenvalues_phi_phiprime[0] > eigenvalues_phi_phiprime[1] ? 0 : 1;

    double angle_phi_phiprime = (TMath::ATan2(eigenvectors_phi_phiprime(1, indexMax_phi_phiprime), eigenvectors_phi_phiprime(0, indexMax_phi_phiprime)))*180/pi;

    double semi_major_phi_phiprime = TMath::Sqrt(eigenvalues_phi_phiprime[indexMax_phi_phiprime]);
    double semi_minor_phi_phiprime = TMath::Sqrt(eigenvalues_phi_phiprime[1-indexMax_phi_phiprime]);

    TEllipse* ellipse_phi_phiprime = new TEllipse(mean_x_phi_phiprime, mean_y_phi_phiprime, 2.0*semi_major_phi_phiprime, 2.0*semi_minor_phi_phiprime, 0, 360, angle_phi_phiprime);
    ellipse_phi_phiprime->SetFillStyle(0);
    ellipse_phi_phiprime->SetLineColor(kRed);
    ellipse_phi_phiprime->SetLineWidth(3);
    c3->cd();
    ellipse_phi_phiprime->Draw("same");

    for(int j=0; j<nevents; j++){

      T1->GetEntry(j);
      if(sieve_r < 26.5) continue;
      int index_hole = -999;

      double gem_k = sqrt(gem_px*gem_px + gem_py*gem_py + gem_pz*gem_pz + Me2);
      if(cut && fabs(gem_k - energy_cut) > 2) continue;   
      if(gem_ph<0) gem_ph += 2*pi;

      if(infile.find("opticsDS") != std::string::npos) tg_vz = -3875.0;
      else if(infile.find("opticsDS30") != std::string::npos) tg_vz = -4200.0;
      else if(infile.find("opticsMS") != std::string::npos) tg_vz = -4500.0;
      else if(infile.find("opticsDS30minus") != std::string::npos) tg_vz = -4800.0;
      else if(infile.find("opticsUS") != std::string::npos) tg_vz = -5125.0;

      double r_prime = (gem_x*gem_px + gem_y*gem_py)/(gem_r*gem_pz);
      double phi_prime = (-gem_y*gem_px+gem_x*gem_py)/(gem_r*gem_pz);

      double phi_local = -999;

      for(int jj = 0; jj < nsectors; jj++){ if (gem_ph > angle_lo[jj] && gem_ph < angle_up[jj]){ phi_local = gem_ph - (angle_lo[jj] + angle_up[jj])/2; } }
      for(int l=0; l<nholes; l++){  if(gem_ph > phi_lo[l] && gem_ph < phi_hi[l]) index_hole = l; }
    
      if( IsInside(ellipse_rphi, gem_r, gem_ph) && IsInside(ellipse_rrprime, r_prime, gem_ph) && IsInside(ellipse_phi_phiprime, phi_prime, gem_ph) ){  

        csvFile << tg_th << "," << tg_ph << "," << tg_p << "," << tg_vz << "," << sieve_r << "," << central_sieve_r[index_hole] << "," << gem_r << "," << r_prime << "," << gem_ph << "," << phi_prime << "," << phi_local << endl;
        csvFile_merged << tg_th << "\t" << tg_ph << "\t" << tg_p << "\t" << tg_vz << "\t" << sieve_r << "\t" << central_sieve_r[index_hole] << "\t" << gem_r << "\t" << r_prime << "\t" << gem_ph << "\t" << phi_prime << "\t" << phi_local << endl;

      }
      
    }
      
    cout << "Hole " << holeNames[ihole] << " Analysis Complete" << endl;
  
  }

  string outfileName = targetName + passName + "_plots.root";
  if(cut) outfileName = targetName + passName + "_plots_non_radiative.root";
  TFile* fout = new TFile(outfileName.c_str(),"RECREATE");
  fout->cd();
  for(int ihole = 0; ihole < nholes; ihole++){
    radial[ihole]->Write();
    h2d_r_phi[ihole]->Write();
    h2d_rprime_phi[ihole]->Write();
    h2d_phiprime_phi[ihole]->Write();
    phi[ihole]->Write();
    rprime[ihole]->Write();
    phiprime[ihole]->Write();
  }
  
  hist_rphi->Write();
  hist_rrprime->Write();
  hist_phi_phiprime->Write();

}