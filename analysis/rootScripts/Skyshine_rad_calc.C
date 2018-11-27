using namespace std;
#include <iostream>

void Skyshine_rad_calc(string modifier = "thickness", const int nEvents = 1000000){//,string units = "mm",int number = 6){
////to use, either run the parser_hallrad_root.sh with the modifier passed in, or use the 'SAMs' modifier with "benchmark" and any number/no number as the third entry



//For each file in the directory (list?) 
//  open the file and find the bin where the energy min for the cut is
//    for det 1006 (roof) counts of E>30MeV (25.12 is bin 75 on log X scale histograms) neutrons (maxes out at bin 94)
//    for det 1001 (LHRS) NEIL (Gold standard) (all three species)
//    for det 3201 (o ring) power/energy deposited (all three species)
//    take integrals of the entire histograms and take the ratio wrt the benchmarks
//    benchmark = noSAMs (with dump, without dump, with shielding, without shielding - with U, no SAMs is the official benchmark)
//    Make a new line entry in a overall output text file (.csv)
//  Plot the line graphs for the benchmark, mitigated, and worst case scenario summaries (just do it by hand for now)

//FIXME Read the modifier, number, and units from a parsed text file list, as in parserHallrad.sh (just use that as a loop to run this over an over again)
//  string modifier = "thickness";
  int number = 6;
  string units = "mm";
//use parserHallrad format to loop over files instead of doing it here
  TFile *file_in;
  string ben = "benchmark";
  //int nEvents = 1e5;//9e6 for regular runs, 54e6 for benchmark runs;
  file_in = TFile::Open(Form("%s",modifier.c_str()));
  
  //TH1D *hist_101_g_neilLogX=(TH1D*)file_in->Get("Det_101/ha_101_g_neilLogX");
  //TH1D *hist_101_e_neilLogX=(TH1D*)file_in->Get("Det_101/ha_101_e_neilLogX");
  //TH1D *hist_101_n_neilLogX=(TH1D*)file_in->Get("Det_101/ha_101_n_neilLogX");
  TH1D *hist_101_n_enerLogX=(TH1D*)file_in->Get("Det_101/ha_101_n_enerLogX");
  TH1D *hist_hSummary_enerLogX=(TH1D*)file_in->Get("hSummary_enerLogX");
  TH1D *hist_hSummary_neilLogX=(TH1D*)file_in->Get("hSummary_neilLogX");
  //TH1D *hist_101_g_enerLogX=(TH1D*)file_in->Get("Det_101/ha_101_g_enerLogX");
  //TH1D *hist_101_e_enerLogX=(TH1D*)file_in->Get("Det_101/ha_101_e_enerLogX");
  //TH1D *hist_101_n_enerLogX=(TH1D*)file_in->Get("Det_101/ha_101_n_enerLogX");

  //NEIL_101 = hist_101_g_neilLogX->Integral(0,94) + hist_1001_e_neilLogX->Integral(0,94) + hist_1001_n_neilLogX->Integral(0,94);
  Double_t NEIL_101 = hist_hSummary_neilLogX->GetBinContent(hist_hSummary_neilLogX->GetXaxis()->FindBin("101 Avg"));
  Double_t NEIL_101_error = hist_hSummary_neilLogX->GetBinError(hist_hSummary_neilLogX->GetXaxis()->FindBin("101 Avg"));
  Double_t Flux_101 = hist_101_n_enerLogX->Integral(hist_101_n_enerLogX->GetXaxis()->FindBin(25.12),94);
  Double_t Flux_101_error = sqrt(Flux_101/nEvents);
  Double_t Energy_101 = hist_hSummary_enerLogX->GetBinContent(hist_hSummary_enerLogX->GetXaxis()->FindBin("101 Avg"));
  Double_t Energy_101_error = hist_hSummary_enerLogX->GetBinError(hist_hSummary_enerLogX->GetXaxis()->FindBin("101 Avg"));

/*SAMs benchmark
  Absolute per event: Neil in LHRS 1001 = 7.98787e-05(9.111e-07), Flux of neutrons to 1006, per event (E>25.12 MeV) = 2.03071e-05(1.50211e-06), Energy in 3201 = 0.108771(6.84946e-05)
  Relative per event: Neil in LHRS 1001 = 1(0.0161306), Flux of neutrons to 1006, per event (E>25.12 MeV) = 1(0.0798966), Energy 3201 = 1(0.00089055)
  noSAMs benchmark
  Absolute per event: Neil in LHRS 1001 = 2.27186e-05(6.09897e-07), Flux of neutrons to 1006, per event (E>25.12 MeV) = 1.30612e-05(1.20468e-06), Energy in 3201 = 0.0113672(1.73374e-05)
  Relative per event: Neil in LHRS 1001 = 0.284414(0.00829588), Flux of neutrons to 1006, per event (E>25.12 MeV) = 0.643185(0.0624217), Energy 3201 = 0.104506(0.000172445)
  SAMs_Ushield benchmark
  Absolute per event: Neil in LHRS 1001 = 6.40978e-05(7.04657e-07), Flux of neutrons to 1006, per event (E>25.12 MeV) = 2.02412e-05(1.49967e-06), Energy in 3201 = 0.107989(6.49807e-05)
  Relative per event: Neil in LHRS 1001 = 0.80244(0.0127119), Flux of neutrons to 1006, per event (E>25.12 MeV) = 0.996754(0.0797483), Energy 3201 = 0.992815(0.00086473)
  noSAMs_Ushield benchmark
  Absolute per event: Neil in LHRS 1001 = 1.05303e-05(3.83341e-07), Flux of neutrons to 1006, per event (E>25.12 MeV) = 1.27087e-05(1.18831e-06), Energy in 3201 = 0.0112639(1.69927e-05)
  Relative per event: Neil in LHRS 1001 = 0.131829(0.00502909), Flux of neutrons to 1006, per event (E>25.12 MeV) = 0.625826(0.0614931), Energy 3201 = 0.103556(0.000169288)
  improvedSAMs_Ushield benchmark
  Absolute per event: Neil in LHRS 1001 = 1.71573e-05(4.60409e-07), Flux of neutrons to 1006, per event (E>25.12 MeV) = 1.28942e-05(1.19695e-06), Energy in 3201 = 0.023519(2.84886e-05)
  Relative per event: Neil in LHRS 1001 = 0.214791(0.00626292), Flux of neutrons to 1006, per event (E>25.12 MeV) = 0.634963(0.061983), Energy 3201 = 0.216225(0.000295192)

  prexSim 1006 == remoll 101

*/
  //Absolute per event: Neil in LHRS 1001 = 1.0121e-05(1.28749e-07), Flux of neutrons to 1006, per event (E>25.12 MeV) = 1.27963e-05(1.6863e-07), Energy in 3201 = 0.0113008(5.94492e-06)
  Double_t bench_NEIL_101 = 4.9331e-4;
  Double_t bench_NEIL_101_error = 9.51624e-07;  // error propagation = sigma = value after modification * relative error of subparts added in quadratures (sigma/previous value)
  Double_t bench_Flux_101 = 1.27963e-05;
  Double_t bench_Flux_101_error = 1.6863e-07 ;//54M is the number of beam events per run that the averages normalize to
  Double_t bench_Energy_101 = 0.0934535;
  Double_t bench_Energy_101_error = 1.61385e-06;

  Double_t NEIL_101_ratio = NEIL_101/bench_NEIL_101;
  Double_t NEIL_101_ratio_error = NEIL_101_ratio*sqrt(pow(NEIL_101_error/NEIL_101,2)+pow(bench_NEIL_101_error/bench_NEIL_101,2));
  Double_t Flux_101_ratio = Flux_101/bench_Flux_101;
  Double_t Flux_101_ratio_error = Flux_101_ratio*sqrt(pow(Flux_101_error/Flux_101,2)+pow(bench_Flux_101_error/bench_Flux_101,2));
  Double_t Energy_101_ratio = Energy_101/bench_Energy_101;
  Double_t Energy_101_ratio_error = Energy_101_ratio*sqrt(pow(Energy_101_error/Energy_101,2)+pow(bench_Energy_101_error/bench_Energy_101,2));

//Energy numerically integrated in a loop
//for each bin Energy_3201+=the bin number times the bin contents
//for(int i=0; i<95; i++) {
//  Energy_3201 = Energy_3201 + hist_3201_g_enerLogX->GetBin(i)*hist_3201_g_enerLogX->GetBinContent(i) + hist_3201_e_enerLogX->GetBin(i)*hist_3201_e_enerLogX->GetBinContent(i) + hist_3201_n_enerLogX->GetBin(i)*hist_3201_n_enerLogX->GetBinContent(i);
//}

  ofstream file_out;
  ofstream NEIL_101_out;
  ofstream Flux_101_out;
  ofstream Energy_101_out;
  file_out.open("analysis_overview.csv",std::ofstream::out | std::ofstream::app);
  NEIL_101_out.open("analysis_NEIL_101.csv",std::ofstream::out | std::ofstream::app);
  Flux_101_out.open("analysis_Flux_101.csv",std::ofstream::out | std::ofstream::app);
  Energy_101_out.open("analysis_Energy_101.csv",std::ofstream::out | std::ofstream::app);

  // This simulation analysis assumes a modifier describing a kind of benchmark wih 54 million events, or a varied parameter of units and number with 9 million events, and everything is normalized to NEIL, Flux, and Energy per 1M events on target
  if (units!=ben){ //case = doing parameter space searches
    file_out<<"Modified = "<<modifier<<std::endl;
    NEIL_101_out<<modifier<<","<<NEIL_101_ratio<<","<<NEIL_101_ratio_error<<std::endl;
    Flux_101_out<<modifier<<","<<Flux_101_ratio<<","<<Flux_101_ratio_error<<std::endl;
    Energy_101_out<<modifier<<","<<Energy_101_ratio<<","<<Energy_101_ratio_error<<std::endl;
  }
  else{ //case = benchmark
    file_out<<"Modified = "<<modifier<<", benchmark"<<std::endl;
    NEIL_101_out<<modifier<<","<<ben<<","<<ben<<","<<NEIL_101_ratio<<","<<NEIL_101_ratio_error<<std::endl;
    Flux_101_out<<modifier<<","<<ben<<","<<ben<<","<<Flux_101_ratio<<","<<Flux_101_ratio_error<<std::endl;
    Energy_101_out<<modifier<<","<<ben<<","<<ben<<","<<Energy_101_ratio<<","<<Energy_101_ratio_error<<std::endl;
  }
  std::cout<<"Absolute per event: Neil in 101 = "<<NEIL_101<<"("<<NEIL_101_error<<"), Flux of neutrons to 101, per event (E>25.12 MeV) = "<<Flux_101<<"("<<Flux_101_error<<"), Energy in 101 = "<<Energy_101<<"("<<Energy_101_error<<")"<<std::endl;
  std::cout<<"Relative per event: Neil in 101 = "<<NEIL_101_ratio<<"("<<NEIL_101_ratio_error<<"), Flux of neutrons to 101, per event (E>25.12 MeV) = "<<Flux_101_ratio<<"("<<Flux_101_ratio_error<<"), Energy in 101 = "<<Energy_101_ratio<<"("<<Energy_101_ratio_error<<")"<<std::endl;
  file_out<<"Absolute per event: Neil in 101 = "<<NEIL_101<<", Flux of neutrons to 101, per event (E>25.12 MeV) = "<<Flux_101<<", Energy in 101 = "<<Energy_101<<std::endl;
  file_out<<"Relative per event: Neil in 101 = "<<NEIL_101_ratio<<", Flux of neutrons to 101, per event (E>25.12 MeV) = "<<Flux_101_ratio<<", Energy in 101 = "<<Energy_101_ratio<<std::endl;
//print the output and ratio wrt benchmark into the outfile (append)
  file_out.close();
 

  NEIL_101_out.close();
  Flux_101_out.close();
  Energy_101_out.close();
};
