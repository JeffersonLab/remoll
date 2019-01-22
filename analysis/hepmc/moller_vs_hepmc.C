void moller_vs_hepmc()
{
  TChain Tmoller("T");
  Tmoller.Add("remollout_ee_ee_moller.root");

  TChain Thepmc("T");
  Thepmc.Add("remollout_ee_ee_hepmc.root");

  TCanvas c1("c1");
  c1.SetTitle("Comparison Moller and HepMC generated events");
  c1.Divide(2,2);
  c1.cd(1);
  Tmoller.Draw("part[0].th/deg >> hm(100,0,2)","ev.xs");
  gPad->SetLogy();
  c1.cd(2);
  Thepmc.Draw("part[0].th/deg >> hh(100,0,2)","ev.xs");
  gPad->SetLogy();
  c1.cd(3);
  TH1F* hm = (TH1F*) gDirectory->Get("hm");
  hm->GetXaxis()->SetTitle("Lab scattering angle #theta [deg]");
  hm->GetYaxis()->SetTitle("Cross section per bin [pb]");
  hm->SetLineColor(kBlue);
  TH1F* hh = (TH1F*) gDirectory->Get("hh");
  hh->GetXaxis()->SetTitle("Lab scattering angle #theta [deg]");
  hh->GetYaxis()->SetTitle("Cross section per bin [pb]");
  hh->SetLineColor(kRed);
  hm->Draw();
  hh->Draw("same");
  gPad->SetLogy();
  c1.cd(4);
  TH1F* hr = new TH1F(*hm);
  hr->Divide(hh, hm);
  hr->GetXaxis()->SetTitle("Lab scattering angle #theta [deg]");
  hr->GetYaxis()->SetTitle("Cross section ratio per bin");
  hr->Draw();
  gPad->SetLogy();
  c1.SaveAs("images/moller_vs_hepmc_xs.png");

  TCanvas c2("c2");
  c2.SetTitle("Comparison Moller and HepMC hits on det 28");
  c2.Divide(2,2);
  c2.cd(1);
  Tmoller.Draw("hit.r/mm >> hrm(150,0,1500)","rate * (hit.det == 28)");
  gPad->SetLogy();
  c2.cd(2);
  Thepmc.Draw("hit.r/mm >> hrh(150,0,1500)","rate * (hit.det == 28)");
  gPad->SetLogy();
  c2.cd(3);
  TH1F* hrm = (TH1F*) gDirectory->Get("hrm");
  hrm->GetXaxis()->SetTitle("Radius at virtual plane 28 [mm]");
  hrm->GetYaxis()->SetTitle("Rate per bin [Hz]");
  hrm->SetLineColor(kBlue);
  TH1F* hrh = (TH1F*) gDirectory->Get("hrh");
  hrh->GetXaxis()->SetTitle("Radius at virtual plane 28 [mm]");
  hrh->GetYaxis()->SetTitle("Rate per bin [Hz]");
  hrh->SetLineColor(kRed);
  hrm->Draw();
  hrh->Draw("same");
  gPad->SetLogy();
  c2.cd(4);
  TH1F* hrr = new TH1F(*hrm);
  hrr->Divide(hrh, hrm);
  hrr->GetXaxis()->SetTitle("Radius at virtual plane 28 [mm]");
  hrr->GetYaxis()->SetTitle("Rate ratio per bin");
  hrr->Draw();
  gPad->SetLogy();
  c2.SaveAs("images/moller_vs_hepmc_rates.png");

}
