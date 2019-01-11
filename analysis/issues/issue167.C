int issue167() {
  T->Scan("sum.by_pid.edep:sum.by_pid.pid:sum.det","sum.by_pid.edep*(sum.by_pid.pid==22 && sum.det==2002)","",10);
  T->Draw("sum.by_pid.edep>>h2","sum.by_pid.edep*(sum.by_pid.pid==22 && sum.det==2002)");
  auto h2 = (TH1D*) gDirectory->Get("h2");
  std::cout << h2->Integral() << std::endl;

  T->Scan("sum.edep:sum.pid:sum.det","sum.edep*(sum.pid==22 && sum.det==2002)","",10);
  T->Draw("sum.edep>>h3","sum.edep*(sum.pid==22 && sum.det==2002)");
  auto h3 = (TH1D*) gDirectory->Get("h3");
  std::cout << h3->Integral() << std::endl;

  return (std::fabs(h2->Integral() - h3->Integral()) < 1.0e-3);
}
