// Run using (for example):
//   build/reroot -l -q 'analysis/raster/plot_gen_beam_raster.C("remollout.root")'

void plot_gen_beam_raster(const TString& files)
{
  TChain* T = new TChain("T");
  T->Add(files);

  remollUnits_t* units = 0;
  std::vector<remollEventParticle_t>* parts = 0;

  T->SetBranchAddress("units", &units);
  T->SetBranchAddress("part", &parts);

  // Axis sizes
  Double_t pos = 5.0;
  Double_t dir = 0.001;

  TH2F hvxvy_at_zu("hvxvy_at_zu","v_x vs v_y at z = US",200,-pos,+pos,200,-pos,+pos);
  TH2F hpxpy_at_zu("hpxpy_at_zu","p_x vs p_y at z = US",200,-dir,+dir,200,-dir,+dir);
  TH2F hvxpx_at_zu("hvxpx_at_zu","v_x vs p_x at z = US",200,-pos,+pos,200,-dir,+dir);
  TH2F hvypy_at_zu("hvypy_at_zu","v_y vs p_y at z = US",200,-pos,+pos,200,-dir,+dir);
  TH2F hvxvy_at_z0("hvxvy_at_z0","v_x vs v_y at z = 0",200,-pos,+pos,200,-pos,+pos);
  TH2F hpxpy_at_z0("hpxpy_at_z0","p_x vs p_y at z = 0",200,-dir,+dir,200,-dir,+dir);
  TH2F hvxpx_at_z0("hvxpx_at_z0","v_x vs p_x at z = 0",200,-pos,+pos,200,-dir,+dir);
  TH2F hvypy_at_z0("hvypy_at_z0","v_y vs p_y at z = 0",200,-pos,+pos,200,-dir,+dir);
  TH2F hvxvy_at_zd("hvxvy_at_zd","v_x vs v_y at z = DS",200,-pos,+pos,200,-pos,+pos);
  TH2F hpxpy_at_zd("hpxpy_at_zd","p_x vs p_y at z = DS",200,-dir,+dir,200,-dir,+dir);
  TH2F hvxpx_at_zd("hvxpx_at_zd","v_x vs p_x at z = DS",200,-pos,+pos,200,-dir,+dir);
  TH2F hvypy_at_zd("hvypy_at_zd","v_y vs p_y at z = DS",200,-pos,+pos,200,-dir,+dir);
  TH2F hvxvy_at_zg("hvxvy_at_zg","v_x vs v_y at z = z_gen",200,-pos,+pos,200,-pos,+pos);
  TH2F hpxpy_at_zg("hpxpy_at_zg","p_x vs p_y at z = z_gen",200,-dir,+dir,200,-dir,+dir);
  TH2F hvxpx_at_zg("hvxpx_at_zg","v_x vs p_x at z = z_gen",200,-pos,+pos,200,-dir,+dir);
  TH2F hvypy_at_zg("hvypy_at_zg","v_y vs p_y at z = z_gen",200,-pos,+pos,200,-dir,+dir);

  // Loop over events
  for (size_t iev = 0; iev < T->GetEntries(); iev++) {
    T->GetEntry(iev);

    for (size_t ipart = 0; ipart < parts->size(); ipart++) {
      remollEventParticle_t part = parts->at(ipart);

      Double_t vx = part.vx/units->mm;
      Double_t vy = part.vy/units->mm;
      Double_t vz = part.vz/units->mm;

      Double_t thx = atan(part.px/part.pz);
      Double_t thy = atan(part.py/part.pz);

      hvxvy_at_zg.Fill(vx,vy);
      hpxpy_at_zg.Fill(thx,thy);
      hvxpx_at_zg.Fill(vx,thx);
      hvypy_at_zg.Fill(vy,thy);

      Double_t z0 = 0*units->m;
      hvxvy_at_z0.Fill(vx+(z0-vz)*tan(thx),vy+(z0-vz)*tan(thy));
      hpxpy_at_z0.Fill(thx,thy);
      hvxpx_at_z0.Fill(vx+(z0-vz)*tan(thx),thx);
      hvypy_at_z0.Fill(vy+(z0-vz)*tan(thy),thy);

      Double_t zu = -0.75*units->m;
      hvxvy_at_zu.Fill(vx+(zu-vz)*tan(thx),vy+(zu-vz)*tan(thy));
      hpxpy_at_zu.Fill(thx,thy);
      hvxpx_at_zu.Fill(vx+(zu-vz)*tan(thx),thx);
      hvypy_at_zu.Fill(vy+(zu-vz)*tan(thy),thy);

      Double_t zd = +0.75*units->m;
      hvxvy_at_zd.Fill(vx+(zd-vz)*tan(thx),vy+(zd-vz)*tan(thy));
      hpxpy_at_zd.Fill(thx,thy);
      hvxpx_at_zd.Fill(vx+(zd-vz)*tan(thx),thx);
      hvypy_at_zd.Fill(vy+(zd-vz)*tan(thy),thy);
    }
  }

  TCanvas c1;
  c1.Divide(2,2);
  c1.cd(1);
  hvxvy_at_zg.Draw();
  c1.cd(2);
  hpxpy_at_zg.Draw();
  c1.cd(3);
  hvxpx_at_zg.Draw();
  c1.cd(4);
  hvypy_at_zg.Draw();
  c1.SaveAs("images/raster_correlation_zg.png");

  TCanvas c2;
  c2.Divide(2,2);
  c2.cd(1);
  hvxvy_at_z0.Draw();
  c2.cd(2);
  hpxpy_at_z0.Draw();
  c2.cd(3);
  hvxpx_at_z0.Draw();
  c2.cd(4);
  hvypy_at_z0.Draw();
  c2.SaveAs("images/raster_correlation_z0.png");

  TCanvas c3;
  c3.Divide(2,2);
  c3.cd(1);
  hvxvy_at_zu.Draw();
  c3.cd(2);
  hpxpy_at_zu.Draw();
  c3.cd(3);
  hvxpx_at_zu.Draw();
  c3.cd(4);
  hvypy_at_zu.Draw();
  c3.SaveAs("images/raster_correlation_zu.png");

  TCanvas c4;
  c4.Divide(2,2);
  c4.cd(1);
  hvxvy_at_zd.Draw();
  c4.cd(2);
  hpxpy_at_zd.Draw();
  c4.cd(3);
  hvxpx_at_zd.Draw();
  c4.cd(4);
  hvypy_at_zd.Draw();
  c4.SaveAs("images/raster_correlation_zd.png");
}
