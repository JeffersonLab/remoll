#include "TString.h"
#include "TCanvas.h"
#include "TChain.h"
#include "TFile.h"

void test_general (const TString& inputdir, const TString& inputname)
{
  // Input files
  TChain T("T");
  T.Add(inputdir + "/" + inputname + ".root");

  // Output files
  TString outputdir = inputdir + "/" + "analysis";
  TFile file(outputdir + "/" + inputname + ".root","RECREATE");

  // Histograms
  TString hopt = "TEXT";

  TCanvas c1;
  T.Draw("hit.pid >> h_hit_pid(5000,-2500,+2500)","",hopt);
  c1.SaveAs(outputdir + "/" + inputname + "_" + "h_hit_pid.png");
  T.Draw("hit.det >> h_hit_det(4500,0,4500)","",hopt);
  c1.SaveAs(outputdir + "/" + inputname + "_" + "h_hit_det.png");
  T.Draw("hit.r/m >> h_hit_r(150,0.0,1.5)","hit.det==28",hopt);
  c1.SaveAs(outputdir + "/" + inputname + "_" + "h_hit_r.png");
  T.Draw("part.vx/m >> h_vx(200,-0.010,+0.010)","",hopt);
  c1.SaveAs(outputdir + "/" + inputname + "_" + "h_vx.png");
  T.Draw("part.vy/m >> h_vy(200,-0.010,+0.010)","",hopt);
  c1.SaveAs(outputdir + "/" + inputname + "_" + "h_vy.png");
  T.Draw("part.vz/m >> h_vz(200,-1.00,+1.00)","",hopt);
  c1.SaveAs(outputdir + "/" + inputname + "_" + "h_vz.png");
  T.Draw("ev.beamp/GeV >> h_beamp(110,0.0,+11.0)","",hopt);
  c1.SaveAs(outputdir + "/" + inputname + "_" + "h_beamp.png");
  T.Draw("ev.beamp/GeV:part.vz/m >> h_beamp_vz(20,-1.0,+1.0)","",hopt+",PROF");
  c1.SaveAs(outputdir + "/" + inputname + "_" + "h_beamp_vz.png");
  T.Draw("ev.thcom/deg >> h_thcom(180,0.0,+180.0)","",hopt);
  c1.SaveAs(outputdir + "/" + inputname + "_" + "h_thcom.png");
  T.Draw("log(rate) >> h_lograte(200,-25.0,+25.0)","",hopt);
  c1.SaveAs(outputdir + "/" + inputname + "_" + "h_lograte.png");

  // Save and close
  file.Write();
  file.Close();
}
