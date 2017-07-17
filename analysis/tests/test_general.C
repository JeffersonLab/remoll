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
  T.Draw("hit.r   >> h_hit_r(150,0.0,1.5)","hit.det==28",hopt);
  c1.SaveAs(outputdir + "/" + inputname + "_" + "h_hit_r.png");
  T.Draw("ev.vx >> h_vx(200,-0.010,+0.010)","",hopt);
  c1.SaveAs(outputdir + "/" + inputname + "_" + "h_vx.png");
  T.Draw("ev.vy >> h_vy(200,-0.010,+0.010)","",hopt);
  c1.SaveAs(outputdir + "/" + inputname + "_" + "h_vy.png");
  T.Draw("ev.vz >> h_vz(200,-1.00,+1.00)","",hopt);
  c1.SaveAs(outputdir + "/" + inputname + "_" + "h_vz.png");

  // Save and close
  file.Write();
  file.Close();
}
