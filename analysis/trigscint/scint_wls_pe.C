// Run using (for example):
//   build/reroot -l -q 'analysis/trigscint/scint_wls_pe.C("o_trigscint_moller_wls_5k_pe.root")'

void scint_wls_pe(const TString& files)
{
  Double_t PE_Threshold=3.0;
  TChain* T = new TChain("T");
  T->Add(files);

  std::vector<remollEventParticle_t>* parts = 0;
  std::vector<remollGenericDetectorHit_t>* hits = 0;

  T->SetBranchAddress("hit", &hits);
  T->SetBranchAddress("part", &parts);

  TH1F primary_e("primary_e","Primary electrons hitting the Scintillator; Momemtum (MeV)",500,500,9000);
  TH1F primary_e_vx("primary_e_vx","Primary electrons hitting the Scintillator;Vertex X (mm)",300,-150,150);
  TH1F primary_e_vy("primary_e_vy","Primary electrons hitting the Scintillator;Vertex Y (mm)",520,-260,260);
  TH1F primary_e_vx_zero_PE("primary_e_vx_zero_PE","Primary electrons hitting the Scintillator for Zero PE;Vertex X (mm)",300,-300,300);
  TH1F primary_e_vy_zero_PE("primary_e_vy_zero_PE","Primary electrons hitting the Scintillator for Zero PE;Vertex Y (mm)",300,-300,300);
  TH1F hpe("hpe","Photo-electrons on scintillator detector photo-cathode",200,0,800);
  TH1F hpe_thresh("hpe_thresh",Form("Photo-electrons above threshold %2.0f PE on scintillator detector photo-cathode",PE_Threshold),200,0,800);
  TH1F hpe_bottom("hpe_bottom","Photo-electrons on scintillator detector photo-cathode from bottom (-250 to -100 mm)",600,0,500);
  TH1F hpe_center("hpe_center","Photo-electrons on scintillator detector photo-cathode from center (-100 to 100 mm)",600,0,500);
  TH1F hpe_top("hpe_top","Photo-electrons on scintillator detector photo-cathode from top (100 to 250 mm)",600,0,500);
  TH1F hpe_t("hpe_t","Photon arrival times on scintillator detector photo-cathode; drift time (ns)",50,0,80);
  TH1F hpe_t_thresh("hpe_t_thresh",Form("Last Photon arrival times on photo-cathode (PE-%f); drift time (ns)",PE_Threshold),200,0,400);
  TH1F hpe_t_bottom("hpe_t_bottom","Photon (-250 to -100 mm) arrival  times on scintillator detector photo-cathode; drift time (ns)",200,0,100);
  //needs to find a way to separate hit times based on primary charge particle vertex
  TH1F hpe_t_top("hpe_t_top","Photon (100 to 250 mm) arrival times on scintillator detector photo-cathode; drift time (ns)",200,0,100);
  TH1F hpe_t_center("hpe_t_center","Photon  (-100 to 100 mm) arrival times on scintillator detector photo-cathode; drift time (ns)",200,0,100);
  TH2F hpe_n("hpe_n","Number of events versus impact position on scintillator",50,-300,+300,50,-300,+300);
  TH2F hpe_all_n("hpe_all_n","Number of events versus impact position on scintillator",40,-300,+300,40,-300,+300);//default bin 20
  TH2F hpe_above_thresh_n("hpe_above_thresh_n","Number of events versus impact position on scintillator",40,-300,+300,40,-300,+300);//default bin 20
  TH2F hpe_below_thresh_n("hpe_below_thresh_n","Number of events below threshold (3.0 PE) impact position on scintillator",50,-300,+300,50,-300,+300);
  TH2F hpe_zero_n("hpe_zero_n","Event impact position on scintillator for PE=0",50,-300,+300,50,-300,+300);
  TH2F hpe_xy("hpe_xy","Photo-electrons versus impact position on scintillator",50,-300,+300,50,-300,+300);

  // Loop over events
  Int_t nevent=0;
  Int_t nScintHits=0;
  Bool_t kScintHit=kFALSE;
  for (size_t iev = 0; iev < T->GetEntries(); iev++) {
    T->GetEntry(iev);
    nevent++;
    Int_t nlu = 0;
    Int_t npe = 0;
    
    for (size_t ihit = 0; ihit < hits->size(); ihit++) {
      remollGenericDetectorHit_t hit = hits->at(ihit);
      if (hit.det == 10000 && hit.pid == 11 && hit.trid==1) {
	kScintHit=kTRUE;
	nScintHits++;
	break;
      }
      
    }
    
    Double_t vx, vy,p;
    if (kScintHit){
      for (size_t ihit = 0; ihit < hits->size(); ihit++) {
	remollGenericDetectorHit_t hit = hits->at(ihit);
	
	// Count primary hits on detector at the moment no other detector is available in scint geometry
	//if (hit.det == 10 && hit.trid == 1) {
        //nlu++;
	//}
	
	// Count PE hits on photocathodes 
	if ((hit.det >= 1001 && hit.det <= 1014) && hit.pid == 0) {//1012
	  if (hit.t>=0 && hit.t<10){//2000
	    npe++;	
	    hpe_t.Fill(hit.t);
	    if (npe==PE_Threshold)
	      hpe_t_thresh.Fill(hit.t);
	  }
	}
      }
      for (size_t ipart = 0; ipart < parts->size(); ipart++) {
	remollEventParticle_t part = parts->at(ipart);
	vx = part.vx;
	vy = part.vy;
	p = part.p;
      }

    //if (nlu > 0)

      hpe_all_n.Fill(vx,vy);//fill all hits
      if (npe){
	primary_e.Fill(p);
	primary_e_vx.Fill(vx);
	primary_e_vy.Fill(vy);   
	hpe.Fill(npe);
	if (npe>PE_Threshold){
	  hpe_thresh.Fill(npe);
	  hpe_above_thresh_n.Fill(vx,vy);//fill only for above threshold
	}
	
	hpe_n.Fill(vx,vy);
	hpe_xy.Fill(vx,vy,npe);
	if (vy<-100 && vy>=-300){
	  hpe_bottom.Fill(npe);
	}else if (vy<100 && vy>=-100){
	  hpe_center.Fill(npe);
	}else if (vy<300 && vy>=100){
	  hpe_top.Fill(npe);
	}
    } else {
	hpe_zero_n.Fill(vx,vy);
	primary_e_vx_zero_PE.Fill(vx);
	primary_e_vy_zero_PE.Fill(vy);
      }
      if (npe<PE_Threshold){
	hpe_below_thresh_n.Fill(vx,vy);
      }

      kScintHit=kFALSE;
    }
      
  }
  
  std::cout << "Total events "<<nevent << std::endl;
  std::cout << "Hits on the Scint "<< nScintHits<< std::endl;
  std::cout << "AVG PE Whole Scint (PE>0)" << hpe.GetMean() << " " << hpe.GetMeanError() <<" Events "<< hpe.GetEntries() << std::endl;
  std::cout << "Events with PE=0 "<< (nScintHits - hpe.GetEntries()) << std::endl;
  std::cout << "Events with PE>3 "<< hpe_thresh.GetEntries() << std::endl;
  
  std::cout << "AVG PE above threhold " << hpe_thresh.GetMean() << " " << hpe_thresh.GetMeanError() <<" Events "<<hpe_thresh.GetEntries()<<" Efficiency (PE>0) "<< hpe_thresh.GetEntries()*100/hpe.GetEntries() << " Effciency (Total) " << hpe_thresh.GetEntries()*100/nScintHits << std::endl;
																																	       
  //std::cout << "AVG PE Bottom Scint " << hpe_bottom.GetMean() << " " << hpe_bottom.GetMeanError() << std::endl;
  //std::cout << "AVG PE center Scint " << hpe_center.GetMean() << " " << hpe_center.GetMeanError() << std::endl;
  //std::cout << "AVG PE Top Scint " << hpe_top.GetMean() << " " << hpe_top.GetMeanError() << std::endl;
  TCanvas c;
  hpe.Draw();
  c.SaveAs("./tmp/full_wls_scint_pe.png");
  hpe_thresh.Draw();
  c.SaveAs("./tmp/full_wls_scint_pe_threshold.png");
  /*
  hpe_bottom.Draw();
  c.SaveAs("./Efficiency_checks/full_wls_bottom_pe.png");
  hpe_center.Draw();
  c.SaveAs("./Efficiency_checks/full_wls_center_pe.png");
  hpe_top.Draw();
  c.SaveAs("./Efficiency_checks/full_wls_top_pe.png");
  */
  hpe_t.Draw();
  c.SaveAs("./tmp/full_wls_pe_t.png");
  hpe_t_thresh.Draw();
  c.SaveAs("./tmp/full_wls_pe_t_thresh.png");
  hpe_n.Draw("colz");
  c.SaveAs("./tmp/full_wls_impacts_xy.png");
  hpe_zero_n.SetStats(kFALSE);
  hpe_zero_n.Draw("colz");
  c.SaveAs("./tmp/full_wls_impacts_xy_zero_pe.png");
  hpe_all_n.Draw("colz");
  c.SaveAs("./tmp/full_wls_impacts_xy_all_pe_threshold.png");
  hpe_above_thresh_n.DrawCopy("colz");
  c.SaveAs("./tmp/full_wls_impacts_xy_above_pe_threshold.png");
  hpe_above_thresh_n.Divide(&hpe_all_n);
  hpe_above_thresh_n.SetStats(kFALSE);
  hpe_above_thresh_n.SetTitle("Efficiency Map for PE>3.0");
  hpe_above_thresh_n.SetContour(80);
  //(hpe_above_thresh_n.GetZaxis())->SetNdivisions(40, kTRUE);
  hpe_above_thresh_n.DrawCopy("colz");
  c.SaveAs("./tmp/full_wls_impacts_xy_efficiency.png");
  hpe_below_thresh_n.Draw("colz");
  c.SaveAs("./tmp/full_wls_impacts_xy_below_pe_threshold.png");
  hpe_xy.Draw("colz");
  c.SaveAs("./tmp/full_wls_pe_xy_weighted_PE.png");
  primary_e_vy.Draw();
  c.SaveAs("./tmp/full_wls_primary_e_vy.png");
  primary_e_vx.Draw();
  c.SaveAs("./tmp/full_wls_primary_e_vx.png");
  primary_e.Draw();
  c.SaveAs("./tmp/full_wls_primary_e_p.png");
  primary_e_vx_zero_PE.Draw();
  c.SaveAs("./tmp/full_wls_primary_e_vx_zero_PE.png");
  primary_e_vy_zero_PE.Draw();
  c.SaveAs("./tmp/full_wls_primary_e_vy_zero_PE.png");
}
