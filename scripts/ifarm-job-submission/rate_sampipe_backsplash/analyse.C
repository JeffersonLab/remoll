using namespace ROOT;

int analyse(TString source, TString out, TString gen, TString radpos, TString overwrite){

  TChain T("T");
  T.Add(Form("%s", source.Data()));                                  // Adding source file
  Int_t nEvents= T.GetEntries();                                     // Number of primary events
  std::cout << "Analyzing "<< nEvents << "events" << std::endl;      // 
  Double_t weight= 1.0/85;                                           // Divide by current. So, the Y-axis gets units of Hz/uA. 


  TFile f(Form("%s", out.Data()), overwrite);
  f.mkdir(radpos);
  f.cd(radpos);

  std::map<TString, TH1D*> h_vz;
  std::map<TString, TH2D*> h_vrvz;
  std::map<TString, TH2D*> h_xy;
  std::map<TString, TH1D*> h_r;

  std::vector<TString> sector;
  sector.push_back("all");
  sector.push_back("closed");
  sector.push_back("trans");
  sector.push_back("open");

  std::vector<TString> pid;
  pid.push_back("primary_electron");
  pid.push_back("secondary_electron");
  pid.push_back("positron");
  pid.push_back("photon");
  pid.push_back("neutron");
  pid.push_back("all");

  std::vector<TString> ene;
  ene.push_back("pless1MeV");
  ene.push_back("p1to10MeV");
  ene.push_back("pgreater10MeV");

  std::vector<TString> mom;
  mom.push_back("forward");
  mom.push_back("backward");
  mom.push_back("all");

  std::vector<TString> vertex;
  vertex.push_back("all");
  vertex.push_back("sampipe");

  TString part;

  for(Int_t i=0; i<sector.size(); i++){
    for(Int_t j=0; j<pid.size(); j++){
      for(Int_t k=0; k<ene.size(); k++){
        for(Int_t l=0; l<mom.size(); l++){
          for(Int_t m=0; m<vertex.size(); m++){
            part= Form("pr_%s_%s_%s_%s_%s",\
	                sector[i].Data(),\
		              pid[j].Data(),\
		              ene[k].Data(),\
		              mom[l].Data(),\
		              vertex[m].Data());
                  std::cout<< part.Data() << std::endl;
            h_vz[part] = new TH1D(part+"_vz",\
                         Form("%s_vz rate-weighted vertex, Generator=%s", part.Data(), gen.Data()),\
                         360, -6000, 30000);
            h_vrvz[part] = new TH2D(part+"_vrvz",\
                           Form("%s_vrvz rate-weighted vertex, Generator=%s", part.Data(), gen.Data()),\
                           360, -6000, 30000, 200, 0, 2000);
            h_xy[part] = new TH2D(part+"_xy",\
                         Form("%s_xy rate-weighted distribution, Generator=%s", part.Data(), gen.Data()),\
                         640, -1600, 1600, 640, -1600, 1600);
            h_r[part]= new TH1D(part+"_r",\
                       Form("%s_r rate-weighted distribution, Generator=%s", part.Data(), gen.Data()),\
                       400, 0, 2000);          
          }
        }
      }
    }
  }


  Double_t fRate=0;
  remollEvent_t *fEvent=0;
  std::vector<remollGenericDetectorHit_t>  *fHit=0;
  std::vector<remollEventParticle_t> *fPart=0;

  T.SetBranchAddress("ev", &fEvent);
  T.SetBranchAddress("hit", &fHit);
  T.SetBranchAddress("rate", &fRate);
  T.SetBranchAddress("part", &fPart);

  for (size_t j=0;j< nEvents;j++){
    T.GetEntry(j);
    for (size_t i=0;i<fHit->size();i++){
      remollGenericDetectorHit_t hit=fHit->at(i);
      Bool_t hit_planedet = hit.det==28;
      if (!hit_planedet) { continue; }

	    Double_t sepdiv=2*TMath::Pi()/7.0;
      Int_t sec=0;
   	  Double_t phi=atan2(hit.y,hit.x);
   	  if (phi<0) {phi+=2*TMath::Pi();}
  
   	  Double_t secphi = fmod(phi, 2*TMath::Pi()/7);
  	  if (secphi<TMath::Pi()/28.0){sec=1;}           // closed 
  	  else if (secphi<3*TMath::Pi()/28.0){sec=2; }    // transition
  	  else if (secphi<5*TMath::Pi()/28.0) {sec=3;}   // open
  	  else if (secphi<7*TMath::Pi()/28.0) {sec=2;}   // transition
  	  else {sec=1;}  //closed 
                
      Bool_t bool_sector;
      Double_t rate=0;
	    std::map<TString,Bool_t> pid_cut;
	    std::map<TString,Bool_t> sector_cut{ {"all", 1},\
	                                         {"closed", sec==1},\
					                                 {"trans", sec==2},\
					                                 {"open", sec==3} };
	    std::map<TString,Bool_t> mom_cut{ {"forward", hit.pz>=0},\
                                        {"backward", hit.pz<0},\
                                        {"all", 1} };
      std::map<TString,Bool_t> ene_cut{ {"pless1MeV", hit.p<=1},\
                                        {"p1to10MeV", hit.p>1 && hit.p<=10},\
                                        {"pgreater10MeV", hit.p>10} };
      std::map<TString,Bool_t> radpos_cut{ {"ring1", hit.r>650 && hit.r<=680},\
                                           {"ring2", hit.r>680 && hit.r<=740},\
                                           {"ring3", hit.r>740 && hit.r<=800},\
                                           {"ring4", hit.r>800 && hit.r<=920},\
                                           {"ring5", hit.r>920 && hit.r<=1060},\
                                           {"ring6", hit.r>1060 && hit.r<=1160},\
                                             {"PMT", hit.r>1160 && hit.r<=1600} };
      std::map<TString,Bool_t> vertex_cut{ {"all", 1},\
                                           {"sampipe", hit.vz>=25188.010 && hit.vz<=26720.141 &&\
                                                       sqrt(hit.vx*hit.vx && hit.vy*hit.vy)<=750} }; 
  
      if (gen=="moller"){
	      pid_cut["primary_electron"]= hit.pid==11 && hit.trid<=2;
	      pid_cut["secondary_electron"]= hit.pid==11 && hit.trid>2;
        rate = fRate*1.602*1e-13;
	    } else if (gen=="elastic"){
        pid_cut["primary_electron"]= hit.pid==11 && hit.trid<=1;
	      pid_cut["secondary_electron"]= hit.pid==11 && hit.trid>1;
        rate = fRate*1.602*1e-13;
      } else if (gen=="beam"){
        pid_cut["primary_electron"]= hit.pid==11 && hit.vz<=-3875;
        pid_cut["secondary_electron"]= hit.pid==11 && hit.vz>-3875;
        rate = 85*1.0/nEvents;
	    }
	    pid_cut["positron"] = hit.pid==-11;
	    pid_cut["photon"] = hit.pid==22;
      pid_cut["neutron"] = hit.pid==2112;
	    pid_cut["all"] = 1;
    
                  
      Bool_t combined_cut;
      for (Int_t i=0; i<sector.size();i++){
	      for  (Int_t j=0;j<pid.size();j++){
          for(Int_t k=0;k<ene.size();k++){
            for(Int_t l=0;l<mom.size();l++){
              for(Int_t m=0; m<vertex.size();m++){
                part= Form("pr_%s_%s_%s_%s_%s",\
		                        sector[i].Data(),\
		                        pid[j].Data(),\
		                        ene[k].Data(),\
		                        mom[l].Data(),\
		                        vertex[m].Data()); 
                combined_cut  = sector_cut[sector[i]]  &&\
		                            pid_cut[pid[j]] &&\
		                            ene_cut[ene[k]] &&\
				                        mom_cut[mom[l]] &&\
				                        vertex_cut[vertex[m]] &&\
				                        radpos_cut[radpos] ;

                if (combined_cut){ 
		              h_vz[part]->Fill(hit.vz, rate*weight);
                  h_vrvz[part]->Fill(hit.vz, sqrt(hit.vx*hit.vx+hit.vy*hit.vy), rate*weight);
	                h_xy[part]->Fill(hit.vx, hit.vy, rate*weight);
		              h_r[part]->Fill(hit.r, rate*weight);
		            }
              }
	         }  
	      }
	    }
	  }

  }
}



for (Int_t i=0; i<sector.size();i++){
  for (Int_t j=0;j<pid.size();j++){
    for(Int_t k=0;k<ene.size();k++){
      for(Int_t l=0;l<mom.size();l++){
        for(Int_t m=0; m<vertex.size();m++){
          part= Form("pr_%s_%s_%s_%s_%s",\
          sector[i].Data(),\
          pid[j].Data(),\
          ene[k].Data(),\
          mom[l].Data(),\
          vertex[m].Data());

          h_vz[part]->Write("", TObject::kOverwrite); 
          h_vrvz[part]->Write("", TObject::kOverwrite);
	        h_xy[part]->Write("", TObject::kOverwrite);
          h_r[part]->Write("", TObject::kOverwrite);
	      }
      }
    }
  }  
}

return 0;

}
