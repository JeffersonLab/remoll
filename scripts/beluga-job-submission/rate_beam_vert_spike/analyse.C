using namespace ROOT;

int analyse(TString source, TString out, TString gen, Float_t min_open, Float_t max_open, Float_t min_trans, Float_t max_trans, Float_t min_closed, Float_t max_closed){

TChain T("T");
T.Add(Form("%s", source.Data())); // Adding source file
Int_t nEvents= T.GetEntries();  // Number of primary events
std::cout << "Analyzing "<< nEvents << "events" << std::endl;
Double_t weight= 1.0/85; // Divide by current. So, the Y-axis gets units of Hz/uA. 

TFile f(Form("%s", out.Data()), "RECREATE");

std::map<TString, TH1D*> h_vz;
std::map<TString, TH2D*> h_vrvz;
std::map<TString, TH2D*> h_vyvx;
std::map<TString, TH2D*> h_xy;
std::map<TString, TH1D*> h_r;
std::map<TString, TH1D*> h_ph;

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

std::vector<TString> vertex;
vertex.push_back("coll1");
vertex.push_back("coll2");
vertex.push_back("lint&coll6A");
vertex.push_back("coll6B");
vertex.push_back("collar1");

std::vector<TString> radposMD;
radposMD.push_back("in");
radposMD.push_back("out");
radposMD.push_back("ring5");



TString part;


for(Int_t j=0; j<sector.size(); j++){
 for(Int_t i=0; i<pid.size();i++){
  for(Int_t l=0;l<ene.size();l++){
   for(Int_t k=0; k<radposMD.size();k++){
    part= Form("pr_%s_%s_%s_%s", sector[j].Data(), pid[i].Data(), ene[l].Data(), radposMD[k].Data());
    std::cout<< part.Data() << std::endl;
    h_vz[part]=new TH1D(part+"_vz", Form("%s_vz rate-weighted vertex, Generator=%s", part.Data(), gen.Data()), 360, -6000, 30000);
    h_vrvz[part]=new TH2D(part+"_vrvz", Form("%s_vrvz rate-weighted vertex, Generator=%s", part.Data(), gen.Data()), 360, -6000, 30000, 200, 0, 2000);
 
   } 
   
   for(Int_t k=0; k<vertex.size();k++){
    part= Form("pr_%s_%s_%s_%s", sector[j].Data(), pid[i].Data(), ene[l].Data(),  vertex[k].Data());
    std::cout<< part.Data() << std::endl;
    h_xy[part]=new TH2D(part+"_xy", Form("%s_xy rate-weighted distribution MD, Generator=%s", part.Data(), gen.Data() ), 520, -1300, 1300, 520, -1300, 1300);
    h_r[part]=new TH1D(part+"_r", Form("%s_r rate-weighted distribution MD, Generator=%s", part.Data(), gen.Data()), 400, 0, 2000);
    h_ph[part]=new TH1D(part+"_ph", Form("%s_ph rate-weighted distribution MD, Generator=%s", part.Data(), gen.Data()), 400,-4,4);
   }

   for(Int_t k=0; k<radposMD.size();k++){
      for(Int_t k1=0; k1<vertex.size();k1++){
          part= Form("pr_%s_%s_%s_%s_%s", sector[j].Data(), pid[i].Data(), ene[l].Data(), radposMD[k].Data(), vertex[k1].Data());   
	  h_vyvx[part]=new TH2D(part+"_vyvx", Form("%s_vyvx rate-weighted vertex, Generator=%s", part.Data(), gen.Data()), 400, -1600, 1600, 400, -1600, 1600);
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
                

        TVector2 xy(hit.x, hit.y);
        Bool_t bool_sector;
	Bool_t septant;
        Double_t rate=0;
	std::map<TString,Bool_t> particle_id;
        std::map<TString,Bool_t> ene_cut{ {"pless1MeV",hit.p<=1}, {"p1to10MeV", hit.p>1 && hit.p<=10}, {"pgreater10MeV", hit.p>10} };
        std::map<TString,Bool_t> radposMD_cut{ {"in",hit.r<=650}, {"out", hit.r>650}, {"ring5", hit.r>900 && hit.r<1060 }};
        std::map<TString,Bool_t> vertex_cut;
        vertex_cut["coll1"]= hit.vz>175 && hit.vz<=750;
        vertex_cut["coll2"]= hit.vz>750 && hit.vz<=900;
        vertex_cut["lint&coll6A"]= hit.vz>9500 && hit.vz<=9600;
        vertex_cut["coll6B"] = hit.vz>11000 && hit.vz<=11150;
        vertex_cut["collar1"] = hit.vz>11650 && hit.vz<=12250; 
  
        if (gen=="moller"){
	      particle_id["primary_electron"]= hit.pid==11 && hit.trid<=2;
	      particle_id["secondary_electron"]= hit.pid==11 && hit.trid>2;
              rate = fRate*1.602*1e-13;
	} else if (gen=="elastic"){
              particle_id["primary_electron"]= hit.pid==11 && hit.trid<=1;
	      particle_id["secondary_electron"]= hit.pid==11 && hit.trid>1;
              rate = fRate*1.602*1e-13;
        } else if (gen=="beam"){
              particle_id["primary_electron"]= hit.pid==11 && hit.vz<=-3875;
              particle_id["secondary_electron"]= hit.pid==11 && hit.vz>-3875;
              rate = 85*1.0/nEvents;
	}
	particle_id["positron"]  = hit.pid==-11;
	particle_id["photon"]    = hit.pid==22;
        particle_id["neutron"]   = hit.pid==2112;
	particle_id["all"]       = 1;

       
               
        std::map<TString, Bool_t> hit_pid;
        for (Int_t j=0; j<sector.size();j++){
	 for  (Int_t i=0;i<pid.size();i++){
          for(Int_t l=0;l<ene.size();l++){
           if(j==0) { bool_sector=1;}else{bool_sector= sec==j;};

           for(Int_t k=0; k<radposMD.size();k++){
               part= Form("pr_%s_%s_%s_%s", sector[j].Data(), pid[i].Data(), ene[l].Data(), radposMD[k].Data()); 
               hit_pid[part] =  bool_sector  && particle_id[pid[i]] && ene_cut[ene[l]] && radposMD_cut[radposMD[k]];
               if (hit_pid[part]){ 
                   std::cout<< part.Data() << std::endl;
		   
                   h_vz[part]->Fill(hit.vz, rate*weight);
                   h_vrvz[part]->Fill(hit.vz, sqrt(hit.vx*hit.vx+hit.vy*hit.vy), rate*weight);
	           for (Int_t k1=0;k1<vertex.size();k1++){
		      part = Form("pr_%s_%s_%s_%s_%s", sector[j].Data(), pid[i].Data(), ene[l].Data(), radposMD[k].Data(),vertex[k1].Data());
                      h_vyvx[part]->Fill(hit.vx, hit.vy, rate*weight);
		   }
               }
           }

           for(Int_t k=0; k<vertex.size();k++){
               part= Form("pr_%s_%s_%s_%s", sector[j].Data(), pid[i].Data(), ene[l].Data(), vertex[k].Data());
               hit_pid[part] =  bool_sector  && particle_id[pid[i]] && ene_cut[ene[l]] && vertex_cut[vertex[k]];
               if (hit_pid[part]){
                   std::cout<< part.Data() << std::endl;
                   
                   h_xy[part]->Fill(hit.x, hit.y, rate*weight);
                   h_r[part]->Fill(hit.r, rate*weight);
                   h_ph[part]->Fill(hit.ph, rate*weight);

               }
           }


                 
          }
         }		
        }
        



    }
}


for (Int_t j=0; j<sector.size(); j++){
 for(Int_t i=0; i<pid.size();i++){
  for(Int_t l=0;l<ene.size();l++){
   for(Int_t k=0; k<radposMD.size();k++){
     part= Form("pr_%s_%s_%s_%s", sector[j].Data(), pid[i].Data(), ene[l].Data(), radposMD[k].Data());
     h_vz[part]->Write("", TObject::kOverwrite); 
     h_vrvz[part]->Write("", TObject::kOverwrite);
   }
   for(Int_t k=0; k<vertex.size();k++){
     part= Form("pr_%s_%s_%s_%s", sector[j].Data(), pid[i].Data(), ene[l].Data(),  vertex[k].Data());
     h_xy[part]->Write("", TObject::kOverwrite);
     h_r[part]->Write("", TObject::kOverwrite);
     h_ph[part]->Write("", TObject::kOverwrite);
   }
  }
 }
}

return 0;

}
