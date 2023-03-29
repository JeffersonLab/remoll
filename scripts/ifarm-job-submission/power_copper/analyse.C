using namespace ROOT;


int analyse(TString source, TString out, TString gen, Double_t open_min, Double_t open_max, Double_t trans_min, Double_t trans_max, Double_t closed_min, Double_t closed_max){

TChain T("T");
T.Add(Form("%s", source.Data())); // Adding source file
Int_t nEvents = T.GetEntries();  // Number of primary events
std::cout << "Analyzing "<< nEvents << " events" << std::endl;
Double_t weight=1;
if (gen!="beam"){
   weight= 1e-9/85/7; // Divide by current and number of septants. So, the Y-axis gets units of GHz/uA/sep. 
}else{
	
   weight=1.0/nEvents; // 1e-9/(nEvents*1.602*1e-13*7);
}

TFile f(Form("%s", out.Data()), "RECREATE");

std::map<TString, TH2D*> h_ue_rz_left;
std::map<TString, TH2D*> h_de_rz_left;
std::map<TString, TH2D*> h_ue_rz_right;
std::map<TString, TH2D*> h_de_rz_right;
std::map<TString, TH2D*> h_ue_phz_bottom;
std::map<TString, TH2D*> h_de_phz_bottom;
std::map<TString, TH2D*> h_de_phph_nose;
std::map<TString, TH1D*> h_ue_rz_left_1D;
std::map<TString, TH1D*> h_de_rz_left_1D;
std::map<TString, TH1D*> h_ue_rz_right_1D;
std::map<TString, TH1D*> h_de_rz_right_1D;
std::map<TString, TH1D*> h_ue_phz_bottom_1D;
std::map<TString, TH1D*> h_de_phz_bottom_1D;
std::map<TString, TH1D*> h_de_phph_nose_1D;




Int_t n_septant=7;
Double_t size_septant=2.0*TMath::Pi()/n_septant;
std::vector<Double_t> off_septant;
TString part;


Int_t energy_bins=4;

for(Int_t k=0;k<energy_bins;k++){
 for(Int_t i=0; i<n_septant; i++){
  off_septant.push_back((3.0-1.0*i)*size_septant);
  part= Form("pr_%d_E%d",i+1,  k);
  std::cout<< part<< std::endl;
  h_ue_rz_left[part]=new TH2D(part+"_ue_rz_left", Form("%s upstream edep, Generator=%s", part.Data(), gen.Data()), 120, 800, 3200, 60, 0, 60);
  h_de_rz_left[part]=new TH2D(part+"_de_rz_left", Form("%s downstream edep, Generator=%s", part.Data(), gen.Data()), 375, 4800, 12300, 450, 0, 450);
  h_ue_rz_right[part]=new TH2D(part+"_ue_rz_right", Form("%s upstream epoxy edep, Generator=%s", part.Data(), gen.Data()), 120, 800, 3200, 60, 0, 60);
  h_de_rz_right[part]=new TH2D(part+"_de_rz_right", Form("%s downstream epoxy edep, Generator=%s", part.Data(), gen.Data()), 375, 4800, 12300, 450, 0, 450);
  h_ue_phz_bottom[part]=new TH2D(part+"_ue_phz_bottom", Form("%s upstream edep, Generator=%s", part.Data(), gen.Data()), 120, 800, 3200, 50 , -25.0, 25.0);
  h_de_phz_bottom[part]=new TH2D(part+"_de_phz_bottom", Form("%s downstream edep, Generator=%s", part.Data(), gen.Data()), 375, 4800, 12300, 50, -25.0, 25.0);
  h_de_phph_nose[part]=new TH2D(part+"_de_phph_nose", Form("%s downstream edep, Generator=%s", part.Data(), gen.Data()), 450, -225, 225, 50, -25.0, 25.0);
  h_ue_rz_left_1D[part]=new TH1D(part+"_ue_rz_left_1D", Form("%s upstream epoxy edep, Generator=%s", part.Data(), gen.Data()), 120, 800, 3200);
  h_de_rz_left_1D[part]=new TH1D(part+"_de_rz_left_1D", Form("%s downstream epoxy edep, Generator=%s", part.Data(), gen.Data()), 375, 4800, 12300);
  h_ue_rz_right_1D[part]=new TH1D(part+"_ue_rz_right_1D", Form("%s upstream edep, Generator=%s", part.Data(), gen.Data()), 120, 800, 3200);
  h_de_rz_right_1D[part]=new TH1D(part+"_de_rz_right_1D", Form("%s downstream edep, Generator=%s", part.Data(), gen.Data()), 375, 4800, 12300);
  h_ue_phz_bottom_1D[part]=new TH1D(part+"_ue_phz_bottom_1D", Form("%s upstream epoxy edep, Generator=%s", part.Data(), gen.Data()), 120, 800, 3200);
  h_de_phz_bottom_1D[part]=new TH1D(part+"_de_phz_bottom_1D", Form("%s downstream epoxy edep, Generator=%s", part.Data(), gen.Data()), 375, 4800, 12300);
  h_de_phph_nose_1D[part]=new TH1D(part+"_de_phph_nose_1D", Form("%s downstream edep, Generator=%s", part.Data(), gen.Data()), 450, -225, 225);


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
		if(gen=="beam"){
			fRate=1;
	        }
                remollGenericDetectorHit_t hit=fHit->at(i);
               
                Bool_t ene_cut; 
                Int_t ene_bins=4 ;

		TVector2 xy(hit.x, hit.y);
		TVector2 XY(hit.x, hit.y);
                
		std::map<TString, Bool_t> hit_pid;
		for (Int_t k=0;k<ene_bins;k++){
                  if(k==0){ ene_cut= hit.p<1; }
		  else if (k==1) { ene_cut=hit.p>=1 && hit.p<10;}
		  else if (k==2) { ene_cut=hit.p>=10 && hit.p<100;}
		  else{ ene_cut = hit.p>= 100;}

                  for (Int_t i=0; i<n_septant; i++){
		         part= Form("pr_%d_E%d",i+1,k);
			 XY= xy.Rotate(-i*size_septant);
                         hit_pid[part]= ene_cut ;
			 if (hit_pid[part]){
				if(hit.det==(4001+i)){
                                  h_ue_rz_left[part]->Fill(hit.z, XY.X(), hit.edep*(fRate)*weight);
                                  h_ue_phz_bottom[part]->Fill(hit.z, XY.Y(), hit.edep*(fRate)*weight);
                                  h_ue_phz_bottom_1D[part]->Fill(hit.z, hit.edep*(fRate)*weight);
				}
				if(hit.det==(3001+i)){
                                  if(hit.z<5939){
                                    h_de_rz_left[part]->Fill(hit.z, XY.X(), hit.edep*(fRate)*weight);
                                    h_de_phz_bottom[part]->Fill(hit.z, XY.Y(), hit.edep*(fRate)*weight);
                                    h_de_phz_bottom_1D[part]->Fill(hit.z, hit.edep*(fRate)*weight);
				  }else if(hit.z>=5938 && hit.z<6973){
                                    h_de_rz_left[part]->Fill(hit.z, XY.X(), hit.edep*(fRate)*weight);
                                    h_de_phz_bottom[part]->Fill(hit.z, XY.Y(), hit.edep*(fRate)*weight);
                                    h_de_phz_bottom_1D[part]->Fill(hit.z, hit.edep*(fRate)*weight);
				  }else if(hit.z>=6973 && hit.z<7965){
                                    h_de_rz_left[part]->Fill(hit.z, XY.X(), hit.edep*(fRate)*weight);
                                    h_de_phz_bottom[part]->Fill(hit.z, XY.Y(), hit.edep*(fRate)*weight);
                                    h_de_phz_bottom_1D[part]->Fill(hit.z, hit.edep*(fRate)*weight);
				  }else{
                                    h_de_rz_left[part]->Fill(hit.z, XY.X(), hit.edep*(fRate)*weight);
                                    h_de_phz_bottom[part]->Fill(hit.z, XY.Y(), hit.edep*(fRate)*weight);
                                    h_de_phz_bottom_1D[part]->Fill(hit.z, hit.edep*(fRate)*weight);
				  }
				}   
                         }
                      }
		   }
       }
}


for(Int_t k=0; k<energy_bins;k++){
  for (Int_t i=0; i<n_septant; i++){
     part= Form("pr_%d_E%d", i+1,k);
     h_ue_rz_left[part]->Write("", TObject::kOverwrite); 
     h_de_rz_left[part]->Write("", TObject::kOverwrite);
     h_ue_rz_right[part]->Write("", TObject::kOverwrite);
     h_de_rz_right[part]->Write("", TObject::kOverwrite);
     h_ue_phz_bottom[part]->Write("", TObject::kOverwrite);
     h_de_phz_bottom[part]->Write("", TObject::kOverwrite);
     h_de_phph_nose[part]->Write("", TObject::kOverwrite);
     h_ue_rz_left_1D[part]->Write("", TObject::kOverwrite);
     h_de_rz_left_1D[part]->Write("", TObject::kOverwrite);
     h_ue_rz_right_1D[part]->Write("", TObject::kOverwrite);
     h_de_rz_right_1D[part]->Write("", TObject::kOverwrite);
     h_ue_phz_bottom_1D[part]->Write("", TObject::kOverwrite);
     h_de_phz_bottom_1D[part]->Write("", TObject::kOverwrite);
     h_de_phph_nose_1D[part]->Write("", TObject::kOverwrite);
  }
}

return 0;
}
