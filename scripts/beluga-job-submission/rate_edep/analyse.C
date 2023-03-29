using namespace ROOT;

int analyse(TString source, TString out, TString gen, Double_t open_min, Double_t open_max, Double_t trans_min, Double_t trans_max, Double_t closed_min, Double_t closed_max){

TChain T("T");
T.Add(Form("%s", source.Data())); // Adding source file
Int_t nEvents = T.GetEntries();  // Number of primary events
std::cout << "Analyzing "<< nEvents << "events" << std::endl;
Double_t weight=1;
if (gen!="beam"){
   weight= 1e-9/85/7; // Divide by current and number of septants. So, the Y-axis gets units of GHz/uA/sep. 
}else{
	
   weight=1.0/nEvents; // 1e-9/(nEvents*1.602*1e-13*7);
}

TFile f(Form("%s", out.Data()), "RECREATE");

std::map<TString, TH2D*> h_u_rz;
std::map<TString, TH2D*> h_d_rz;
std::map<TString, TH2D*> h_ue_rz;
std::map<TString, TH2D*> h_de_rz;
std::map<TString, TH2D*> h_u_xy;
std::map<TString, TH2D*> h_d_xy;
std::map<TString, TH2D*> h_ue_xy;
std::map<TString, TH2D*> h_de_xy;
std::map<TString, TH2D*> h_col1_xy;
std::map<TString, TH2D*> h_col2_xy;
std::map<TString, TH2D*> h_col4_xy;
std::map<TString, TH2D*> h_col5_xy;

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
  h_u_rz[part]=new TH2D(part+"_u_rz", Form("%s upstream edep, Generator=%s", part.Data(), gen.Data()), 480, 800, 3200, 60, 0, 300);
  h_d_rz[part]=new TH2D(part+"_d_rz", Form("%s downstream edep, Generator=%s", part.Data(), gen.Data()), 1500, 4800, 12300, 90, 0, 450);
  h_ue_rz[part]=new TH2D(part+"_ue_rz", Form("%s upstream epoxy edep, Generator=%s", part.Data(), gen.Data()), 480, 800, 3200, 60, 0, 300);
  h_de_rz[part]=new TH2D(part+"_de_rz", Form("%s downstream epoxy edep, Generator=%s", part.Data(), gen.Data()), 1500, 4800, 12300, 90, 0, 450);
 }
 part= Form("pr_E%d", k);
 h_col1_xy[part]=new TH2D(part+"_col1_xy", Form("%s col1 edep, Generator=%s", part.Data(), gen.Data()), 100, -250, 250, 100, -250, 250);
 h_col2_xy[part]=new TH2D(part+"_col2_xy", Form("%s col2 edep, Generator=%s", part.Data(), gen.Data()), 100, -250, 250, 100, -250, 250);
 h_col4_xy[part]=new TH2D(part+"_col4_xy", Form("%s col4 edep, Generator=%s", part.Data(), gen.Data()), 100, -250, 250, 100, -250, 250);
 h_col5_xy[part]=new TH2D(part+"_col5_xy", Form("%s col5 edep, Generator=%s", part.Data(), gen.Data()), 100, -250, 250, 100, -250, 250);
 h_u_xy[part]=new TH2D(part+"_u_xy", Form("%s upstream edep, Generator=%s", part.Data(), gen.Data()), 180, -450, 450, 180, -450, 450);
 h_d_xy[part]=new TH2D(part+"_d_xy", Form("%s downstream edep, Generator=%s", part.Data(), gen.Data()), 180, -450, 450, 180, -450, 450);
 h_ue_xy[part]=new TH2D(part+"_ue_xy", Form("%s upstream edep, Generator=%s", part.Data(), gen.Data()), 180, -450, 450, 180, -450, 450);
 h_de_xy[part]=new TH2D(part+"_de_xy", Form("%s downstream edep, Generator=%s", part.Data(), gen.Data()), 180, -450, 450, 180, -450, 450);

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
	
                
		std::map<TString, Bool_t> hit_pid;
		for (Int_t k=0;k<ene_bins;k++){
                  if(k==0){ ene_cut= hit.p<1; }
		  else if (k==1) { ene_cut=hit.p>=1 && hit.p<10;}
		  else if (k==2) { ene_cut=hit.p>=10 && hit.p<100;}
		  else{ ene_cut = hit.p>= 100;}
		  

                  for (Int_t i=0; i<n_septant; i++){
		         part= Form("pr_%d_E%d",i+1,k);
                         hit_pid[part]= ene_cut ;
			 if (hit_pid[part]){
				if(hit.det==(4001+i)){
                                    h_u_rz[part]->Fill(hit.z, hit.r, hit.edep*(fRate)*weight);
				    
			        }
			        if(hit.det==(3001+i)){
				    h_d_rz[part]->Fill(hit.z, hit.r, hit.edep*(fRate)*weight);
				}
				if(hit.det==(4008+i)){
				    h_ue_rz[part]->Fill(hit.z, hit.r, hit.edep*(fRate)*weight);
				}
				if(hit.det==(3008+i)){
				    h_de_rz[part]->Fill(hit.z, hit.r, hit.edep*(fRate)*weight);
	                         }			    
                         }

                    }
		    part= Form("pr_E%d",k);
		    hit_pid[part]=  ene_cut ;
                    if(hit_pid[part]){
                            if(hit.det>=4001 && hit.det<=4007){
			          h_u_xy[part]->Fill(hit.x, hit.y, hit.edep*(fRate)*weight);
		  	    }
			    if(hit.det>=3001 && hit.det <=3007){
			          h_d_xy[part]->Fill(hit.x, hit.y, hit.edep*(fRate)*weight);
			     }
			     if(hit.det>=4008 && hit.det<=4014){
			          h_ue_xy[part]->Fill(hit.x, hit.y, hit.edep*(fRate)*weight);
			     }
			     if(hit.det>=3008 && hit.det<=3014){
			          h_de_xy[part]->Fill(hit.x, hit.y, hit.edep*(fRate)*weight);
			     }             
			      if(hit.det==2001 ||  hit.det==2006){
			           h_col1_xy[part]->Fill(hit.x, hit.y, hit.edep*(fRate)*weight);
			      }
			    
			      if(hit.det==2002||hit.det==2008){
				   h_col2_xy[part]->Fill(hit.x, hit.y, hit.edep*(fRate)*weight);
                              }
                              if(hit.det==2004){
				   h_col4_xy[part]->Fill(hit.x, hit.y, hit.edep*(fRate)*weight);
		              }
			     if(hit.det==2005){
				   h_col5_xy[part]->Fill(hit.x, hit.y, hit.edep*(fRate)*weight);
		             }

	             }		    

		  }  
				
       }
}


for(Int_t k=0; k<energy_bins;k++){
  for (Int_t i=0; i<n_septant; i++){
     part= Form("pr_%d_E%d", i+1,k);
     h_u_rz[part]->Write("", TObject::kOverwrite); 
     h_d_rz[part]->Write("", TObject::kOverwrite);
     h_ue_rz[part]->Write("", TObject::kOverwrite);
     h_de_rz[part]->Write("", TObject::kOverwrite);
  }
  part= Form("pr_E%d", k);
  h_u_xy[part]->Write("", TObject::kOverwrite);
  h_d_xy[part]->Write("", TObject::kOverwrite);
  h_ue_xy[part]->Write("", TObject::kOverwrite);
  h_de_xy[part]->Write("", TObject::kOverwrite);
  h_col1_xy[part]->Write("", TObject::kOverwrite);
  h_col2_xy[part]->Write("", TObject::kOverwrite);
  h_col4_xy[part]->Write("", TObject::kOverwrite);
  h_col5_xy[part]->Write("", TObject::kOverwrite);

}

return 0;
}
