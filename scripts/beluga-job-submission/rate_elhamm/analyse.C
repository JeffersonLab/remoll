using namespace ROOT;

int analyse(TString source, TString out, TString gen, Float_t min_open, Float_t max_open, Float_t min_trans, Float_t max_trans, Float_t min_closed, Float_t max_closed){

TChain T("T");
T.Add(Form("%s", source.Data())); // Adding source file
Int_t nEvents= T.GetEntries();  // Number of primary events
std::cout << "Analyzing "<< nEvents << "events" << std::endl;
Double_t weight= 1e-9/85/7; // Divide by current and number of septants. So, the Y-axis gets units of GHz/uA/sep. 


TFile f(Form("%s", out.Data()), "RECREATE");

std::map<TString, TH1D*> h;


std::vector<TString> sector;
sector.push_back("all");
sector.push_back("closed");
sector.push_back("trans");
sector.push_back("open");

Int_t n_septant=7;
Double_t size_septant=2.0*TMath::Pi()/n_septant;
std::vector<Double_t> off_septant;
TString part;
for (Int_t i=0; i<n_septant; i++){
  off_septant.push_back((3.0-1.0*i)*size_septant);
}

for(Int_t j=0; j<sector.size(); j++){
    part= Form("pr_%s", sector[j].Data());
    std::cout<< part<< std::endl;
    h[part]=new TH1D(part, Form("%s rate, Generator=%s, Range= Open[%3.0f-%3.0f mm], Trans[%3.0f-%3.0f mm], Closed[%3.0f-%3.0f mm]", part.Data(), gen.Data(), min_open, max_open, min_trans, max_trans, min_closed, max_closed), 400, 0, 2000);
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
               
                Bool_t hit_planedet = hit.det==28 ;
                
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

                Bool_t hit_radial=0;
		Bool_t hit_pidcut= hit.pid==-11 || hit.pid==11 || hit.pid==211 || hit.pid==-211 || hit.pid==13 || hit.pid==-13;
                                         

                /**** The radial ranges of open, closed and transition sector ****/		
                if (sec==1) {		
			hit_radial=hit.r>=min_closed && hit.r<max_closed;
		}
                if (sec==2) {                
                        hit_radial=hit.r>=min_trans && hit.r<max_trans;
                }
                if (sec==3) {
                        hit_radial=hit.r>=min_open && hit.r<max_open;
                }
                
                if (!hit_planedet) { continue; }



                TVector2 xy(hit.x, hit.y);
                Bool_t septant;
                Bool_t bool_sector;
                
		std::map<TString, Bool_t> hit_pid;
                for (Int_t i=0; i<n_septant; i++){
                    TVector2 XY=xy.Rotate(off_septant[i]);
                    septant= atan(XY.Y()/XY.X())>= -size_septant/2.0 && atan(XY.Y()/XY.X()) < size_septant/2.0 && XY.X()<0;
                    for (Int_t j=0; j<sector.size();j++){
		         part= Form("pr_%s", sector[j].Data());
                         if(j==0) { bool_sector=1;} else {bool_sector= sec==j;};
                         hit_pid[part]=septant && bool_sector && hit_radial && hit_pidcut ;
		  	if (hit_pid[part]){ 
                           h[part]->Fill(hit.r, (fRate)*weight);
                        }
                     }
		 } 



       }
}


for (Int_t j=0; j<sector.size(); j++){
     part= Form("pr_%s", sector[j].Data());
     std::cout<< part << std::endl;
     h[part]->Write("", TObject::kOverwrite); 
}

return 0;

}

