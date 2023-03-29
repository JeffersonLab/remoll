using namespace ROOT;

int analyse(TString source, TString out, TString gen, Float_t min_open, Float_t max_open, Float_t min_trans, Float_t max_trans, Float_t min_closed, Float_t max_closed){

TChain T("T");
T.Add(Form("%s", source.Data())); // Adding source file
Int_t nEvents= T.GetEntries();  // Number of primary events
std::cout << "Analyzing "<< nEvents << "events" << std::endl;
Double_t weight= 1e-9/85/7; // Divide by current and number of septants. So, the Y-axis gets units of GHz/uA/sep. 


TFile f(Form("%s", out.Data()), "RECREATE");

std::map<TString, TH1D*> h;
std::map<TString, TH1D*> h_fom;
std::map<TString, TH1D*> h_asy;
std::map<TString, TH1D*> h_tcom;
std::map<TString, TH1D*> h_th;
std::map<TString, TH1D*> h_ph;
std::map<TString, TH1D*> h_ene;
std::map<TString, TH1D*> h_q2;
std::map<TString, TH2D*> h_xy;
std::map<TString, TH2D*> h_rt;  // rate vs theta
std::map<TString, TH1D*> h_rA; //asymmetry weighted rate


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
    h_rA[part]=new TH1D(part+"_rA", Form("%s asymmetry weigthed rate, Generator=%s, Range= Open[%3.0f-%3.0f mm], Trans[%3.0f-%3.0f mm], Closed[%3.0f-%3.0f mm]", part.Data(), gen.Data(), min_open, max_open, min_trans, max_trans, min_closed, max_closed), 400, 0, 2000);
    h_xy[part]=new TH2D(part+"_xy", Form("%s xy, Generator=%s, Range= Open[%3.0f-%3.0f mm], Trans[%3.0f-%3.0f mm], Closed[%3.0f-%3.0f mm]", part.Data(), gen.Data(), min_open, max_open, min_trans, max_trans, min_closed, max_closed), 520, -1300, 1300, 520, -1300, 1300);
    h_rt[part]=new TH2D(part+"_rt",Form("%s rt, Generator=%s, Range= Open[%3.0f-%3.0f mm], Trans[%3.0f-%3.0f mm], Closed[%3.0f-%3.0f mm]", part.Data(), gen.Data(), min_open, max_open, min_trans, max_trans, min_closed, max_closed),140,600.0,1300.0,100, 0.0,0.03);
   h_fom[part]=new TH1D(part+"_fom", Form("%s FOM, Generator=%s, Range =  Open[%3.0f-%3.0f mm], Trans[%3.0f-%3.0f mm], Closed[%3.0f-%3.0f mm]", part.Data(), gen.Data(), min_open, max_open, min_trans, max_trans, min_closed, max_closed), 400, 0, 2000);
   h_th[part]=new TH1D(part+"_th", Form("%s th, Generator=%s, Range =  Open[%3.0f-%3.0f mm], Trans[%3.0f-%3.0f mm], Closed[%3.0f-%3.0f mm]", part.Data(), gen.Data(), min_open, max_open, min_trans, max_trans, min_closed, max_closed), 100, 0, 0.03);
    h_ph[part]=new TH1D(part+"_ph", Form("%s ph, Generator=%s, Range =  Open[%3.0f-%3.0f mm], Trans[%3.0f-%3.0f mm], Closed[%3.0f-%3.0f mm]", part.Data(), gen.Data(), min_open, max_open, min_trans, max_trans, min_closed, max_closed), 540, -27, 27);
    h_ene[part]=new TH1D(part+"_ene", Form("%s energy, Generator=%s, Range =  Open[%3.0f-%3.0f mm], Trans[%3.0f-%3.0f mm], Closed[%3.0f-%3.0f mm]", part.Data(), gen.Data(), min_open, max_open, min_trans, max_trans, min_closed, max_closed), 115, 0, 11500);


    if(gen=="moller"){

      h_asy[part]=new TH1D(part+"_asy", Form("%s asy, Generator=%s, Range=  Open[%3.0f-%3.0f mm], Trans[%3.0f-%3.0f mm], Closed[%3.0f-%3.0f mm]", part.Data(),gen.Data(), min_open, max_open, min_trans, max_trans, min_closed, max_closed ), 300, -60, 0);
      h_tcom[part]=new TH1D(part+"_tcom", Form("%s tcom, Generator=%s, Range =  Open[%3.0f-%3.0f mm], Trans[%3.0f-%3.0f mm], Closed[%3.0f-%3.0f mm]", part.Data(), gen.Data(), min_open, max_open, min_trans, max_trans, min_closed, max_closed), 180, 0, 180);

    }else if(gen=="elastic"){

      h_asy[part]=new TH1D(part+"_asy", Form("%s asy, Generator=%s, Range=  Open[%3.0f-%3.0f mm], Trans[%3.0f-%3.0f mm], Closed[%3.0f-%3.0f mm]", part.Data(), gen.Data(), min_open, max_open, min_trans, max_trans, min_closed, max_closed), 50, -150, 0);
      h_q2[part]=new TH1D(part+"_q2", Form("%s q2, Generator=%s, Range =  Open[%3.0f-%3.0f mm], Trans[%3.0f-%3.0f mm], Closed[%3.0f-%3.0f mm]", part.Data(), gen.Data(), min_open, max_open, min_trans, max_trans, min_closed, max_closed), 150, 0, 15000);

    }else{

      h_asy[part] = new TH1D(part+"_asy", Form("%s asy, Generator=%s, Range=  Open[%3.0f-%3.0f mm], Trans[%3.0f-%3.0f mm], Closed[%3.0f-%3.0f mm]", part.Data(), gen.Data(), min_open, max_open, min_trans, max_trans, min_closed, max_closed), 400, -2000, 0);
      h_q2[part]=new TH1D(part+"_q2", Form("%s q2, Generator=%s, Range =  Open[%3.0f-%3.0f mm], Trans[%3.0f-%3.0f mm], Closed[%3.0f-%3.0f mm]", part.Data(), gen.Data(), min_open, max_open, min_trans, max_trans, min_closed, max_closed), 150, 0, 15000);


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
                
                Bool_t hit_cutoff = hit.p<1;    //Cut off all particles with momentum less than 1 MeV
                if (hit_cutoff || !hit_planedet) { continue; }


                Int_t max_track=0; // maximum track of primary
                if (gen=="moller"){
			max_track=2;
                } else{
                        max_track=1;
                }

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
                         hit_pid[part]=septant && bool_sector && hit_radial && hit.trid<=max_track;
		  	if (hit_pid[part]){ 
                           remollEventParticle_t primary=fPart->at(hit.trid-1);
                           h[part]->Fill(hit.r, (fRate)*weight);
                           h_fom[part]->Fill(hit.r, (fRate)*(fEvent->A)*(fEvent->A)*weight);                                     h_asy[part]->Fill(fEvent->A, fRate*weight);
                           h_th[part]->Fill(primary.th, fRate*weight);
                           h_ene[part]->Fill(hit.e, fRate*weight);
                           Double_t ph=atan2(XY.Y(),XY.X());
                           if(ph<0){ ph+=2*TMath::Pi() ;}
                           h_ph[part]->Fill( (ph-TMath::Pi())*180/TMath::Pi(), fRate*weight);
                           if(gen!="moller"){
                             h_q2[part]->Fill(fEvent->Q2, fRate*weight);
                           }else{
                             Double_t thetacom=0;
                             if(hit.trid==2){
                                thetacom=(TMath::Pi()-fEvent->thcom)*(180/TMath::Pi());
                             } else {
                                thetacom=(fEvent->thcom)*(180/TMath::Pi());
                             }
                             h_tcom[part]->Fill(thetacom, fRate*weight);
                           }
			   h_xy[part]->Fill(XY.X(), XY.Y(), fRate*weight);
	                   h_rt[part]->Fill(hit.r, primary.th, fRate*weight);
			   h_rA[part]->Fill(hit.r, (fRate)*weight*(fEvent->A));
                        }
                     }
		 } 



       }
}


for (Int_t j=0; j<sector.size(); j++){
     part= Form("pr_%s", sector[j].Data());
     std::cout<< part << std::endl;
     h[part]->Write("", TObject::kOverwrite); 
     h_fom[part]->Write("", TObject::kOverwrite);
     h_asy[part]->Write("", TObject::kOverwrite);
     h_th[part]->Write("", TObject::kOverwrite);
     h_ph[part]->Write("", TObject::kOverwrite);
     h_ene[part]->Write("", TObject::kOverwrite);
     if(gen !="moller"){
        h_q2[part]->Write("", TObject::kOverwrite);
     }else{
        h_tcom[part]->Write("", TObject::kOverwrite);
     }
     h_xy[part]->Write("", TObject::kOverwrite);
     h_rt[part]->Write("", TObject::kOverwrite);
     h_rA[part]->Write("", TObject::kOverwrite);
}

return 0;

}

