using namespace ROOT;

std::map<TString, std::set<Int_t>> isValid(std::vector<remollGenericDetectorHit_t> *);
Int_t findSector(Float_t, Float_t);

int analyse(TString source, TString out, TString gen, TString detid, TString writeState){

TChain T("T");
T.Add(Form("%s", source.Data())); // Adding source file
Int_t nEvents= T.GetEntries();  // Number of primary events
Double_t weight= 1.0/85; // Divide by current. So, the Y-axis gets units of Hz/uA.

TFile* f = new TFile(Form("%s", out.Data()), writeState);
TDirectory* subdir = f->mkdir(detid);

std::map<TString, TH1D*> h_vz;
std::map<TString, TH2D*> h_xy;
std::map<TString, TH2D*> h_prpz;
std::map<TString, TH1D*> h_r;
std::map<TString, TH1D*> h_ph;

std::vector<TString> sector;
sector.push_back("all");
sector.push_back("closed");
sector.push_back("trans");
sector.push_back("open");

std::vector<TString> photon_ene;
photon_ene.push_back("all");
photon_ene.push_back("ypless1MeV");
photon_ene.push_back("yp1to10MeV");
photon_ene.push_back("ypgreater10MeV");
 
std::vector<TString> photon_mom;
photon_mom.push_back("all");
photon_mom.push_back("forward");
photon_mom.push_back("backward");

TString detector = detid;
TString pid = "photon";


TString part;

for(Int_t j=0; j<sector.size(); j++){
 for(Int_t k=0; k<photon_ene.size();k++){
  for(Int_t l=0; l<photon_mom.size();l++){
   part= Form("pr_%s_%s_%s_%s_%s", detector.Data(), sector[j].Data(), pid.Data(), photon_ene[k].Data(), photon_mom[l].Data());
   h_vz[part]=new TH1D(part+"_vz", Form("%s_vz rate-weighted vertex, Generator=%s", part.Data(), gen.Data()), 360, -6000, 30000);
   h_xy[part]=new TH2D(part+"_xy", Form("%s_xy rate-weighted distribution, Generator=%s", part.Data(), gen.Data() ), 520, -1300, 1300, 520, -1300, 1300);
   h_prpz[part]=new TH2D(part+"_prpz", Form("%s_prpz rate-weighted distribution, Generator=%s", part.Data(), gen.Data() ), 130, -2000, 11000, 220, -11000, 11000);
   h_r[part]=new TH1D(part+"_r", Form("%s_r rate-weighted distribution, Generator=%s", part.Data(), gen.Data()), 400, 0, 2000);
   h_ph[part]=new TH1D(part+"_ph", Form("%s_ph rate-weighted distribution, Generator=%s", part.Data(), gen.Data()), 400,-4,4);
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

for (size_t j=0; j< nEvents; j++){
    T.GetEntry(j);
    std::map<TString,std::set<Int_t>> selectedEvent = isValid(fHit);
    if (selectedEvent["all"].empty()) { continue; }

    for (size_t i=0;i<fHit->size();i++){
        remollGenericDetectorHit_t hit=fHit->at(i);
        Int_t sec=findSector(hit.x, hit.y);
        Double_t rate=0;
     
        std::map<TString, Bool_t> sector_cut{ {"all", 1},  \
                                              {"open", sec==3}, \
                                              {"trans", sec==2}, \
                                              {"closed", sec==1} };

        std::map<TString,Bool_t> photon_ene_cut{ {"all", selectedEvent["all"].find(hit.trid) != selectedEvent["all"].end()}, \
                                                 {"ypless1MeV", selectedEvent["ypless1MeV"].find(hit.trid) != selectedEvent["ypless1MeV"].end()}, \
                                                 {"yp1to10MeV", selectedEvent["yp1to10MeV"].find(hit.trid) != selectedEvent["yp1to10MeV"].end()}, \
                                                 {"ypgreater10MeV", selectedEvent["ypgreater10MeV"].find(hit.trid) != selectedEvent["ypgreater10MeV"].end()} };

        std::map<TString, Bool_t> photon_mom_cut{ {"all", 1},  \
                                                  {"forward", hit.pz>0}, \
                                                  {"backward", hit.pz<=0} };
                                              
        std::map<TString,Bool_t> detector_cut{ {"MD", hit.det==28}, \
                                               {"Col2Ent", hit.det==38}, \
                                               {"Col2Exit", hit.det==39}, \
                                               {"USCoil1", hit.det==40}, \
                                               {"USCoil2", hit.det==41}, \
                                               {"USCoil3", hit.det==43}, \
                                               {"USCoil4", hit.det==144}, \
                                               {"Col4Ent", hit.det==44}, \
                                               {"Col4Exit", hit.det==45}, \
                                               {"PbWallEnt", hit.det==46}, \
                                               {"PbWallExit", hit.det==47}, \
                                               {"PhotonBlockerEnt", hit.det==48}, \
                                               {"PhotonBlockerExit", hit.det==49}, \
                                               {"DSCoil2", hit.det==50}, \
                                               {"DSCoil3", hit.det==51}, \
                                               {"DSCoil6", hit.det==54}, \
                                               {"DSCoil10", hit.det==78} };
     
        std::map<TString,Bool_t> pid_cut;

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
        pid_cut["positron"]  = hit.pid==-11;
        pid_cut["photon"]    = hit.pid==22;
        pid_cut["neutron"]   = hit.pid==2112;
        pid_cut["all"]       = 1;
     
     
        for (Int_t j=0; j<sector.size(); j++){
         for(Int_t k=0; k<photon_ene.size(); k++){
          for(Int_t l=0; l<photon_mom.size(); l++){
           part= Form("pr_%s_%s_%s_%s_%s", detector.Data(), sector[j].Data(), pid.Data(), photon_ene[k].Data(), photon_mom[l].Data());
           Bool_t selectedHit =  detector_cut[detector] && \
                                 sector_cut[sector[j]]  && \
                                 pid_cut[pid] && \
                                 photon_ene_cut[photon_ene[k]] && \
                                 photon_mom_cut[photon_mom[l]];
           if (selectedHit){
            h_vz[part]->Fill(hit.vz, rate*weight);
            h_xy[part]->Fill(hit.x, hit.y, rate*weight);
            h_prpz[part]->Fill(sqrt(hit.px*hit.px+hit.py*hit.py), hit.pz, rate*weight);
            h_r[part]->Fill(hit.r, rate*weight);
            h_ph[part]->Fill(hit.ph, rate*weight);
           }
          }
         }
        }
          
    }
}


for (Int_t j=0; j<sector.size(); j++){
 for(Int_t k=0; k<photon_ene.size(); k++){
  for(Int_t l=0; l<photon_mom.size(); l++){
   part= Form("pr_%s_%s_%s_%s_%s", detector.Data(), sector[j].Data(), pid.Data(), photon_ene[k].Data(), photon_mom[l].Data());
   h_vz[part]->SetDirectory(subdir);
   h_xy[part]->SetDirectory(subdir);
   h_prpz[part]->SetDirectory(subdir);
   h_r[part]->SetDirectory(subdir);
   h_ph[part]->SetDirectory(subdir);
  }
 }
}
f->Write();

return 0;
}

std::map<TString, std::set<Int_t>> isValid(std::vector<remollGenericDetectorHit_t> *fHit){

 std::map<TString, std::set<Int_t>> found;

 // return true if there is atleast one hit from a photon in ring 5 from an event
 for (size_t i=0;i<fHit->size();i++){
  remollGenericDetectorHit_t hit=fHit->at(i);
  if (!(hit.det==28 && hit.pid==22 && hit.r>900 && hit.r<=1060)) continue;

  found["all"].insert(hit.trid);
  if(hit.p<=1){
      found["ypless1MeV"].insert(hit.trid);
  }else if (hit.p>1 && hit.p<=10){
      found["yp1to10MeV"].insert(hit.trid);
  }else{
      found["ypgreater10MeV"].insert(hit.trid);
  }
 }

return found;

}

Int_t findSector(Float_t x, Float_t y){
  Double_t sepdiv=2*TMath::Pi()/7.0;
  Int_t sec=0;
  Double_t phi=atan2(y,x);
  if (phi<0) {phi+=2*TMath::Pi();}

  Double_t secphi = fmod(phi, 2*TMath::Pi()/7);
  if (secphi<TMath::Pi()/28.0){sec=1;}           // closed
  else if (secphi<3*TMath::Pi()/28.0){sec=2; }    // transition
  else if (secphi<5*TMath::Pi()/28.0) {sec=3;}   // open
  else if (secphi<7*TMath::Pi()/28.0) {sec=2;}   // transition
  else {sec=1;}  //closed
  
  return sec;
}
