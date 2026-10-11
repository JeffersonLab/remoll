//  Simple macro to look at incidentflux at a GEM layer
//
//  Need to use reroot (CERN's root compiled with special remoll libraries), 
//  or generic root when you have linked to libremoll.so, i.e. by a command like
//     setenv LD_PRELOAD build/libremoll.so
//   
// Run using (for example):
//    build/reroot 
//    Root>.L GEM_flux.C
//    Root> GEM_flux(50000,"input.root")
//      to analyze 
//
//  or to do entirely in the command line:
//     build/reroot -l -q 'GEM_flux.C(50000,"input.root")'
//  to do the same thing
//
//    GEM_flux(-1,"input.root") will process the entire input file
///
#include <TF1.h>
#include <TF2.h>

void GEM_flux(const int number, const TString& file)
{

 TChain* T = new TChain("T");
 T->Add(file);
// T->Add("001.root");
// T->Add("002.root");
// T->Add("003.root");
// T->Add("004.root");
// T->Add("005.root");
// T->Add("006.root");
// T->Add("007.root");
// T->Add("008.root");
// T->Add("009.root");
// T->Add("010.root");
// T->Add("011.root");
// T->Add("012.root");
// T->Add("013.root");
// T->Add("014.root");
// T->Add("015.root");
// T->Add("016.root");
// T->Add("017.root");
// T->Add("018.root");
// T->Add("019.root");
// T->Add("020.root");
// T->Add("021.root");
// T->Add("022.root");
// T->Add("023.root");
// T->Add("024.root");
// T->Add("025.root");
// T->Add("026.root");
// T->Add("027.root");
// T->Add("028.root");
// T->Add("029.root");

 Double_t rate = 0;

 int nskim = 0;
 int nanalyzed = 0;
 int maxevent = 0;
 remollEvent_t* events = 0;
 std::vector<remollEventParticle_t>* parts = 0;
 std::vector<remollGenericDetectorHit_t>* hits = 0;

 // Define some branches of the Tree (which is "T")
 T->SetBranchAddress("rate", &rate);
 T->SetBranchAddress("hit", &hits);
 T->SetBranchAddress("part", &parts);
 T->SetBranchAddress("ev", &events);

 TH1F* p_g = new TH1F("p_g","p at GEM, gammas",100,0,500.0);
 p_g->GetXaxis()->SetTitle("Momentum  (MeV)");
 p_g->GetYaxis()->SetTitle("hits");

 TH1F* p_g_back = new TH1F("p_g_back","p at GEM, backward gammas",100,0,500.0);
 p_g_back->GetXaxis()->SetTitle("Momentum  (MeV)");
 p_g_back->GetYaxis()->SetTitle("hits");

 TH1F* p_e = new TH1F("p_e","p at GEM, electrons",120,0,12000.0);
 p_e->GetXaxis()->SetTitle("Momentum  (MeV)");
 p_e->GetYaxis()->SetTitle("hits");

 TH1F* r_e = new TH1F("r_e","r at GEM, electrons",150,0,1500.0);
 r_e->GetXaxis()->SetTitle("Radius  (mm)");
 r_e->GetYaxis()->SetTitle("hits");

 TH1F* r_p = new TH1F("r_p","r at GEM, positrons",150,0,1500.0);
 r_p->GetXaxis()->SetTitle("Radius  (mm)");
 r_p->GetYaxis()->SetTitle("hits");

 TH1F* p_p = new TH1F("p_p","p at GEM, postrons",200,0,200.0);
 p_p->GetXaxis()->SetTitle("Momentum  (MeV)");
 p_p->GetYaxis()->SetTitle("hits");

 TH1F* r_e_h = new TH1F("r_e_h","r at GEM, electrons, p>1 MeV",150,0,1500.0);
 r_e_h->GetXaxis()->SetTitle("Radius  (mm)");
 r_e_h->GetYaxis()->SetTitle("hits");

 TH1F* r_e_m = new TH1F("r_e_m","r at GEM, electrons, p>1000 MeV",150,0,1500.0);
 r_e_m->GetXaxis()->SetTitle("Radius  (mm)");
 r_e_m->GetYaxis()->SetTitle("hits");

 TH1F* z_e_m = new TH1F("z_e_m","z at GEM, electrons", 100, 0, 30000);

 TH2F* xy_e_m = new TH2F("x_y_e_m","x-y at GEM, electrons", 100,  -1500, 1500, 100, -1500, 1500);

 TH2F* r_p_m = new TH2F("r_p_m","r vs p  GEM, electrons, p>1000 MeV",60, 0, 12000, 50,0,1500.0);
 r_p_m->GetXaxis()->SetTitle("Momentum  (MeV)");
 r_p_m->GetYaxis()->SetTitle("Radius (mm)");



 TH1F* r_g = new TH1F("r_g","r at GEM, gammas",150,0,1500.0);
 r_g->GetXaxis()->SetTitle("Radius  (mm)");
 r_g->GetYaxis()->SetTitle("hits");

 TH1F* r_g_h = new TH1F("r_g_h","r at GEM, gammas, p>1 MeV", 150, 0, 1500.0);
 r_g_h->GetXaxis()->SetTitle("Radius  (mm)");
 r_g_h->GetYaxis()->SetTitle("hits");

 TH1F* p_g_l = new TH1F("p_g_l","p at GEM, gammas, p<1 MeV", 100, 0, 1.0);
 p_g_l->GetXaxis()->SetTitle("Momentum  (MeV)");
 p_g_l->GetYaxis()->SetTitle("hits");

 TH2F* xy_g_m = new TH2F("x_y_g_m","x-y at GEM, gammas", 100,  -1500, 1500, 100, -1500, 1500);


 TH1F* ph = new TH1F("ph","phi particle",100,-3.5,3.5);
 ph->GetXaxis()->SetTitle("Phi  (radians)");
 ph->GetYaxis()->SetTitle("hits");

 TH1F* ph_c = new TH1F("ph_c","phi hit coll1",100,-3.5,3.5);
 ph_c->GetXaxis()->SetTitle("Phi  (radians)");
 ph_c->GetYaxis()->SetTitle("hits");

 TH1F* ph_c_e = new TH1F("ph_c_e","phi electron at coll1",100,-3.5,3.5);
 ph_c_e->GetXaxis()->SetTitle("Phi  (radians)");
 ph_c_e->GetYaxis()->SetTitle("hits");

 TH1F* ph_c_h = new TH1F("ph_c_h","phi energetic electron at coll1",100,-3.5,3.5);
 ph_c_h->GetXaxis()->SetTitle("Phi  (radians)");
 ph_c_h->GetYaxis()->SetTitle("hits");


 ///*--------------------------------------------------*/ 
 // Argon Volume

 TH1F* r_g_Argon = new TH1F("r_g_Argon","r at GEM Argon, gammas", 150, 0, 1500);
// TH1F* z_g_Argon = new TH1F("z_g_Argon","z at GEM Argon, gammas", 100, 0, 30000);
 TH1F* z_g_Argon = new TH1F("z_g_Argon","z at GEM Argon, gammas", 100, 20000, 22000);
 TH1F* p_g_Argon = new TH1F("p_g_Argon","z at GEM Argon, gammas", 100, 0, 11000);
 TH2F* xy_g_Argon = new TH2F("x_y_g_Argon","x-y at GEM Argon, gammas", 100,  -1500, 1500, 100,  -1500, 1500);

 TH1F* r_e_Argon = new TH1F("r_e_Argon","r at GEM Argon, electrons", 150, 0, 1500);
 TH1F* z_e_Argon = new TH1F("z_e_Argon","z at GEM Argon, electrons", 100, 0, 30000);
 TH1F* p_e_Argon = new TH1F("p_e_Argon","z at GEM Argon, electrons", 100, 0, 11000);

 TH2F* xy_e_Argon = new TH2F("x_y_e_Argon","x-y at GEM Argon, electrons", 100,  -1500, 1500, 100,  -1500, 1500);

 // r_g->GetXaxis()->SetTitle("Radius  (mm)");
 // r_g->GetYaxis()->SetTitle("hits");


 // TFile newfile("small.root", "recreate");
 // auto newtree = T->CloneTree(0);

 maxevent = number;
 if (number==-1) maxevent=T->GetEntries();

 // Loop over all events//
   for (size_t iev = 0; iev < maxevent; iev++) {

   T->GetEntry(iev);
   remollEventParticle_t part =  parts->at(0);
   int nhits = 0;

   ph->Fill(part.ph);

 // Loop over all hits//
   for (size_t ihit = 0; ihit < hits->size(); ihit++) {
     remollGenericDetectorHit_t hit = hits->at(ihit);
 
     if (hit.det==38) {  // front face, coll 2	
       ph_c->Fill(hit.ph);
       if (hit.pid==11){
	 ph_c_e->Fill(hit.ph);
	 if (hit.p>1000) ph_c_h->Fill(hit.ph);
       }
     }


//     if (hit.det==34) {  // Detector 34 is GEM layer 3
     if (hit.det==32) {  // Detector 34 is GEM layer 3
       if (hit.r>600&&hit.r<1100){
       nhits++;
       if (hit.pid==11){
	 p_e->Fill(hit.p);
	 r_e->Fill(hit.r);
	 if (hit.p>1) r_e_h->Fill(hit.r);
	 if (hit.p>1000) {
	   r_e_m->Fill(hit.r);
           r_p_m->Fill(hit.p,hit.r);
	 }

     z_e_m->Fill(hit.z);
     xy_e_m->Fill(hit.x, hit.y);

       }
       if (hit.pid==22){
	 p_g->Fill(hit.p);
	 if (hit.pz <0.0) p_g_back->Fill(hit.p);
   	 r_g->Fill(hit.r);
	 if (hit.p>1) r_g_h->Fill(hit.r);
	 if (hit.p<1) p_g_l->Fill(hit.p);

     xy_g_m->Fill(hit.x, hit.y);

       }
       if (hit.pid==-11){
	 p_p->Fill(hit.p);
	 r_p->Fill(hit.r);
       }
       } // end if 600 < hit.r <1100 mm
     } // end if hit.det==34

     if (hit.det==577) {  
       if (hit.pid==11){
          r_e_Argon->Fill(hit.r);
          z_e_Argon->Fill(hit.z);
          p_e_Argon->Fill(hit.p);
          xy_e_Argon->Fill(hit.x, hit.y);

	   }
       if (hit.pid==22){
          r_g_Argon->Fill(hit.r);
          z_g_Argon->Fill(hit.z);
          p_g_Argon->Fill(hit.p);
          xy_g_Argon->Fill(hit.x, hit.y);
	   }
     }




   } // end loop all hits    

   nanalyzed++;
   if(iev%10000==0) cout << "events processed so far: " << iev << endl;
 }    //end loop over events

   cout << "Total events analyzed =  " << nanalyzed << endl;
  

   TCanvas *c1 = new TCanvas("c1","electrons",100,100,1200,600);
   c1->Divide(3,2);
   c1->cd(1);
   gPad->SetLogy();
   p_e->Draw();
   c1->cd(2);
   r_e->Draw();
   c1->cd(3);
   r_e_h->Draw();
   c1->cd(4);
   r_e_m->Draw();
   c1->cd(5);
   z_e_m->Draw();
   c1->cd(6);
   xy_e_m->Draw("colz");

   c1->Print("images/GEM_electrons.pdf");

   TCanvas *c2 = new TCanvas("c2","gammas",100,100,1200,600);
   c2->Divide(3,2);
   c2->cd(1);
   gPad->SetLogy();
   p_g->Draw();
   c2->cd(2);
   r_g->Draw();
   c2->cd(3);
   r_g_h->Draw();
   c2->cd(4);
   p_g_l->Draw();
   c2->cd(6);
   xy_g_m->Draw("colz");
   c2->Print("images/GEM_gammas.pdf");
   



   c2->Print("images/GEM_gammas.pdf");
   



   TCanvas *c3 = new TCanvas("c3","others",100,100,800,600);
   c3->Divide(2,2);
   c3->cd(1);
   r_p->Draw();
   c3->cd(2);
   gPad->SetLogy();
   p_p->Draw();
   c3->cd(3);
   r_p_m->Draw("colz");
   c3->cd(4);
   gPad->SetLogy();
   p_g_back->Draw();
   c3->Print("images/GEM_others.pdf");

   TCanvas *c4 = new TCanvas("c4","Phi",100,100,800,600);
   c4->Divide(2,2);
   c4->cd(1);
   ph->Draw();
   c4->cd(2);
   ph_c->Draw();
   c4->cd(3);
   ph_c_e->Draw();
   c4->cd(4);
   ph_c_h->Draw();
   c4->Print("images/GEM_phi.pdf");


   TCanvas *c5 = new TCanvas("c5","GEM_gamma_Argon",100,100,800,600);
   c5->Divide(2,2);
   c5->cd(1);
   r_g_Argon->Draw();
   c5->cd(2);
   z_g_Argon->Draw();
   c5->cd(3);
   p_g_Argon->Draw();
   c5->cd(4);
   xy_g_Argon->Draw("colz");
   c5->Print("images/GEM_gamma_Argon.pdf");

   TCanvas *c6 = new TCanvas("c6","GEM_e_Argon",100,100,800,600);
   c6->Divide(2,2);
   c6->cd(1);
   r_e_Argon->Draw();
   c6->cd(2);
   z_e_Argon->Draw();
   c6->cd(3);
   p_e_Argon->Draw();
   c6->cd(4);
   xy_e_Argon->Draw("colz");
   c6->Print("images/GEM_e_Argon.pdf");




}
