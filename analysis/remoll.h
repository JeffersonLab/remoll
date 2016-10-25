//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Tue Oct 25 13:57:03 2016 by ROOT version 6.06/08
// from TTree T/Geant4 Moller Simulation
// found on file: remollout.root
//////////////////////////////////////////////////////////

#ifndef remoll_h
#define remoll_h

#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>

// Header file for the classes stored in the TTree if any.

class remoll {
public :
   TTree          *fChain;   //!pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //!current Tree number in a TChain

// Fixed size dimensions of array or collections stored in the TTree if any.

   // Declaration of leaf types
   Double_t        rate;
   Double_t        ev_A;
   Double_t        ev_Am;
   Double_t        ev_xs;
   Double_t        ev_Q2;
   Double_t        ev_W2;
   Double_t        ev_thcom;
   Double_t        ev_beamp;
   Int_t           ev_npart;
   Int_t           ev_pid[2];   //[ev.npart]
   Double_t        ev_vx[2];   //[ev.npart]
   Double_t        ev_vy[2];   //[ev.npart]
   Double_t        ev_vz[2];   //[ev.npart]
   Double_t        ev_p[2];   //[ev.npart]
   Double_t        ev_px[2];   //[ev.npart]
   Double_t        ev_py[2];   //[ev.npart]
   Double_t        ev_pz[2];   //[ev.npart]
   Double_t        ev_th[2];   //[ev.npart]
   Double_t        ev_ph[2];   //[ev.npart]
   Double_t        ev_tpx[2];   //[ev.npart]
   Double_t        ev_tpy[2];   //[ev.npart]
   Double_t        ev_tpz[2];   //[ev.npart]
   Double_t        bm_x;
   Double_t        bm_y;
   Double_t        bm_z;
   Double_t        bm_dx;
   Double_t        bm_dy;
   Double_t        bm_dz;
   Double_t        bm_th;
   Double_t        bm_ph;
   Int_t           hit_n;
   Int_t           hit_det[30];   //[hit.n]
   Int_t           hit_vid[30];   //[hit.n]
   Int_t           hit_pid[30];   //[hit.n]
   Int_t           hit_trid[30];   //[hit.n]
   Int_t           hit_mtrid[30];   //[hit.n]
   Int_t           hit_gen[30];   //[hit.n]
   Double_t        hit_x[30];   //[hit.n]
   Double_t        hit_y[30];   //[hit.n]
   Double_t        hit_z[30];   //[hit.n]
   Double_t        hit_r[30];   //[hit.n]
   Double_t        hit_ph[30];   //[hit.n]
   Double_t        hit_px[30];   //[hit.n]
   Double_t        hit_py[30];   //[hit.n]
   Double_t        hit_pz[30];   //[hit.n]
   Double_t        hit_vx[30];   //[hit.n]
   Double_t        hit_vy[30];   //[hit.n]
   Double_t        hit_vz[30];   //[hit.n]
   Double_t        hit_p[30];   //[hit.n]
   Double_t        hit_e[30];   //[hit.n]
   Double_t        hit_m[30];   //[hit.n]
   Int_t           hit_colCut;
   Int_t           sum_n;
   Int_t           sum_det[7];   //[sum.n]
   Int_t           sum_vid[7];   //[sum.n]
   Double_t        sum_edep[7];   //[sum.n]

   // List of branches
   TBranch        *b_rate;   //!
   TBranch        *b_ev_A;   //!
   TBranch        *b_ev_Am;   //!
   TBranch        *b_ev_xs;   //!
   TBranch        *b_ev_Q2;   //!
   TBranch        *b_ev_W2;   //!
   TBranch        *b_ev_thcom;   //!
   TBranch        *b_ev_beamp;   //!
   TBranch        *b_ev_npart;   //!
   TBranch        *b_ev_pid;   //!
   TBranch        *b_ev_vx;   //!
   TBranch        *b_ev_vy;   //!
   TBranch        *b_ev_vz;   //!
   TBranch        *b_ev_p;   //!
   TBranch        *b_ev_px;   //!
   TBranch        *b_ev_py;   //!
   TBranch        *b_ev_pz;   //!
   TBranch        *b_ev_th;   //!
   TBranch        *b_ev_ph;   //!
   TBranch        *b_ev_tpx;   //!
   TBranch        *b_ev_tpy;   //!
   TBranch        *b_ev_tpz;   //!
   TBranch        *b_bm_x;   //!
   TBranch        *b_bm_y;   //!
   TBranch        *b_bm_z;   //!
   TBranch        *b_bm_dx;   //!
   TBranch        *b_bm_dy;   //!
   TBranch        *b_bm_dz;   //!
   TBranch        *b_bm_th;   //!
   TBranch        *b_bm_ph;   //!
   TBranch        *b_hit_n;   //!
   TBranch        *b_hit_det;   //!
   TBranch        *b_hit_vid;   //!
   TBranch        *b_hit_pid;   //!
   TBranch        *b_hit_trid;   //!
   TBranch        *b_hit_mtrid;   //!
   TBranch        *b_hit_gen;   //!
   TBranch        *b_hit_x;   //!
   TBranch        *b_hit_y;   //!
   TBranch        *b_hit_z;   //!
   TBranch        *b_hit_r;   //!
   TBranch        *b_hit_ph;   //!
   TBranch        *b_hit_px;   //!
   TBranch        *b_hit_py;   //!
   TBranch        *b_hit_pz;   //!
   TBranch        *b_hit_vx;   //!
   TBranch        *b_hit_vy;   //!
   TBranch        *b_hit_vz;   //!
   TBranch        *b_hit_p;   //!
   TBranch        *b_hit_e;   //!
   TBranch        *b_hit_m;   //!
   TBranch        *b_hit_colCut;   //!
   TBranch        *b_sum_n;   //!
   TBranch        *b_sum_det;   //!
   TBranch        *b_sum_vid;   //!
   TBranch        *b_sum_edep;   //!

   remoll(TTree *tree=0);
   virtual ~remoll();
   virtual Int_t    Cut(Long64_t entry);
   virtual Int_t    GetEntry(Long64_t entry);
   virtual Long64_t LoadTree(Long64_t entry);
   virtual void     Init(TTree *tree);
   virtual void     Loop();
   virtual Bool_t   Notify();
   virtual void     Show(Long64_t entry = -1);
};

#endif

#ifdef remoll_cxx
remoll::remoll(TTree *tree) : fChain(0) 
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("remollout.root");
      if (!f || !f->IsOpen()) {
         f = new TFile("remollout.root");
      }
      f->GetObject("T",tree);

   }
   Init(tree);
}

remoll::~remoll()
{
   if (!fChain) return;
   delete fChain->GetCurrentFile();
}

Int_t remoll::GetEntry(Long64_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}
Long64_t remoll::LoadTree(Long64_t entry)
{
// Set the environment to read one entry
   if (!fChain) return -5;
   Long64_t centry = fChain->LoadTree(entry);
   if (centry < 0) return centry;
   if (fChain->GetTreeNumber() != fCurrent) {
      fCurrent = fChain->GetTreeNumber();
      Notify();
   }
   return centry;
}

void remoll::Init(TTree *tree)
{
   // The Init() function is called when the selector needs to initialize
   // a new tree or chain. Typically here the branch addresses and branch
   // pointers of the tree will be set.
   // It is normally not necessary to make changes to the generated
   // code, but the routine can be extended by the user if needed.
   // Init() will be called many times when running on PROOF
   // (once per file to be processed).

   // Set branch addresses and branch pointers
   if (!tree) return;
   fChain = tree;
   fCurrent = -1;
   fChain->SetMakeClass(1);

   fChain->SetBranchAddress("rate", &rate, &b_rate);
   fChain->SetBranchAddress("ev.A", &ev_A, &b_ev_A);
   fChain->SetBranchAddress("ev.Am", &ev_Am, &b_ev_Am);
   fChain->SetBranchAddress("ev.xs", &ev_xs, &b_ev_xs);
   fChain->SetBranchAddress("ev.Q2", &ev_Q2, &b_ev_Q2);
   fChain->SetBranchAddress("ev.W2", &ev_W2, &b_ev_W2);
   fChain->SetBranchAddress("ev.thcom", &ev_thcom, &b_ev_thcom);
   fChain->SetBranchAddress("ev.beamp", &ev_beamp, &b_ev_beamp);
   fChain->SetBranchAddress("ev.npart", &ev_npart, &b_ev_npart);
   fChain->SetBranchAddress("ev.pid", ev_pid, &b_ev_pid);
   fChain->SetBranchAddress("ev.vx", ev_vx, &b_ev_vx);
   fChain->SetBranchAddress("ev.vy", ev_vy, &b_ev_vy);
   fChain->SetBranchAddress("ev.vz", ev_vz, &b_ev_vz);
   fChain->SetBranchAddress("ev.p", ev_p, &b_ev_p);
   fChain->SetBranchAddress("ev.px", ev_px, &b_ev_px);
   fChain->SetBranchAddress("ev.py", ev_py, &b_ev_py);
   fChain->SetBranchAddress("ev.pz", ev_pz, &b_ev_pz);
   fChain->SetBranchAddress("ev.th", ev_th, &b_ev_th);
   fChain->SetBranchAddress("ev.ph", ev_ph, &b_ev_ph);
   fChain->SetBranchAddress("ev.tpx", ev_tpx, &b_ev_tpx);
   fChain->SetBranchAddress("ev.tpy", ev_tpy, &b_ev_tpy);
   fChain->SetBranchAddress("ev.tpz", ev_tpz, &b_ev_tpz);
   fChain->SetBranchAddress("bm.x", &bm_x, &b_bm_x);
   fChain->SetBranchAddress("bm.y", &bm_y, &b_bm_y);
   fChain->SetBranchAddress("bm.z", &bm_z, &b_bm_z);
   fChain->SetBranchAddress("bm.dx", &bm_dx, &b_bm_dx);
   fChain->SetBranchAddress("bm.dy", &bm_dy, &b_bm_dy);
   fChain->SetBranchAddress("bm.dz", &bm_dz, &b_bm_dz);
   fChain->SetBranchAddress("bm.th", &bm_th, &b_bm_th);
   fChain->SetBranchAddress("bm.ph", &bm_ph, &b_bm_ph);
   fChain->SetBranchAddress("hit.n", &hit_n, &b_hit_n);
   fChain->SetBranchAddress("hit.det", hit_det, &b_hit_det);
   fChain->SetBranchAddress("hit.vid", hit_vid, &b_hit_vid);
   fChain->SetBranchAddress("hit.pid", hit_pid, &b_hit_pid);
   fChain->SetBranchAddress("hit.trid", hit_trid, &b_hit_trid);
   fChain->SetBranchAddress("hit.mtrid", hit_mtrid, &b_hit_mtrid);
   fChain->SetBranchAddress("hit.gen", hit_gen, &b_hit_gen);
   fChain->SetBranchAddress("hit.x", hit_x, &b_hit_x);
   fChain->SetBranchAddress("hit.y", hit_y, &b_hit_y);
   fChain->SetBranchAddress("hit.z", hit_z, &b_hit_z);
   fChain->SetBranchAddress("hit.r", hit_r, &b_hit_r);
   fChain->SetBranchAddress("hit.ph", hit_ph, &b_hit_ph);
   fChain->SetBranchAddress("hit.px", hit_px, &b_hit_px);
   fChain->SetBranchAddress("hit.py", hit_py, &b_hit_py);
   fChain->SetBranchAddress("hit.pz", hit_pz, &b_hit_pz);
   fChain->SetBranchAddress("hit.vx", hit_vx, &b_hit_vx);
   fChain->SetBranchAddress("hit.vy", hit_vy, &b_hit_vy);
   fChain->SetBranchAddress("hit.vz", hit_vz, &b_hit_vz);
   fChain->SetBranchAddress("hit.p", hit_p, &b_hit_p);
   fChain->SetBranchAddress("hit.e", hit_e, &b_hit_e);
   fChain->SetBranchAddress("hit.m", hit_m, &b_hit_m);
   fChain->SetBranchAddress("hit.colCut", &hit_colCut, &b_hit_colCut);
   fChain->SetBranchAddress("sum.n", &sum_n, &b_sum_n);
   fChain->SetBranchAddress("sum.det", sum_det, &b_sum_det);
   fChain->SetBranchAddress("sum.vid", sum_vid, &b_sum_vid);
   fChain->SetBranchAddress("sum.edep", sum_edep, &b_sum_edep);
   Notify();
}

Bool_t remoll::Notify()
{
   // The Notify() function is called when a new file is opened. This
   // can be either for a new TTree in a TChain or when when a new TTree
   // is started when using PROOF. It is normally not necessary to make changes
   // to the generated code, but the routine can be extended by the
   // user if needed. The return value is currently not used.

   return kTRUE;
}

void remoll::Show(Long64_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}
Int_t remoll::Cut(Long64_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
   return 1;
}
#endif // #ifdef remoll_cxx
