/*
This program for calculating the power on the collimator 1,2,4,5 for MOLLER 
experiment
Chandan Ghosh
part of this is taken from Rakitha's code coll_scan.cc
*/
#include <vector>
#include <string>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <new>
#include <cstdlib>
#include <math.h>

#include <TRandom.h>
#include <TRandom3.h>
#include <TApplication.h>
#include <TSystem.h>

#include <TH2F.h>
#include <TTree.h>
#include <TF1.h>
#include <TProfile.h>
#include <Rtypes.h>
#include <TROOT.h>
#include <TFile.h>
#include <TChain.h>
#include <TString.h>
#include <TDatime.h>
#include <TStopwatch.h>
#include <stdexcept>
#include <time.h>
#include <cstdio>
#include <map>
#include <cassert>

#include <TMath.h>
#include <TStyle.h>
#include <TPaveStats.h>

#include <TCanvas.h>
#include <TGraph.h>
#include <TMultiGraph.h>
#include <TLegend.h>
#include <TGraphErrors.h>
#include <TFrame.h>
#include <TObjArray.h>
#include <TVector2.h>

using namespace std;

void set_plot_style();

//int main(Int_t argc,Char_t* argv[]) {
//int main() {
void issue179()
{
      	//TApplication theApp("App",&argc,argv);

	  gROOT->SetStyle("Plain");
	  //gStyle->SetOptStat(0); 
	  gSystem->Load("libremoll.so");
	  gStyle->SetOptStat("eMR");
	  set_plot_style();
	
	  const Int_t nofile =1;
	  Int_t collimator[5]={2001,2006,2002,2004,2005};
	  Int_t volume;
	  Double_t dumyenergy=0.;
	  Double_t dumyenergy1=0.;
	  Int_t particle;
	  Double_t esum[5]={0.};
	  Double_t esum_pid[5]={0.};
	  Double_t esum1[5][4]={0.};
	  //collimator edep
	  TChain * Tmol =new TChain("T");
	  TString added_file_array[nofile]={""};
	  for(int v=1;v<=nofile;v++)
	  {
		ostringstream temp_string1;
                ostringstream temp_string2;
                temp_string1<<v;
                TString vS;
                vS = temp_string1.str();
                temp_string2<<Form("remollout%d",v)<<".root";
                added_file_array[v-1]=temp_string2.str();
                cout<<temp_string2.str()<<endl;
                //cout<<"file "<<v<<endl;
                Tmol->Add(added_file_array[v-1]);
                //cout<<"file next "<<v<<endl;
         }
	  //Tmol->Add("/home/lead/G4WORK/data/CollPower/remollout.root");
	  std::vector< remollGenericDetectorSum_t > *fGenDetSum =0;
	  Tmol->SetBranchAddress("sum",&fGenDetSum);
	  
	  std::vector< remollGenericDetectorSumByPID_t > fGenSumByPID;
	  Int_t nentries = (Int_t)Tmol->GetEntries();
	  std::cout<<"Number of entries in TChain "<<nentries<<std::endl;
	  std::cout<<"Partice 0: electron; 1: positron; 2: gamma; 3: neutron"<<std::endl;
	  //for(int ij=0;ij<100;ij++)//Event loop
	  for(int ij=0;ij<nentries;ij++)//Event loop
	  {
		  Tmol->GetEntry(ij);
		  for(size_t pk=0;pk<fGenDetSum->size();pk++)
		  {
			  volume = fGenDetSum->at(pk).det;
			  dumyenergy = fGenDetSum->at(pk).edep;
			  fGenSumByPID = fGenDetSum->at(pk).by_pid;
			  //cout<<"event  "<<ij<<"  instance "<<pk<<" det "<<volume<<"  edep "<<dumyenergy<<endl;
			  /*
			  fGenSumByPID = fGenDetSum->at(pk).by_pid;
			  for(size_t ik=0;ik<fGenSumByPID.size();ik++)
			  {
				  cout<<"Event "<<ij<<"  pk "<<pk<<" ik "<<ik<<" x "<< fGenSumByPID.at(ik).x<<" y "<<fGenSumByPID.at(ik).y<<" z "<< fGenSumByPID.at(ik).z<<endl;
			  }*/
			  if(volume==collimator[0])//collimator 1
			  {
				  esum[0] +=dumyenergy;
				  for(size_t ik=0;ik<fGenSumByPID.size();ik++)
				  {
					  particle = fGenSumByPID.at(ik).pid;
					  dumyenergy1=fGenSumByPID.at(ik).edep;
					  //cout<<"Event "<<ij<<" pk "<<pk<<" ik "<<ik<<" det "<<volume<<" pid "<<particle<<" edep "<<dumyenergy1<<endl;
					  if(particle==11)
					  {
						  esum1[0][0] +=dumyenergy1;
						  //cout<<"Particle "<<particle<<" edep "<<esum1[0][0]<<endl;
					  }
					  if(particle==-11)
					  {
						  esum1[0][1] +=dumyenergy1;
						  //cout<<"Particle "<<particle<<" edep "<<esum1[0][1]<<endl;
					  }
					  if(particle==22)
					  {
						  esum1[0][2] +=dumyenergy1;
						  //cout<<"Particle "<<particle<<" edep "<<esum1[0][2]<<endl;
					  }
					  if(particle==2112)
					  {
						  esum1[0][3] +=dumyenergy1;
						  //cout<<"Particle "<<particle<<" edep "<<esum1[0][3]<<endl;
					  }
				  }
				  //std::cout<<"Collimator 1  Energy (MeV) "<<esum[0]<<" dumyenergy  "<<dumyenergy<<std::endl;
			  }
			  if(volume==collimator[1])//collimator 1 fins
			  {
				  esum[1] +=dumyenergy;
				  for(size_t ik=0;ik<fGenSumByPID.size();ik++)
				  {
					  particle = fGenSumByPID.at(ik).pid;
					  dumyenergy1=fGenSumByPID.at(ik).edep;
					  //cout<<"Event "<<ij<<" pk "<<pk<<" ik "<<ik<<" det "<<volume<<" pid "<<particle<<" edep "<<dumyenergy1<<endl;
					  if(particle==11)
					  {
						  esum1[1][0] +=dumyenergy1;
						 // cout<<"Particle "<<particle<<" edep "<<esum1[1][0]<<endl;
					  }
					  if(particle==-11)
					  {
						  esum1[1][1] +=dumyenergy1;
						  //cout<<"Particle "<<particle<<" edep "<<esum1[1][1]<<endl;
					  }
					  if(particle==22)
					  {
						  esum1[1][2] +=dumyenergy1;
						 // cout<<"Particle "<<particle<<" edep "<<esum1[1][2]<<endl;
					  }
					  if(particle==2112)
					  {
						  esum1[1][3] +=dumyenergy1;
						  //cout<<"Particle "<<particle<<" edep "<<esum1[1][3]<<endl;
					  }
				  }
				  //std::cout<<"Collimator 1 fins  Energy (MeV) "<<esum[1]<<" dumyenergy  "<<dumyenergy<<std::endl;
			  }
			  if(volume==collimator[2])//collimator 2
			  {
				  esum[2] +=dumyenergy;
				  for(size_t ik=0;ik<fGenSumByPID.size();ik++)
				  {
					  particle = fGenSumByPID.at(ik).pid;
					  dumyenergy1=fGenSumByPID.at(ik).edep;
					  //cout<<"Event "<<ij<<" pk "<<pk<<" ik "<<ik<<" det "<<volume<<" pid "<<particle<<" edep "<<dumyenergy1<<endl;
					  if(particle==11)
					  {
						  esum1[2][0] +=dumyenergy1;
						  //cout<<"Particle "<<particle<<" edep "<<esum1[2][0]<<endl;
					  }
					  if(particle==-11)
					  {
						  esum1[2][1] +=dumyenergy1;
						  //cout<<"Particle "<<particle<<" edep "<<esum1[2][1]<<endl;
					  }
					  if(particle==22)
					  {
						  esum1[2][2] +=dumyenergy1;
						  //cout<<"Particle "<<particle<<" edep "<<esum1[2][2]<<endl;
					  }
					  if(particle==2112)
					  {
						  esum1[2][3] +=dumyenergy1;
						  //cout<<"Particle "<<particle<<" edep "<<esum1[2][3]<<endl;
					  }
				  }
				  //std::cout<<"Collimator 2  Energy (MeV) "<<esum[2]<<" dumyenergy  "<<dumyenergy<<std::endl;
			  }
			  if(volume==collimator[3])//collimator 4
			  {
				  esum[3] +=dumyenergy;
				  for(size_t ik=0;ik<fGenSumByPID.size();ik++)
				  {
					  particle = fGenSumByPID.at(ik).pid;
					  dumyenergy1=fGenSumByPID.at(ik).edep;
					  //cout<<"Event "<<ij<<" pk "<<pk<<" ik "<<ik<<" det "<<volume<<" pid "<<particle<<" edep "<<dumyenergy1<<endl;
					  if(particle==11)
					  {
						  esum1[3][0] +=dumyenergy1;
						  //cout<<"Particle "<<particle<<" edep "<<esum1[3][0]<<endl;
					  }
					  if(particle==-11)
					  {
						  esum1[3][1] +=dumyenergy1;
						  //cout<<"Particle "<<particle<<" edep "<<esum1[3][1]<<endl;
					  }
					  if(particle==22)
					  {
						  esum1[3][2] +=dumyenergy1;
						  //cout<<"Particle "<<particle<<" edep "<<esum1[3][2]<<endl;
					  }
					  if(particle==2112)
					  {
						  esum1[3][3] +=dumyenergy1;
						  //cout<<"Particle "<<particle<<" edep "<<esum1[3][3]<<endl;
					  }
				  }
				  //std::cout<<"Collimator 4  Energy (MeV) "<<esum[3]<<" dumyenergy  "<<dumyenergy<<std::endl;
			  }
			  if(volume==collimator[4])//collimator 5
			  {
				  esum[4] +=dumyenergy;
				  for(size_t ik=0;ik<fGenSumByPID.size();ik++)
				  {
					  particle = fGenSumByPID.at(ik).pid;
					  dumyenergy1=fGenSumByPID.at(ik).edep;
					  //cout<<"Event "<<ij<<" pk "<<pk<<" ik "<<ik<<" det "<<volume<<" pid "<<particle<<" edep "<<dumyenergy1<<endl;
					  if(particle==11)
					  {
						  esum1[4][0] +=dumyenergy1;
						  //cout<<"Particle "<<particle<<" edep "<<esum1[4][0]<<endl;
					  }
					  if(particle==-11)
					  {
						  esum1[4][1] +=dumyenergy1;
						  //cout<<"Particle "<<particle<<" edep "<<esum1[4][1]<<endl;
					  }
					  if(particle==22)
					  {
						  esum1[4][2] +=dumyenergy1;
						  //cout<<"Particle "<<particle<<" edep "<<esum1[4][2]<<endl;
					  }
					  if(particle==2112)
					  {
						  esum1[4][3] +=dumyenergy1;
						  //cout<<"Particle "<<particle<<" edep "<<esum1[4][3]<<endl;
					  }
				  }
				  //std::cout<<"Collimator 5  Energy (MeV) "<<esum[4]<<" dumyenergy  "<<dumyenergy<<std::endl;
			  }

		  }
	  }
	  for(int pq=0;pq<5;pq++)
	  {
		  std::cout<<"Collimator  "<<collimator[pq]<<"  Deposited energy (from sum.edep) "<<esum[pq]*85./nentries<<" W/85uA"<<std::endl;
		  for(int ik=0;ik<4;ik++)
		  {
			  cout<<"Collimator  "<<collimator[pq]<<" particle "<<ik<<" energy deposited (from by_pid) "<<esum1[pq][ik]*85./nentries<<"  W/85uA"<<endl;
			  esum_pid[pq]+=esum1[pq][ik];
		  }
		  cout<<"Collimator "<<collimator[pq]<<" By pid sum edep (from by_pid) "<<esum_pid[pq]*85./nentries<<"  W/85uA"<<endl;

	  }
	  std::cout<<"Total energy deposited inside collimator (from sum.edep) "<<esum[0]*85./nentries+esum[1]*85./nentries<<"  W/85uA"<<std::endl;//sum of collimator 1 and fins
}
void set_plot_style()
{
    const Int_t NRGBs = 5;
    const Int_t NCont = 255;
    // See class TColor documentation and SetPalette() command
    Double_t stops[NRGBs] = { 0.00, 0.34, 0.61, 0.84, 1.00 };
    Double_t red[NRGBs]   = { 0.00, 0.00, 0.87, 1.00, 0.51 };
    Double_t green[NRGBs] = { 0.00, 0.81, 1.00, 0.20, 0.00 };
    Double_t blue[NRGBs]  = { 0.51, 1.00, 0.12, 0.00, 0.00 };
    TColor::CreateGradientColorTable(NRGBs, stops, red, green, blue, NCont);
    gStyle->SetNumberContours(NCont);
}

