// Run commands in this order:
//
// //Start reroot
// >/path/to/build/reroot
//
// //Load in the script, and run it
// > .L radDamage.cc+
// > gSystem->Load("radDamage_cc.so");
//
// //Load in the script, and run it
// >.L radAna.C
// > radAna(<remoll output file>,<1 for beam generator, 0 else>)

#include "radDamage.hh"
#include "histogramUtilities.h"
#include "mainDetUtilities.h"

TFile *fout;
const int nSp=5;// [e/pi,e/pi E>1,gamma,neutron]
const string spTit[nSp]={"e/#pi","e/#pi E>1","#gamma","neutron","primary e E>1"};
const string spH[nSp]={"e","e1","g","n","eP1"};
map<int,int> spM {{11,1},{211,1},{22,3},{2112,4}};

const int nDet=3;
const string detH[nDet]={"det28","det26","det27"};
map<int,int> dtM {{26,2},{27,3},{28,1}};

TH1D *energy[nSp][nDet], *energyNIEL[nSp][nDet];
TH1D *z0[nSp][nDet], *z0E[nSp][nDet],*z0NIEL[nSp][nDet];
TH1D *z0R5[nSp][nDet], *z0R5E[nSp][nDet],*z0R5NIEL[nSp][nDet];
TH1D *z0R7[nSp][nDet], *z0R7E[nSp][nDet],*z0R7NIEL[nSp][nDet];
TH1D *z0HE[nSp][nDet], *z0HEE[nSp][nDet],*z0HENIEL[nSp][nDet];

TH2D *xy[nSp][nDet], *xyE[nSp][nDet],*xyNIEL[nSp][nDet];
TH2D *z0r0[nSp][nDet],*z0r0E[nSp][nDet],*z0r0NIEL[nSp][nDet];
TH2D *z0x0[nSp][nDet],*z0x0E[nSp][nDet],*z0x0NIEL[nSp][nDet];

//for det28 only
const int nSecDet = 21; // 7(ring, including pmts) x 3 (sectors)
const int nErange = 4; //all, E<=1MeV; 1<E<=10MeV; 10MeV<E;
const string eRgTit[nErange]={"all E","E<=1","1<E<=10","10<E"};
TH1D *mdHits[nSp][nErange]; //number of hits for each species; bins are different sectors and rings in det 28;
TH1D *mdHitsE[nSp][nErange],*mdHitsNIEL[nSp][nErange];

radDamage radDmg;

string fileNm;
int beamGen(1);
long nTotEv(0);
int nFiles(0);
long currentEvNr(0);

const double pi = acos(-1);

void initHisto();
void writeOutput();
long processOne(string);
void process();

void radAna(const string& finName = "./remollout.root", int beamGenerator=1){
  fileNm = finName;
  beamGen = beamGenerator;

  initHisto();
  process();
  writeOutput();
}

void process(){

  if(fileNm==""){
    cout<<"\t did not find input file. Quitting!"<<endl;
    return 2;
  }

  if( fileNm.find(".root") < fileNm.size() ){
    cout<<"Processing single file:\n\t"<<fileNm<<endl;
    nTotEv+=processOne(fileNm);
    nFiles=1;
  }else{
    cout<<"Attempting to process list of output from\n\t"<<fileNm<<endl;
    ifstream ifile(fileNm.c_str());
    string data;
    while(ifile>>data){
      cout<<" processing: "<<data<<endl;
      nTotEv+=processOne(data);
      nFiles++;
    }
  }

  cout<<"\nFinished processing a total of "<<nTotEv<<endl;
}

long processOne(string fnm){
  TFile *fin=TFile::Open(fnm.c_str(),"READ");
  if(!fin->IsOpen() || fin->IsZombie()){
    cout<<"Problem: can't find file: "<<fnm<<endl;
    fin->Close();
    delete fin;
    return 0;
  }
  TTree *t=(TTree*)fin->Get("T");
  if (t == 0) return 0;
  Double_t rate=0;
  remollEvent_t *ev=0;
  remollBeamTarget_t *bm=0;
  std::vector<remollEventParticle_t> *part=0;
  std::vector<remollGenericDetectorHit_t> *hit=0;
  //std::vector<remollGenericDetectorSum_t> *sum;
  t->SetBranchAddress("rate", &rate);
  t->SetBranchAddress("bm", &bm);
  t->SetBranchAddress("ev", &ev);
  t->SetBranchAddress("part", &part);
  t->SetBranchAddress("hit", &hit);
  //t->SetBranchAddress("sum", &sum);

  long nEntries = t->GetEntries();
  cout<<"\tTotal events: "<<nEntries<<endl;
  float currentProc=1,procStep=30;
  vector<int> procID;
  int sector(-1);

  //for (Long64_t event = 0; event < 5; t->GetEntry(event++)) {
  for (Long64_t event = 0; event < nEntries; t->GetEntry(event++)) {
    currentEvNr++;
    if( float(event+1)/nEntries*100 > currentProc){
      cout<<"at tree entry\t"<<event<<"\t"<< float(event+1)/nEntries*100<<endl;
      currentProc+=procStep;
    }

    for(int j=0;j<hit->size();j++){

      if(std::isnan(rate) || std::isinf(rate)) continue;
      if(rate==0) {rate=1;}

      int sp = spM[int(abs(hit->at(j).pid))]-1;
      if(sp==-1) continue;

      int dt = dtM[int(hit->at(j).det)]-1;
      if(dt==-1) continue;

      double kinE = hit->at(j).p;
      double niel = radDmg.GetNIEL(hit->at(j).pid,kinE,0);
      if(niel<0) niel=0;
      double vz0 = hit->at(j).vz;
      double vx0 = hit->at(j).vx;
      double vr0=sqrt(pow(hit->at(j).vx,2)+pow(hit->at(j).vy,2));

      energy[sp][dt]->Fill(kinE,rate);
      energyNIEL[sp][dt]->Fill(kinE,rate*niel);

      z0[sp][dt]->Fill(vz0,rate);
      z0E[sp][dt]->Fill(vz0,rate*kinE);
      z0NIEL[sp][dt]->Fill(vz0,rate*niel);

      double rr=hit->at(j).r;
      if(kinE>10 && rr>500 && rr<1500){
	z0HE[sp][dt]->Fill(vz0,rate);
	z0HEE[sp][dt]->Fill(vz0,rate*kinE);
	z0HENIEL[sp][dt]->Fill(vz0,rate*niel);
      }
      
      z0r0[sp][dt]->Fill(vz0,vr0,rate);
      z0r0E[sp][dt]->Fill(vz0,vr0,rate*kinE);
      z0r0NIEL[sp][dt]->Fill(vz0,vr0,rate*niel);

      z0x0[sp][dt]->Fill(vz0,vx0,rate);
      z0x0E[sp][dt]->Fill(vz0,vx0,rate*kinE);
      z0x0NIEL[sp][dt]->Fill(vz0,vx0,rate*niel);

      double xx = hit->at(j).x;
      double yy = hit->at(j).y;
      xy[sp][dt]->Fill(xx,yy,rate);
      xyE[sp][dt]->Fill(xx,yy,rate*kinE);
      xyNIEL[sp][dt]->Fill(xx,yy,rate*niel);

      if(sp==0 && kinE>1){
	energy[1][dt]->Fill(kinE,rate);
	energyNIEL[1][dt]->Fill(kinE,rate*niel);
	
	z0[1][dt]->Fill(vz0,rate);
	z0E[1][dt]->Fill(vz0,rate*kinE);
	z0NIEL[1][dt]->Fill(vz0,rate*niel);
	
	z0r0[1][dt]->Fill(vz0,vr0,rate);
	z0r0E[1][dt]->Fill(vz0,vr0,rate*kinE);
	z0r0NIEL[1][dt]->Fill(vz0,vr0,rate*niel);
	
	z0x0[1][dt]->Fill(vz0,vx0,rate);
	z0x0E[1][dt]->Fill(vz0,vx0,rate*kinE);
	z0x0NIEL[1][dt]->Fill(vz0,vx0,rate*niel);

	xy[1][dt]->Fill(xx,yy,rate);
	xyE[1][dt]->Fill(xx,yy,rate*kinE);
	xyNIEL[1][dt]->Fill(xx,yy,rate*niel);

	if(hit->at(j).trid==1 || hit->at(j).trid==2){
	  energy[4][dt]->Fill(kinE,rate);
	  energyNIEL[4][dt]->Fill(kinE,rate*niel);
	  
	  z0[4][dt]->Fill(vz0,rate);
	  z0E[4][dt]->Fill(vz0,rate*kinE);
	  z0NIEL[4][dt]->Fill(vz0,rate*niel);
	  
	  z0r0[4][dt]->Fill(vz0,vr0,rate);
	  z0r0E[4][dt]->Fill(vz0,vr0,rate*kinE);
	  z0r0NIEL[4][dt]->Fill(vz0,vr0,rate*niel);
	  
	  z0x0[4][dt]->Fill(vz0,vx0,rate);
	  z0x0E[4][dt]->Fill(vz0,vx0,rate*kinE);
	  z0x0NIEL[4][dt]->Fill(vz0,vx0,rate*niel);

	  xy[4][dt]->Fill(xx,yy,rate);
	  xyE[4][dt]->Fill(xx,yy,rate*kinE);
	  xyNIEL[4][dt]->Fill(xx,yy,rate*niel);

	}

      }

      if(dt!=0) continue;
      
      double phi = atan2(hit->at(j).y,hit->at(j).x);
      if(phi<0) phi+=2*pi;
      int foundRing = findDetector(sector, phi, hit->at(j).r,1);
      if(foundRing==-1) continue;

      if(foundRing==4){
	z0R5[sp][dt]->Fill(vz0,rate);
	z0R5E[sp][dt]->Fill(vz0,rate*kinE);
	z0R5NIEL[sp][dt]->Fill(vz0,rate*niel);

	if(sp==0 && kinE>1){
	  z0R5[1][dt]->Fill(vz0,rate);
	  z0R5E[1][dt]->Fill(vz0,rate*kinE);
	  z0R5NIEL[1][dt]->Fill(vz0,rate*niel);
	  if(hit->at(j).trid==1 || hit->at(j).trid==2){
	    z0R5[4][dt]->Fill(vz0,rate);
	    z0R5E[4][dt]->Fill(vz0,rate*kinE);
	    z0R5NIEL[4][dt]->Fill(vz0,rate*niel);
	  }
	}	
      }else if(foundRing==6){
	z0R7[sp][dt]->Fill(vz0,rate);
	z0R7E[sp][dt]->Fill(vz0,rate*kinE);
	z0R7NIEL[sp][dt]->Fill(vz0,rate*niel);

	if(sp==0 && kinE>1){
	  z0R7[1][dt]->Fill(vz0,rate);
	  z0R7E[1][dt]->Fill(vz0,rate*kinE);
	  z0R7NIEL[1][dt]->Fill(vz0,rate*niel);
	  if(hit->at(j).trid==1 || hit->at(j).trid==2){
	    z0R7[4][dt]->Fill(vz0,rate);
	    z0R7E[4][dt]->Fill(vz0,rate*kinE);
	    z0R7NIEL[4][dt]->Fill(vz0,rate*niel);
	  }
	}	
      }
      
      mdHits[sp][0]->SetBinContent(foundRing*3+sector+1,
				   rate + mdHits[sp][0]->GetBinContent(foundRing*3+sector+1));
      mdHitsE[sp][0]->SetBinContent(foundRing*3+sector+1,
				    rate*kinE + mdHitsE[sp][0]->GetBinContent(foundRing*3+sector+1));
      mdHitsNIEL[sp][0]->SetBinContent(foundRing*3+sector+1,
				       rate*niel + mdHitsNIEL[sp][0]->GetBinContent(foundRing*3+sector+1));

      if(kinE<=1){
	mdHits[sp][1]->SetBinContent(foundRing*3+sector+1,
				     rate + mdHits[sp][1]->GetBinContent(foundRing*3+sector+1));
	mdHitsE[sp][1]->SetBinContent(foundRing*3+sector+1,
				      rate*kinE + mdHitsE[sp][1]->GetBinContent(foundRing*3+sector+1));
	mdHitsNIEL[sp][1]->SetBinContent(foundRing*3+sector+1,
					 rate*niel + mdHitsNIEL[sp][1]->GetBinContent(foundRing*3+sector+1));
      }else if(kinE<=10){
	mdHits[sp][2]->SetBinContent(foundRing*3+sector+1,
				     rate + mdHits[sp][2]->GetBinContent(foundRing*3+sector+1));
	mdHitsE[sp][2]->SetBinContent(foundRing*3+sector+1,
				      rate*kinE + mdHitsE[sp][2]->GetBinContent(foundRing*3+sector+1));
	mdHitsNIEL[sp][2]->SetBinContent(foundRing*3+sector+1,
					 rate*niel + mdHitsNIEL[sp][2]->GetBinContent(foundRing*3+sector+1));
      }else{
	mdHits[sp][3]->SetBinContent(foundRing*3+sector+1,
				     rate + mdHits[sp][3]->GetBinContent(foundRing*3+sector+1));
	mdHitsE[sp][3]->SetBinContent(foundRing*3+sector+1,
				      rate*kinE + mdHitsE[sp][3]->GetBinContent(foundRing*3+sector+1));
	mdHitsNIEL[sp][3]->SetBinContent(foundRing*3+sector+1,
					 rate*niel + mdHitsNIEL[sp][3]->GetBinContent(foundRing*3+sector+1));
      }

      if(sp==0 && kinE>1){//species should be all 1
	mdHits[1][0]->SetBinContent(foundRing*3+sector+1,
				     rate + mdHits[sp][0]->GetBinContent(foundRing*3+sector+1));
	mdHitsE[1][0]->SetBinContent(foundRing*3+sector+1,
				      rate*kinE + mdHitsE[sp][0]->GetBinContent(foundRing*3+sector+1));
	mdHitsNIEL[1][0]->SetBinContent(foundRing*3+sector+1,
					 rate*niel + mdHitsNIEL[sp][0]->GetBinContent(foundRing*3+sector+1));

	if(kinE<=1){
	  mdHits[1][1]->SetBinContent(foundRing*3+sector+1,
				      rate + mdHits[1][1]->GetBinContent(foundRing*3+sector+1));
	  mdHitsE[1][1]->SetBinContent(foundRing*3+sector+1,
				       rate*kinE + mdHitsE[1][1]->GetBinContent(foundRing*3+sector+1));
	  mdHitsNIEL[1][1]->SetBinContent(foundRing*3+sector+1,
					  rate*niel + mdHitsNIEL[1][1]->GetBinContent(foundRing*3+sector+1));
	}else if(kinE<=10){
	  mdHits[1][2]->SetBinContent(foundRing*3+sector+1,
				      rate + mdHits[1][2]->GetBinContent(foundRing*3+sector+1));
	  mdHitsE[1][2]->SetBinContent(foundRing*3+sector+1,
				       rate*kinE + mdHitsE[1][2]->GetBinContent(foundRing*3+sector+1));
	  mdHitsNIEL[1][2]->SetBinContent(foundRing*3+sector+1,
					  rate*niel + mdHitsNIEL[1][2]->GetBinContent(foundRing*3+sector+1));
	}else{
	  mdHits[1][3]->SetBinContent(foundRing*3+sector+1,
				      rate + mdHits[1][3]->GetBinContent(foundRing*3+sector+1));
	  mdHitsE[1][3]->SetBinContent(foundRing*3+sector+1,
				       rate*kinE + mdHitsE[1][3]->GetBinContent(foundRing*3+sector+1));
	  mdHitsNIEL[1][3]->SetBinContent(foundRing*3+sector+1,
					  rate*niel + mdHitsNIEL[1][3]->GetBinContent(foundRing*3+sector+1));
	}

	if(hit->at(j).trid==1 || hit->at(j).trid==2){
	  mdHits[4][0]->SetBinContent(foundRing*3+sector+1,
				      rate + mdHits[4][0]->GetBinContent(foundRing*3+sector+1));
	  mdHitsE[4][0]->SetBinContent(foundRing*3+sector+1,
				       rate*kinE + mdHitsE[4][0]->GetBinContent(foundRing*3+sector+1));
	  mdHitsNIEL[4][0]->SetBinContent(foundRing*3+sector+1,
					  rate*niel + mdHitsNIEL[4][0]->GetBinContent(foundRing*3+sector+1));

	  if(kinE<=1){
	    mdHits[4][1]->SetBinContent(foundRing*3+sector+1,
					rate + mdHits[4][1]->GetBinContent(foundRing*3+sector+1));
	    mdHitsE[4][1]->SetBinContent(foundRing*3+sector+1,
					 rate*kinE + mdHitsE[4][1]->GetBinContent(foundRing*3+sector+1));
	    mdHitsNIEL[4][1]->SetBinContent(foundRing*3+sector+1,
					    rate*niel + mdHitsNIEL[4][1]->GetBinContent(foundRing*3+sector+1));
	  }else if(kinE<=10){
	    mdHits[4][2]->SetBinContent(foundRing*3+sector+1,
					rate + mdHits[4][2]->GetBinContent(foundRing*3+sector+1));
	    mdHitsE[4][2]->SetBinContent(foundRing*3+sector+1,
					 rate*kinE + mdHitsE[4][2]->GetBinContent(foundRing*3+sector+1));
	    mdHitsNIEL[4][2]->SetBinContent(foundRing*3+sector+1,
					    rate*niel + mdHitsNIEL[4][2]->GetBinContent(foundRing*3+sector+1));
	  }else{
	    mdHits[4][3]->SetBinContent(foundRing*3+sector+1,
					rate + mdHits[4][3]->GetBinContent(foundRing*3+sector+1));
	    mdHitsE[4][3]->SetBinContent(foundRing*3+sector+1,
					 rate*kinE + mdHitsE[4][3]->GetBinContent(foundRing*3+sector+1));
	    mdHitsNIEL[4][3]->SetBinContent(foundRing*3+sector+1,
					    rate*niel + mdHitsNIEL[4][3]->GetBinContent(foundRing*3+sector+1));
	  }

	}
      }

    }
  }

  fin->Close();
  delete fin;
  return nEntries;
};


void initHisto(){
  string foutNm = Form("%s_radAnaV2.root",fileNm.substr(0,fileNm.find_last_of(".")).c_str());

  fout = new TFile(foutNm.c_str(),"RECREATE");

  for(int j=0;j<nDet;j++){
    fout->mkdir(detH[j].c_str(),Form("%s plane",detH[j].c_str()));
    fout->cd(detH[j].c_str());
    for(int i=0;i<nSp;i++){
      energy[i][j]=new TH1D(Form("%s_energy_%s",detH[j].c_str(),spH[i].c_str()),
			    Form("rate weighted for %s;E [MeV]",spTit[i].c_str()),
			    121,-8,4.1);
      niceLogXBins(energy[i][j]);

      energyNIEL[i][j]=new TH1D(Form("%s_energyNEIL_%s",detH[j].c_str(),spH[i].c_str()),
				Form("rate weighted for %s;E [MeV]",spTit[i].c_str()),
				121,-8,4.1);
      niceLogXBins(energyNIEL[i][j]);

      z0[i][j]=new TH1D(Form("%s_z0_%s",detH[j].c_str(),spH[i].c_str()),
			Form("rate weighted %s;z0[mm]",spTit[i].c_str()),
			2000,-6000,32000);
      z0E[i][j]=new TH1D(Form("%s_z0E_%s",detH[j].c_str(),spH[i].c_str()),
			 Form("rate*E weighted %s;z0[mm]",spTit[i].c_str()),
			 2000,-6000,32000);
      z0NIEL[i][j]=new TH1D(Form("%s_z0NIEL_%s",detH[j].c_str(),spH[i].c_str()),
			    Form("rate*NEIL weighted %s;z0[mm]",spTit[i].c_str()),
			    2000,-6000,32000);

      z0R5[i][j]=new TH1D(Form("%s_z0R5_%s",detH[j].c_str(),spH[i].c_str()),
			  Form("R5 rate weighted %s;z0[mm]",spTit[i].c_str()),
			  2000,-6000,32000);
      z0R5E[i][j]=new TH1D(Form("%s_z0R5E_%s",detH[j].c_str(),spH[i].c_str()),
			   Form("R5 rate*E weighted %s;z0[mm]",spTit[i].c_str()),
			   2000,-6000,32000);
      z0R5NIEL[i][j]=new TH1D(Form("%s_z0R5NIEL_%s",detH[j].c_str(),spH[i].c_str()),
			      Form("R5 rate*NEIL weighted %s;z0[mm]",spTit[i].c_str()),
			      2000,-6000,32000);

      z0R7[i][j]=new TH1D(Form("%s_z0R7_%s",detH[j].c_str(),spH[i].c_str()),
			  Form("R7 rate weighted %s;z0[mm]",spTit[i].c_str()),
			  2000,-6000,32000);
      z0R7E[i][j]=new TH1D(Form("%s_z0R7E_%s",detH[j].c_str(),spH[i].c_str()),
			   Form("R7 rate*E weighted %s;z0[mm]",spTit[i].c_str()),
			   2000,-6000,32000);
      z0R7NIEL[i][j]=new TH1D(Form("%s_z0R7NIEL_%s",detH[j].c_str(),spH[i].c_str()),
			      Form("R7 rate*NEIL weighted %s;z0[mm]",spTit[i].c_str()),
			      2000,-6000,32000);

      z0HE[i][j]=new TH1D(Form("%s_z0HE_%s",detH[j].c_str(),spH[i].c_str()),
			  Form("E>10MeV rate weighted %s 500<R<1500;z0[mm]",spTit[i].c_str()),
			  2000,-6000,32000);
      z0HEE[i][j]=new TH1D(Form("%s_z0HEE_%s",detH[j].c_str(),spH[i].c_str()),
			   Form("E>10MeV rate*E weighted %s 500<R<1500;z0[mm]",spTit[i].c_str()),
			   2000,-6000,32000);
      z0HENIEL[i][j]=new TH1D(Form("%s_z0HENIEL_%s",detH[j].c_str(),spH[i].c_str()),
			      Form("E>10MeV rate*NEIL weighted %s 500<R<1500;z0[mm]",spTit[i].c_str()),
			      2000,-6000,32000);

      xy[i][j]=new TH2D(Form("%s_xy_%s",detH[j].c_str(),spH[i].c_str()),
			Form("rate for %s;x[mm];y[mm]",spTit[i].c_str()),
			200,-2000,2000,
			200,-2000,2000);
      xyE[i][j]=new TH2D(Form("%s_xyE_%s",detH[j].c_str(),spH[i].c_str()),
			 Form("rate*E for %s;x[mm];y[mm]",spTit[i].c_str()),
			 200,-2000,2000,
			 200,-2000,2000);
      xyNIEL[i][j]=new TH2D(Form("%s_xyNIEL_%s",detH[j].c_str(),spH[i].c_str()),
			    Form("rate*NIEL for %s;x[mm];y[mm]",spTit[i].c_str()),
			    200,-2000,2000,
			    200,-2000,2000);

      z0r0[i][j]=new TH2D(Form("%s_z0r0_%s",detH[j].c_str(),spH[i].c_str()),
			  Form("rate for %s;z0[mm];r0[mm]",spTit[i].c_str()),
			  2000,-6000,32000,
			  200,0,3000);
      z0r0E[i][j]=new TH2D(Form("%s_z0r0E_%s",detH[j].c_str(),spH[i].c_str()),
			   Form("rate*E for %s;z0[mm];r0[mm]",spTit[i].c_str()),
			   2000,-6000,32000,
			   200,0,3000);
      z0r0NIEL[i][j]=new TH2D(Form("%s_z0r0NIEL_%s",detH[j].c_str(),spH[i].c_str()),
			      Form("rate*NIEL for %s;z0[mm];r0[mm]",spTit[i].c_str()),
			      2000,-6000,32000,
			      200,0,3000);

      z0x0[i][j]=new TH2D(Form("%s_z0x0_%s",detH[j].c_str(),spH[i].c_str()),
			  Form("rate for %s;z0[mm];x0[mm]",spTit[i].c_str()),
			  2000,-6000,32000,
			  200,-3000,3000);
      z0x0E[i][j]=new TH2D(Form("%s_z0x0E_%s",detH[j].c_str(),spH[i].c_str()),
			   Form("rate*E for %s;z0[mm];x0[mm]",spTit[i].c_str()),
			   2000,-6000,32000,
			   200,-3000,3000);
      z0x0NIEL[i][j]=new TH2D(Form("%s_z0x0NIEL_%s",detH[j].c_str(),spH[i].c_str()),
			      Form("rate*NIEL for %s;z0[mm];x0[mm]",spTit[i].c_str()),
			      2000,-6000,32000,
			      200,-3000,3000);
      if(j==0){
	for(int k=0;k<nErange;k++){
	  mdHits[i][k]=new TH1D(Form("mdHits_%s_ER%d",spH[i].c_str(),k),
				Form("rate per electron for %s with %s",spTit[i].c_str(),eRgTit[k].c_str()),
				nSecDet,0,nSecDet);
	  mdHitsE[i][k]=new TH1D(Form("mdHitsE_%s_ER%d",spH[i].c_str(),k),
				 Form("rate*E per electron for %s with %s",spTit[i].c_str(),eRgTit[k].c_str()),
				 nSecDet,0,nSecDet);
	  mdHitsNIEL[i][k]=new TH1D(Form("mdHitsNIEL_%s_ER%d",spH[i].c_str(),k),
				    Form("rate*NIEL per electron for %s with %s",spTit[i].c_str(),eRgTit[k].c_str()),
				    nSecDet,0,nSecDet);				    

	  const string secNm[3]={"closed","transition","open"};
	  for(int kk=1;kk<=nSecDet;kk++){
	    int ring= (kk-1-(kk-1)%3)/3+1;
	    int sector = (kk-1)%3;
	    mdHits[i][k]->GetXaxis()->SetBinLabel(kk,Form("R%d %s",ring,secNm[sector].c_str()));
	    mdHitsE[i][k]->GetXaxis()->SetBinLabel(kk,Form("R%d %s",ring,secNm[sector].c_str()));
	    mdHitsNIEL[i][k]->GetXaxis()->SetBinLabel(kk,Form("R%d %s",ring,secNm[sector].c_str()));
	  }
	}
      }
    }
  }
}

void writeOutput(){

  double scaleFactor = 1./nFiles;
  if(beamGen)
    scaleFactor = 1./nTotEv;

  for(int j=0;j<nDet;j++){
    fout->cd(detH[j].c_str());
    for(int i=0;i<nSp;i++){
      energy[i][j]->Scale(scaleFactor);
      energy[i][j]->Write();
  
      energyNIEL[i][j]->Scale(scaleFactor);
      energyNIEL[i][j]->Write();

      z0[i][j]->Scale(scaleFactor);
      z0[i][j]->Write();
      z0E[i][j]->Scale(scaleFactor);
      z0E[i][j]->Write();
      z0NIEL[i][j]->Scale(scaleFactor);
      z0NIEL[i][j]->Write();

      z0R5[i][j]->Scale(scaleFactor);
      z0R5[i][j]->Write();
      z0R5E[i][j]->Scale(scaleFactor);
      z0R5E[i][j]->Write();
      z0R5NIEL[i][j]->Scale(scaleFactor);
      z0R5NIEL[i][j]->Write();

      z0R7[i][j]->Scale(scaleFactor);
      z0R7[i][j]->Write();
      z0R7E[i][j]->Scale(scaleFactor);
      z0R7E[i][j]->Write();
      z0R7NIEL[i][j]->Scale(scaleFactor);
      z0R7NIEL[i][j]->Write();

      z0HE[i][j]->Scale(scaleFactor);
      z0HE[i][j]->Write();
      z0HEE[i][j]->Scale(scaleFactor);
      z0HEE[i][j]->Write();
      z0HENIEL[i][j]->Scale(scaleFactor);
      z0HENIEL[i][j]->Write();

      xy[i][j]->Scale(scaleFactor);
      xy[i][j]->Write();
      
      xyE[i][j]->Scale(scaleFactor);
      xyE[i][j]->Write();
      
      xyNIEL[i][j]->Scale(scaleFactor);
      xyNIEL[i][j]->Write();

      z0r0[i][j]->Scale(scaleFactor);
      z0r0[i][j]->Write();

      z0r0E[i][j]->Scale(scaleFactor);
      z0r0E[i][j]->Write();
      
      z0r0NIEL[i][j]->Scale(scaleFactor);
      z0r0NIEL[i][j]->Write();

      if(j==0)
	for(int k=0;k<nErange;k++){
	  mdHits[i][k]->Scale(scaleFactor);
	  mdHits[i][k]->Write();

	  mdHitsE[i][k]->Scale(scaleFactor);
	  mdHitsE[i][k]->Write();

	  mdHitsNIEL[i][k]->Scale(scaleFactor);
	  mdHitsNIEL[i][k]->Write();
	}
    }
  }
  fout->Close();
}
