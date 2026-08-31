// background_fill.C
// Every photon or electron that hits main detector was created somewhere in
// the beamline, in a collimator, shield, quartz tile itself, and so on. This
// macro finds out where each one was born and counts them per component, so we
// can say 'this much of background comes from collimator 2'.
// what this macro does
// 1. loop over every hit on main detector plane and keep the good ones
// (right detector ID, enough energy born outside the target if cut is on, going forwards,
//  e+- or gamma);
// 2. take the point where that particle was created, its vertex (vz,vr), and see
// which component box contains it;
// 3. add 1 to that component's counter
// a component here is just a rectangle in (z,r); z-range and r-range that together
// enclose a real object in the geometry. they are listed in the box[][] table below.
// boxes are checked independently so if two of them overlap, a hit inside both is counted in both.
// nothing is normalized here, this macro only counts. it also stores the total number of beam
// events processed, and the cut that was used, so plotting macro (analysis/background_plotter.C)
// can turn counts into a rate later and always know which mode it has.
// how to run
// build/reroot -l -b -q 'background_fill.C("rootfiles.txt","outputs")'
// build/reroot -l -b -q 'background_fill.C("rootfiles.txt","outputs",28, 5, -3440)'
// arg1 = file list 
// arg2 = output directory
// arg3 = main detector plane detector ID
// arg4 = signal ring
// arg5 = target cut in mm
// vzTgtCut = -1e9 (default), no cut, keep everything
// vzTgtCut = -3440  drop tracks born in the target
// why target cut? with the blocker out simulation, most of what reaches ring 5 was created
// in the target, that is signal, not background. Dropping tracks born at vz <=vzTgtCut
// leaves only beamline background.

int sectorOf(double phi){
  const double pi = acos(-1);
  if(phi < 0) phi += 2*pi;	// atan2 returns [-pi,pi], we want [0,2pi)
  double s = fmod(phi, 2*pi/7);	// where we are inside one septant
  if(s < pi/28) return 0;	// 0,1,2 == closed, transition, open
  if(s < 3*pi/28) return 1;
  if(s < 5*pi/28) return 2;
  if(s < 7*pi/28) return 1;
  return 0;
}

//Is the vertex (vz,vr) inside this component's box?
// box = {z_begin, z_end, r_min, r_max}. Lower edge open, upper edge closed,
// so a vertex sitting exactly on shared edge belongs to one box only.
bool inBox(double vz, double vr, double *box){
  return vz > box[0] && vz <= box[1] && vr > box[2] && vr <= box[3];
}

void background_fill(TString infile = "rootfiles.txt",
		     TString outdir = "background_out",		// output directory
		     int MD = 28,	// main det plane
		     int ring = 5,	// signal ring for counting
		     double vzTgtCut = -1e9)	  //drop tracks born at vz <= this, in mm.
{
  gSystem->mkdir(outdir, kTRUE);

  if(vzTgtCut > -1e8)
    cout<<"target cut ON: dropping tracks created at vz <= "<<vzTgtCut<<"mm"<<endl;
  else
    cout<<"target cut OFF: keeping every track"<<endl;

  //cuts
  const double EMIN = 1.0; // keep hits with total energy above this [MeV]

  //the components
  // One box per component: {z_begin, z_end, r_min, r_max} in mm, world
  // coordinates, listed in order of increasing z.
  const int nComp = 42;
  TString compName[nComp] = { "Target", "Bellows 1", "US Beam Pipe", "Collar 0", "Bellows 2",
			      "US Encl Pipe", "Collimator 1", "Collimator 2", "Two bounce shield", "Collimator 4",
			      "Bellows 3", "Vac Encl US End", "Photon Scraper", "Belly Plates 1", "Belly Plates 2",
			      "Bulkheads", "Belly Plates 3", "Collimator 5", "Belly Plates 4", "Collimator 6A",
			      "Clamp 6A", "Lintel", "Clamps 6B", "Collimator 6B", "Collar 1",
			      "Vac Encl DS End", "Bellows 4", "Drift Cone", "Drift Pipe", "Vacuum Window", "Vac Window Flange",
			      "Collar 2", "Air Box 1", "Air Box 2", "Bellows 5", "Det Beam Pipe",
			      "Quartz R6", "Quartz R5", "Quartz R4", "Quartz R3", "Quartz R2",
			      "Quartz R1" };

  double box[nComp][4] = {
    {-6005,-3440, 0, 40}, {-3439,-2972,100,200}, {-2971,-1310,100, 188},
    {-1309,-1000, 70, 335}, {-999,-400,140,195}, {-399,319,140, 190},
    {320, 905, 10, 31}, {745, 905, 20, 152}, {931, 3094, 20, 65},
    {3220, 3380, 26, 250}, {4177, 4695, 325, 420}, {4595, 4905, 310, 350},
    {4696, 4824, 20, 155}, {4929, 5941, 32, 57}, {5946, 6978, 32, 57},
    {6495, 9005, 350, 900}, {6982, 7965, 32, 57}, {7549, 7777, 57, 300},
    {7975, 9768, 45, 90}, {9550, 9713, 55, 102}, {9551, 9713, 95, 639},
    {9639, 9749, 475, 950}, {10922, 11085, 116, 639}, {10922, 11085, 80, 126},
    {11810, 11971, 540, 760}, {11995, 12185, 700, 1300}, {12238, 12649, 685, 788},
    {12730, 13160, 720, 1275}, {13161, 18826, 1240, 1397}, {18506, 18834, 516, 1074},
    {18849,18923,1006,1402}, {18927, 19288, 1010, 1315}, {19006, 20505, 800, 1009}, 
    {19006, 22505, 640, 800}, {19036, 19332, 400, 640}, {19334, 25477, 500, 696}, 
    {21810, 21990, 1040, 1190}, {22100, 22300, 915, 1105}, {22425, 22605, 810, 985}, 
    {22700, 22880, 765, 880}, {22975, 23155, 720, 830}, {23245, 23425, 705, 785} };

  const int nQuartz = 6;
  const int firstQ = nComp - nQuartz;

  double rMin[7] = {660, 660, 690, 750, 810, 930, 1070};
  double rMax[7] = {1170, 690, 750, 810, 930, 1070, 1170};

  //for summary table
  const int nSlice = 7;
  double vzEdge[8] = {-1e5, -3700, 2000, 8500, 14500, 22200, 35000, 1e6};

  //histogram booking, index 0 is for electrons and positrons. index 1 is for photons
  TString spName[2] = {"e","g"};
  TH1D *hR[2];	// counts vs radius
  TH1D *hVz[2];	// vertex z, quartz born hits removed
  TH1D *hVzAll[2];	// vertex z, everything kept
  TH2D *hVrVz[2];	// map (z vs r) for signal ring

  for(int sp=0;sp<2;sp++){
    hR[sp] = new TH1D("hR_"+spName[sp],"counts vs r; r [mm];count", 140, 600, 1300);
    hVz[sp] = new TH1D("hVz_"+spName[sp],"vertex z; z_{vertex} [mm];count", 200, -6000, 32000);
    hVzAll[sp] = new TH1D("hVzAll_"+spName[sp],"vertex z (no quartz cut); z_{vertex} [mm];count", 200, -6000, 32000);
    hVrVz[sp] = new TH2D("hVrVz_"+spName[sp],"vertex; z_{vertex} [mm];r_{vertex} [mm]", 760, -6000, 32000,160, 0, 1600);
  }

  TH2D *hVrVzAll = new TH2D("hVrVzAll","origin of all MD hits; z_{vertex} [mm]; r_{vertex} [mm]", 644, -6100, 26100, 160, 0, 1600);

  TString xyName[3] = {"all","closed","open"};
  TH2D *hXY[2][3];
  for(int sp=0; sp<2; sp++) for(int q=0; q<3; q++)
			      hXY[sp][q] = new TH2D("hXY_"+spName[sp]+"_"+xyName[q], "MD "+spName[sp]+" hit x vs y ("+xyName[q]+" sectors);x [mm];y [mm]",
						    260,-1300,1300, 260,-1300,1300);

  // counters
  double compCount[2][nComp] = {{0}}; // per component, all sectors
  double compCountC[2][nComp] = {{0}}; // per component, closed sectors
  double compCountO[2][nComp] = {{0}}; // per component, open sectors
  double nUncovered[2] = {0}; // hits whose vertex is in no box
  double sliceRing[2][nSlice][7] = {{{0}}};

  //Building list of input files
  std::vector<TString> files;
  if(infile.EndsWith(".root")) files.push_back(infile);
  else {ifstream fl(infile); string line; while (fl>>line) files.push_back(line); }
  cout <<"Will process "<<files.size()<<" file(s)"<<endl;

  long nPrimary = 0;  // total beam events for our normalization

  for(int fi=0; fi<(int)files.size(); fi++){

    TFile *f = TFile::Open(files[fi]);
    if(!f || f->IsZombie()){ cout<<"skip, cannot open: " <<files[fi]<<endl; continue; }
    TTree *T = (TTree*)f->Get("T");
    if(!T){cout<<"skip, no tree T: "<<files[fi]<<endl; f->Close(); continue; }

    std::vector<remollGenericDetectorHit_t> *hit = 0;
    T->SetBranchAddress("hit", &hit);

    long nEv = T->GetEntries();
    nPrimary += nEv;     // 1 entry = 1 beam event
    if(fi%50==0) cout<<"["<<fi+1<<"/"<<files.size()<<"] "<<files[fi]<<" ("<<nEv<<" ev)"<<endl;

    for(long i=0;i<nEv;i++){
      T->GetEntry(i);

      for(int j=0;j<(int)hit->size();j++){

	remollGenericDetectorHit_t &h = hit->at(j);

	// cut 1: only main detector plane
	if(h.det != MD ) continue;

	// cut 2: energy
	if(h.e <= EMIN) continue;

	// cut 3: keep electrons/positrons(+-11) and photons (22)
	int sp = (abs(h.pid)==11) ? 0 : (h.pid==22 ? 1 : -1);
	if (sp < 0) continue;

	// cut 4: drop anything created in target, so only background is left.
	if(h.vz <= vzTgtCut) continue;

	// cut5: forward going only
	if(h.pz <=0) continue;

	double r = h.r;

	// the vertex, where the particle was created
	double vz = h.vz;
	double vr = sqrt(h.vx*h.vx + h.vy*h.vy);

	int sec = sectorOf(atan2(h.y,h.x)); // 0 closed, 1 transition, 2 open

	hR[sp]->Fill(r);
	hVrVzAll->Fill(vz, vr); 

	if(r > rMin[0] && r <= rMax[0]){
	  hXY[sp][0]->Fill(h.x,h.y);   // all sectors
	  if(sec==0) hXY[sp][1]->Fill(h.x,h.y);  // closed
	  if(sec==2) hXY[sp][2]->Fill(h.x,h.y);  // open
	}

	//signal ring only
	if(r > rMin[ring] && r <= rMax[ring]){

	  bool inQuartz = false;
	  for(int c=firstQ; c< nComp; c++) if(inBox(vz,vr,box[c])) inQuartz = true;

	  if (!inQuartz) hVz[sp]->Fill(vz);  // quartz excluded
	  hVzAll[sp]->Fill(vz);   // everything, quartz included
	  hVrVz[sp]->Fill(vz,vr);

	  // find which components the vertex falls in
	  bool anyBox = false;
	  for(int c=0; c< nComp; c++){

	    if(!inBox(vz,vr,box[c])) continue;
	    anyBox = true;
	    compCount[sp][c]++;	// all sectors
	    if(sec==0) compCountC[sp][c]++;	// closed only
	    if(sec==2) compCountO[sp][c]++;	// open only
	  }

	  // no box at all
	  if(!anyBox) nUncovered[sp]++;
	}

	// z-slice x ring counts, for summary table
	for(int s=0; s<nSlice; s++){
	  if(vz > vzEdge[s] && vz <= vzEdge[s+1]){
	    for(int k=0;k<7;k++)
	      if(r > rMin[k] && r <= rMax[k]) sliceRing[sp][s][k]++;
	    break;
	  }
	}
      } // hits
    } // events

    f->Close(); delete f;

  } // files

  //print outs
  cout<<"Done. Total beam events processed, nPrimary = " << nPrimary<<endl;
  for(int sp=0; sp<2; sp++){
    double tot = 0;
    for(int c =0; c<nComp; c++) tot+= compCount[sp][c];
    cout << spName[sp] << " --> signal ring hits in no box: " << nUncovered[sp] << " (sum of bars " << tot << ", overlaps counted more than once)" << endl;
}

  // Turn plain counters into histograms, one bin per component
  TH1D *hComp[2], *hCompC[2], *hCompO[2];
  TH2D *hSlice[2];
  for(int sp=0; sp<2; sp++){
    hComp[sp] = new TH1D("hComp_"+spName[sp], "component counts;;count",nComp,0,nComp);
    hCompC[sp] = new TH1D("hComp_"+spName[sp]+"_closed", "component counts (closed sectors);;count",nComp,0,nComp);
    hCompO[sp] = new TH1D("hComp_"+spName[sp]+"_open", "component counts (open sectors);;count",nComp,0,nComp);
    for(int c=0; c<nComp; c++){
	hComp[sp]->SetBinContent(c+1,compCount[sp][c]);
	hCompC[sp]->SetBinContent(c+1,compCountC[sp][c]);
	hCompO[sp]->SetBinContent(c+1,compCountO[sp][c]);
	hComp[sp]->GetXaxis()->SetBinLabel(c+1,compName[c]);
	hCompC[sp]->GetXaxis()->SetBinLabel(c+1,compName[c]);
	hCompO[sp]->GetXaxis()->SetBinLabel(c+1,compName[c]);
    }
    hSlice[sp] = new TH2D ("hSlice_"+spName[sp],"vz-slice x ring counts; slice; ring",nSlice,0,nSlice,7,0,7);
    for(int s=0; s<nSlice; s++) for (int k=0;k<7;k++)
				    hSlice[sp]->SetBinContent(s+1,k+1, sliceRing[sp][s][k]);
  }

  //nPrimary and uncovered counts go in as histograms
  TH1D *hNprimary = new TH1D("hNprimary","total beam events processed",1,0,1);
  hNprimary->SetBinContent(1, nPrimary);

  //record the cut used
  TH1D *hTgtCut = new TH1D("hTgtCut","target vz cut used [mm]; -1e9 means no cut",1,0,1);
  hTgtCut->SetBinContent(1, vzTgtCut);

  TH1D *hUncov = new TH1D("hUncovered","ring hits outside every box;species;count",2,0,2);
  for(int sp=0;sp<2;sp++){
    hUncov->SetBinContent(sp+1, nUncovered[sp]);
    hUncov->GetXaxis()->SetBinLabel(sp+1, spName[sp]);

  }

  //Write everything into ROOT file
  TFile *fout = new TFile(outdir+"/background_counts.root","RECREATE");
  hNprimary->Write();
  hTgtCut->Write();
  hUncov->Write();
  hVrVzAll->Write();
  for(int sp=0;sp<2; sp++){
    hR[sp]->Write(); hVz[sp]->Write(); hVzAll[sp]->Write(); hVrVz[sp]->Write();
    hComp[sp]->Write(); hCompC[sp]->Write(); hCompO[sp]->Write(); hSlice[sp]->Write();
    for(int q=0;q<3;q++) hXY[sp][q]->Write();
  }
  fout->Close();
  
  //cross check plot
  gStyle->SetOptStat(0);
  TCanvas *cchk = new TCanvas("cchk","boxes check",1100,650);
  cchk->SetLogz(); cchk->SetRightMargin(0.13);
  hVrVzAll->Draw("colz");
  for(int c=0;c< nComp; c++){
    if(box[c][1] <= -6100 || box[c][0] >= 26100) continue;
    TBox *bx = new TBox(box[c][0], box[c][2], box[c][1], box[c][3]);
    bx->SetFillStyle(0); bx->SetLineColor(kRed); bx->SetLineWidth(1); bx->Draw("l");
    TText *tt = new TText(0.5*(box[c][0]+box[c][1]),box[c][3], compName[c]);
    tt->SetTextSize(0.011); tt->SetTextColor(kRed+1); tt->SetTextAngle(90); tt->Draw();

  }
  cchk->SaveAs(outdir+"/boxes_check.pdf");
  cout<<"Wrote "<<outdir<<"/background_counts.root"<<endl;
  cout<<"Also wrote "<<outdir<<"/boxes_check.pdf"<<endl;
}
