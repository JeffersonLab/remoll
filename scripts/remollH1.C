#include "remollH1.h"
#include "remollAna.h"


remollH1::remollH1(Double_t *v, Index_t idx, remollAna *t, Double_t min, Double_t max, const char *name, const char *title, const char *xaxis, const char *yaxis) :
    fVar(v), fIdx(idx), fT(t), fMin(min), fMax(max), fName(name), fTitle(title) {


        //Main Moller ring	
	fRmin = 0.9;
	fRmax = 1.06;

	if (strcmp(fName,"apv_ee") == 0 ) fNbin = 300;
	if (strcmp(fName,"apv_ep") == 0 ) fNbin = 200;
	if (strcmp(fName,"apv_in") == 0 ) fNbin = 400;
	else if (strcmp(fName,"rad") == 0 ) fNbin = 140;
	else if (strcmp(fName,"thcom") == 0 ) fNbin = 180;
	else if (strcmp(fName,"th") == 0 ) fNbin = 100;
	else if (strcmp(fName,"energy_det") == 0 ) fNbin = 150;
	else if (strcmp(fName,"q2") == 0 ) fNbin = 150;
	else if (strcmp(fName,"vert_z0") == 0 ) fNbin = 160;
	else if (strcmp(fName,"vert_y0") == 0 ) fNbin = 100;
	else if (strcmp(fName,"vert_x0") == 0 ) fNbin = 100;
	else if (strcmp(fName,"W2_in") == 0 ) fNbin = 200;
	else fNbin = 300; //default (just in case)

	/*
	   Rcut_t ridx;
	   Sect_t sidx;
	   Vert_t vidx;
	   */

	Int_t ridx, sidx, vidx;

	TString rtag, stag, vtag;

	for( ridx = kNoCut; ridx <= kCut; ridx++ ){
	    for( sidx = kSect1; sidx <= kAll; sidx++ ){
		for( vidx = kUp; vidx <= kFull; vidx++ ){
		    // Build strings
		    switch(ridx){
			case kCut:
			    rtag = Form(", %3.2f < R < %3.2f m", fRmin, fRmax);
			    break;
			case kNoCut:
			default:
			    rtag = "";		  
			    break;
		    }

		    switch(sidx){
			case kAll:
			    stag = "";
			    break;
			default:
			    stag = Form(", Sec %d", sidx+1);
			    break;
		    }

		    switch(vidx){
			case kUp:
			    vtag = ", Upstream";
			    break;
			case kMid:
			    vtag = ", Middle";
			    break;
			case kDown:
			    vtag = ", Downstream";
			    break;
			case kFull:
			default:
			    vtag = "";
			    break;
		    }

		    fH[ridx][sidx][vidx] = new TH1F( Form("%s_%d_%d_%d", fName.Data(), ridx, sidx, vidx),
			    Form("%s%s%s%s", fTitle.Data(), rtag.Data(), stag.Data(), vtag.Data()),
			    fNbin, fMin, fMax);
		    if(xaxis) fH[ridx][sidx][vidx]->GetXaxis()->SetTitle(xaxis);
		    fH[ridx][sidx][vidx]->GetXaxis()->CenterTitle();
		    if(yaxis) fH[ridx][sidx][vidx]->GetYaxis()->SetTitle(yaxis);
		    fH[ridx][sidx][vidx]->GetYaxis()->CenterTitle();
		    fH[ridx][sidx][vidx]->Sumw2();
		}
	    }
	}
	return;
    }

remollH1::~remollH1(){
}

void remollH1::Fill(const char *gen){

    //Rcut_t ridx;  Sect_t sidx,  Vert_t vidx;
    Int_t ridx, sidx, vidx;

    sidx = -10000;
    vidx = -10000;
    ridx = -10000;

    Int_t ev_idx, hit_idx;

    Bool_t fill_it = 0;
    if(strcmp(gen,"moller") == 0 ) {
      if( fT->ev_thcom > 53 && fT->ev_thcom < 127) {
	fill_it= 1;
      }
    }else if(strcmp(gen,"elastic") == 0 ) {
	fill_it= 1;
    }else if(strcmp(gen,"inelastic") == 0 ) {
	fill_it= 1;
    }

    if(fill_it) {
      for( ev_idx = 0; ev_idx < fT->ev_npart; ev_idx++ ){
	//   Determine vertex position
	if( fT->ev_vz[ev_idx] < -0.65 ){
	  vidx = kUp;
	}
	if( -0.05 < fT->ev_vz[ev_idx] && fT->ev_vz[ev_idx] <0.05 ){
	  vidx = kMid;
	}
	if( 0.65 < fT->ev_vz[ev_idx] ){
	  vidx = kDown;
	}
	
	// Go through hits
	for( hit_idx = 0; hit_idx < fT->hit_n; hit_idx++ ){
	  if( fT->hit_det[hit_idx] != 28 ) continue; // Only pay attention to det 28	  
	  // Match hit to generated particle
	  if( fT->hit_trid[hit_idx] == ev_idx+1 ){
	    // Determine sector
	    double hitphi = atan2(fT->hit_y[hit_idx],fT->hit_x[hit_idx]);
	    if( hitphi < 0 ) hitphi += 2.0*3.14159;
	    double secphi = fmod(hitphi, 2.0*3.14159/7 );
	    
	    // logic from onefile.C
	    if( secphi < 3.14159/28. ){ sidx = kSect1; }
	    else if(  secphi < 3.0*3.14159/28. ){ sidx = kSect2; }
	    else if(  secphi < 5.0*3.14159/28. ){ sidx = kSect3; }
	    else if(  secphi < 7.0*3.14159/28. ){ sidx = kSect2; }
	    else { sidx = kSect1; }

	    if (strcmp(fName,"thcom") == 0 && fT->hit_trid[hit_idx] == 2) fT->ev_thcom = 180 - fT->ev_thcom;
	    
	    if( fRmin < fT->hit_r[hit_idx] && fT->hit_r[hit_idx] < fRmax ){
	      if(sidx >= 0 && vidx >= 0) FillOne(fH[kCut][sidx][vidx], ev_idx, hit_idx);
	      if(vidx >= 0) FillOne(fH[kCut][kAll][vidx], ev_idx, hit_idx);
	      if(sidx >= 0) FillOne(fH[kCut][sidx][kFull], ev_idx, hit_idx);
	      FillOne(fH[kCut][kAll][kFull], ev_idx, hit_idx);
	    }
	    
	    if( sidx >= 0 && vidx >= 0 ) FillOne(fH[kNoCut][sidx][vidx], ev_idx, hit_idx);
	    if( vidx >= 0 ) FillOne(fH[kNoCut][kAll][vidx], ev_idx, hit_idx);
	    if( sidx >= 0 ) FillOne(fH[kNoCut][sidx][kFull], ev_idx, hit_idx);
	    FillOne(fH[kNoCut][kAll][kFull], ev_idx, hit_idx);
	  }
	  
	}
      }
    }
    
    return;
}

void remollH1::FillOne( TH1F *h, int ev, int hit ){
    Double_t thisrate = fT->rate*1e-9/fT->fNfile;

    // Normalize to bin width
    //thisrate /= (fMax-fMin)/fNbin;

    switch( fIdx ){
	case kSingle:
	    h->Fill(fVar[0], thisrate);
	    break;
	case kEvent:
	    h->Fill(fVar[ev], thisrate);
	    break;
	case kHit:
	    h->Fill(fVar[hit], thisrate);
	    break;
	default:
	    break;
    }
    return;
}

void remollH1::Draw(Rcut_t rcut, const char *gen){
    Int_t sidx, vidx;

    TCanvas *c = new TCanvas();

    Color_t color[4] = {kBlue, kGreen, kRed+2};

    Double_t xmin = 0.70, xmax=0.999;
    Double_t ymax=0.999, deltay=0.045, ymin=ymax-deltay;
    char label[256];
    TPaveLabel *pt;
    TPaveLabel *pt_main;
    pt_main = new TPaveLabel(xmin,ymin,xmax,ymax,"Entries  Mean  RMS  Int #pm err","NDC");
    pt_main->SetBorderSize(1);
    pt_main->SetFillColor(0);
    Bool_t first;	
    Double_t integral = 0.0;
    Double_t integralErr = 0.0;
    
    vidx = kFull;
    fH[rcut][kAll][vidx]->Draw(); //Do this first to get labelling and optimal y-range to view histograms
    fH[rcut][kAll][vidx]->SetLineColor(kBlack);
    fH[kNoCut][kAll][vidx]->Draw("same");
    fH[kNoCut][kAll][vidx]->SetLineColor(kCyan);
    fH[rcut][kAll][vidx]->Draw("same"); //Do this twice to make sure black is on top of cyan
    fH[rcut][kAll][vidx]->SetLineColor(kBlack);
    first=1;	
    ymax=0.999; 
    deltay=0.045;
    ymin=ymax-deltay;
    ymax=ymin;
    ymin=ymax-deltay;
    if (first) {
      pt_main->Draw();
      first=0;
    }
    integral = fH[kNoCut][kAll][vidx]->IntegralAndError(0,-1, integralErr);
    sprintf(label,"%.0f  %.4g  %.4g  %.4g #pm %.4g",fH[kNoCut][kAll][vidx]->GetEntries(),fH[kNoCut][kAll][vidx]->GetMean(),fH[kNoCut][kAll][vidx]->GetRMS(),integral, integralErr);
    pt = new TPaveLabel(xmin,ymin,xmax,ymax,label,"NDC");
    pt->SetBorderSize(1);
    pt->SetFillColor(0);
    pt->SetTextColor(kCyan);
    pt->Draw();
    ymax=ymin;
    ymin=ymax-deltay;
    
    integral = fH[rcut][kAll][vidx]->IntegralAndError(0,-1, integralErr);
    sprintf(label,"%.0f  %.4g  %.4g  %.4g #pm %.4g",fH[rcut][kAll][vidx]->GetEntries(),fH[rcut][kAll][vidx]->GetMean(),fH[rcut][kAll][vidx]->GetRMS(), integral, integralErr);
    pt = new TPaveLabel(xmin,ymin,xmax,ymax,label,"NDC");
    pt->SetBorderSize(1);
    pt->SetFillColor(0);
    pt->SetTextColor(kBlack);
    pt->Draw();
    ymax=ymin;
    ymin=ymax-deltay;
    
    for( sidx = kSect1; sidx <= kSect3; sidx++ ){
      fH[rcut][sidx][vidx]->SetLineColor(color[sidx]);
      fH[rcut][sidx][vidx]->Draw("same");
      integral = fH[rcut][sidx][vidx]->IntegralAndError(0,-1, integralErr);
      sprintf(label,"%.0f  %.4g  %.4g  %.4g #pm %.4g",fH[rcut][sidx][vidx]->GetEntries(),fH[rcut][sidx][vidx]->GetMean(),fH[rcut][sidx][vidx]->GetRMS(), integral, integralErr);
      pt = new TPaveLabel(xmin,ymin,xmax,ymax,label,"NDC");
      pt->SetBorderSize(1);
      pt->SetFillColor(0);
      pt->SetTextColor(color[sidx]);
      pt->Draw();
      ymax=ymin;
      ymin=ymax-deltay;
      
    }
    
    char plotname[256];
    char hist_name[128];
    sprintf(hist_name, "%s-", fH[kNoCut][kAll][kFull]->GetName());
    strcpy(strchr(hist_name,'_'),strchr(hist_name,'-'));
    sprintf(plotname,"figs/%s%s_R5.png", hist_name, gen);
    c->Print(plotname);
    
    
    return;
}

void remollH1::Write(const char *gen){
  unsigned int i, j, k;
  TFile *outfile=new TFile(Form("rootFiles/remollAna_%s.root",gen),"update");
  for( i = 0; i < 2; i++ ){
    for( j = 0; j < 4; j++ ){
      for( k = 0; k < 4; k++ ){
	fH[i][j][k]->Write("", TObject::kOverwrite);
      }
    }
  }
  
    outfile->Close();
    return;
}
