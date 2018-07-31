void getdose(){
    gStyle->SetOptStat(0);

    TChain *T = new TChain("T");
    T->Add("output/*dose*.root");
//    T->Add("output/output_moller_dose_20140310_204406_1.root");

    double curr = 75e-6; // Originally 70 microamps
    double nelec_s = curr/1.6e-19;
    double scale = nelec_s/T->GetEntries();

    printf("scale = %g\n", scale);

    TH1F *hEdep[2];
    TH1F *hwEdep[2];
    TH1F *hp[2][50];
    TH1F *hwp[2][50];
    TH1F *hvz[2][50];

    TH1F *hpEdep[2][50];
    TH2F *hdose[2][50];

    TH2F *htotaldose[2];

    int part[50] = {11, -11, 22, 2112, 2122, -211, 211,  0};

    char partname[255][50] = {
	"electron",
	"positron",
	"photon  ",
	"neutron ",
	"proton  ",
	"#pi^{-} ",
	"#pi^{+} ",
    };

    double mass[50] = {
	0.511e-3,
	0.511e-3,
	0,
	0.939,
	0.938,
	0.140,
	0.140,
    };

    int idx;
    int npart = 0;

    double Edephigh = 4.0;
    int Edepnbin = 100; 

    hEdep[0] = new TH1F(Form("hEdep_%d", 0), Form("Upstream coil, energy deposition sum"), Edepnbin, 0, Edephigh );
    hEdep[1] = new TH1F(Form("hEdep_%d", 1), Form("Downstream coil, energy deposition sum"), Edepnbin, 0, Edephigh );
    T->Project("hEdep_0", "sum.edep", Form("%g*(sum.det>=1000 && sum.det < 2000 && sum.pid==0)", scale*((double) Edepnbin)/Edephigh));
    T->Project("hEdep_1", "sum.edep", Form("%g*(sum.det>=2000 && sum.det < 3000 && sum.pid==0)", scale*Edepnbin/Edephigh));

    hwEdep[0] = new TH1F(Form("hwEdep_%d", 0), Form("Upstream coil, energy deposition sum"), Edepnbin, 0, Edephigh );
    hwEdep[1] = new TH1F(Form("hwEdep_%d", 1), Form("Downstream coil, energy deposition sum"), Edepnbin, 0, Edephigh );
    T->Project("hwEdep_0", "sum.edep", Form("%g*sum.edep*(sum.det>=1000 && sum.det < 2000 && sum.pid==0 && sum.edep>1e-20)", scale));
    T->Project("hwEdep_1", "sum.edep", Form("%g*sum.edep*(sum.det>=2000 && sum.det < 3000 && sum.pid==0 && sum.edep>1e-20)", scale));

    hEdep[0]->GetXaxis()->SetTitle("Energy Deposition [GeV]");
    hEdep[0]->GetXaxis()->CenterTitle();
    hEdep[0]->GetYaxis()->SetTitle("Rate [GeV^{-1}s^{-1}]");
    hEdep[0]->GetYaxis()->CenterTitle();

    hEdep[1]->GetXaxis()->SetTitle("Energy Deposition [GeV]");
    hEdep[1]->GetXaxis()->CenterTitle();
    hEdep[1]->GetYaxis()->SetTitle("Rate [GeV^{-1}s^{-1}]");
    hEdep[1]->GetYaxis()->CenterTitle();

    double rmax_up = 0.3;
    double z_up_min = 5.7;
    double z_up_max = 8.2;

    double rmax_dn = 0.42;
    double z_dn_min = 9.5;
    double z_dn_max = 17.5;

    int dose_upbinx = 100;
    int dose_upbiny = 50;

    int dose_dnbinx = 100;
    int dose_dnbiny = 50;

    double maxdose = 12e-3;

    // Coils are roughly 6cm in cross section?  Divide by 7 to average over coils
    double Cu_density = 0.009*pow(10.,3); // kg/m^3
    double massper_upbin = Cu_density*0.06/(rmax_up/dose_upbiny)/((z_up_max-z_up_min)/dose_upbinx)/7.; // kg/m^2
    double massper_dnbin = Cu_density*0.06/(rmax_dn/dose_dnbiny)/((z_dn_max-z_dn_min)/dose_dnbinx)/7.; // kg/m^2


    htotaldose[0] = new TH2F(Form("htotaldose_%d", 0), Form("Upstream coil, Total Dose"), dose_upbinx, z_up_min, z_up_max, dose_upbiny, 0.0, rmax_up);
    htotaldose[1] = new TH2F(Form("htotaldose_%d", 1), Form("Downstream coil, Total Dose"), dose_dnbinx, z_dn_min, z_dn_max, dose_dnbiny, 0.0, rmax_dn);
    T->Project("htotaldose_0", "sqrt(sum.x*sum.x+sum.y*sum.y):sum.z", Form("%g*sum.edep*(sum.det>=1000 && sum.det < 2000 && sum.pid==0)", scale*1.6e-10/massper_upbin));
    T->Project("htotaldose_1", "sqrt(sum.x*sum.x+sum.y*sum.y):sum.z", Form("%g*sum.edep*(sum.det>=2000 && sum.det < 3000 && sum.pid==0)", scale*1.6e-10/massper_dnbin));

    htotaldose[0]->SetTitle("Total Dose Rate [rad/s], Upstream Coil");
    htotaldose[0]->GetXaxis()->SetTitle("z [m]");
    htotaldose[0]->GetXaxis()->CenterTitle();
    htotaldose[0]->GetYaxis()->SetTitle("r [m]");
    htotaldose[0]->GetYaxis()->CenterTitle();
    htotaldose[0]->SetMaximum(maxdose);

    htotaldose[1]->SetTitle("Total Dose Rate [rad/s], Downstream Coil");
    htotaldose[1]->GetXaxis()->SetTitle("z [m]");
    htotaldose[1]->GetXaxis()->CenterTitle();
    htotaldose[1]->GetYaxis()->SetTitle("r [m]");
    htotaldose[1]->GetYaxis()->CenterTitle();
    htotaldose[1]->SetMaximum(maxdose);

    int nbin = 100;
    double phigh[50] = {11, 11, 11, 1, 11, 11, 11};

    double dephigh[50] = {4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0};

    double vzlo = -2;
    double vzhi = 20;

    int j;
    for( idx = 0; part[idx] != 0; idx++ ){
	hp[0][idx] = new TH1F(Form("hp_%d_%d", 0, idx), Form("Upstream coil, %s momentum spectrum", partname[idx]), nbin, 0, phigh[idx]);
	hpEdep[0][idx] = new TH1F(Form("hpEdep_%d_%d", 0, idx), Form("Upstream coil, %s Edep spectrum", partname[idx]), nbin, 0, dephigh[idx]);
	hwp[0][idx] = new TH1F(Form("hwp_%d_%d", 0, idx), Form("Upstream coil, %s momentum spectrum", partname[idx]), nbin, 0, phigh[idx]);
	hvz[0][idx] = new TH1F(Form("hvz_%d_%d", 0, idx), Form("Upstream coil, %s origin", partname[idx]), nbin, vzlo, vzhi);

	hp[1][idx] = new TH1F(Form("hp_%d_%d", 1, idx), Form("Downstream coil, %s momentum spectrum", partname[idx]), nbin, 0, phigh[idx] );
	hpEdep[1][idx] = new TH1F(Form("hpEdep_%d_%d", 1, idx), Form("Downstream coil, %s Edep spectrum", partname[idx]), nbin, 0, dephigh[idx]);
	hwp[1][idx] = new TH1F(Form("hwp_%d_%d", 1, idx), Form("Downstream coil, %s momentum spectrum", partname[idx]), nbin, 0, phigh[idx] );
	hvz[1][idx] = new TH1F(Form("hvz_%d_%d", 1, idx), Form("Downstream coil, %s origin", partname[idx]), nbin, vzlo, vzhi);

	hdose[0][idx] = new TH2F(Form("hdose_%d_%d", 0, idx), Form("%s Dose Rate [rad/s], Upstream Coil", partname[idx]), dose_upbinx, z_up_min, z_up_max, dose_upbiny, 0.0, rmax_up);
	hdose[1][idx] = new TH2F(Form("hdose_%d_%d", 1, idx), Form("%s Dose Rate [rad/s], Downstream Coil", partname[idx]), dose_dnbinx, z_dn_min, z_dn_max, dose_dnbiny, 0.0, rmax_dn);
	hdose[0][idx]->SetMaximum(maxdose);
	hdose[1][idx]->SetMaximum(maxdose);


	for( j = 0; j < 2; j++ ){
	    hp[j][idx]->GetXaxis()->SetTitle("p [GeV]");
	    hp[j][idx]->GetXaxis()->CenterTitle();
	    hp[j][idx]->GetYaxis()->SetTitle("Rate [GeV^{-1}s^{-1}]");
	    hp[j][idx]->GetYaxis()->CenterTitle();

	    hpEdep[j][idx]->GetXaxis()->SetTitle("E_{dep} [GeV]");
	    hpEdep[j][idx]->GetXaxis()->CenterTitle();
	    hpEdep[j][idx]->GetYaxis()->SetTitle("Rate [GeV^{-1}s^{-1}]");
	    hpEdep[j][idx]->GetYaxis()->CenterTitle();


	    hvz[j][idx]->GetXaxis()->SetTitle("vz [m]");
	    hvz[j][idx]->GetXaxis()->CenterTitle();
	    hvz[j][idx]->GetYaxis()->SetTitle("Rate [cm^{-1}s^{-1}]");
	    hvz[j][idx]->GetYaxis()->CenterTitle();

	    hp[j][idx]->SetTitleSize(0.15);
	    hpEdep[j][idx]->SetTitleSize(0.15);
	    hvz[j][idx]->SetTitleSize(0.15);

	    hp[j][idx]->GetXaxis()->SetTitleSize(0.08);
	    hp[j][idx]->GetXaxis()->SetTitleOffset(0.4);
	    hp[j][idx]->GetYaxis()->SetTitleSize(0.08);
	    hp[j][idx]->GetYaxis()->SetTitleOffset(0.4);

	    hpEdep[j][idx]->GetXaxis()->SetTitleSize(0.08);
	    hpEdep[j][idx]->GetXaxis()->SetTitleOffset(0.4);
	    hpEdep[j][idx]->GetYaxis()->SetTitleSize(0.08);
	    hpEdep[j][idx]->GetYaxis()->SetTitleOffset(0.4);

	    hvz[j][idx]->GetXaxis()->SetTitleSize(0.08);
	    hvz[j][idx]->GetXaxis()->SetTitleOffset(0.4);
	    hvz[j][idx]->GetYaxis()->SetTitleSize(0.08);
	    hvz[j][idx]->GetYaxis()->SetTitleOffset(0.4);


	    hp[j][idx]->GetXaxis()->SetLabelSize(0.08);
	    hp[j][idx]->GetXaxis()->SetLabelOffset(0.0);
	    hp[j][idx]->GetYaxis()->SetLabelSize(0.08);
	    hp[j][idx]->GetYaxis()->SetLabelOffset(0.0);

	    hpEdep[j][idx]->GetXaxis()->SetLabelSize(0.08);
	    hpEdep[j][idx]->GetXaxis()->SetLabelOffset(0.0);
	    hpEdep[j][idx]->GetYaxis()->SetLabelSize(0.08);
	    hpEdep[j][idx]->GetYaxis()->SetLabelOffset(0.0);

	    hvz[j][idx]->GetXaxis()->SetLabelSize(0.08);
	    hvz[j][idx]->GetXaxis()->SetLabelOffset(0.0);
	    hvz[j][idx]->GetYaxis()->SetLabelSize(0.08);
	    hvz[j][idx]->GetYaxis()->SetLabelOffset(0.0);

	    hdose[j][idx]->GetXaxis()->SetTitle("z [m]");
	    hdose[j][idx]->GetXaxis()->CenterTitle();
	    hdose[j][idx]->GetYaxis()->SetTitle("r [m]");
	    hdose[j][idx]->GetYaxis()->CenterTitle();
	    hdose[j][idx]->SetTitleSize(0.15);
	    hdose[j][idx]->GetXaxis()->SetLabelSize(0.08);
	    hdose[j][idx]->GetXaxis()->SetLabelOffset(0.0);
	    hdose[j][idx]->GetYaxis()->SetLabelSize(0.08);
	    hdose[j][idx]->GetYaxis()->SetLabelOffset(0.0);

	    T->Project(Form("hp_%d_%d", j, idx), "hit.p", Form("%g*(hit.det>=%d && hit.det < %d && hit.pid==%d)", scale*nbin/phigh[idx], 1000*(j+1), 1000*(j+2),  part[idx]));
	    T->Project(Form("hpEdep_%d_%d", j, idx), "sum.edep", Form("%g*(sum.det>=%d && sum.det < %d && sum.pid==%d && sum.edep>1e-20)", scale*nbin/dephigh[idx], 1000*(j+1), 1000*(j+2),  part[idx]));
	    // Weighted by kinetic energy
	    T->Project(Form("hwp_%d_%d", j, idx), "hit.p", Form("%g*(sqrt(hit.p*hit.p+%g*%g)-%g)*(hit.det>=%d && hit.det < %d && hit.pid==%d)", scale,1000*(j+1), mass[j], mass[j], mass[j], 1000*(j+2), part[idx]));
	    T->Project(Form("hvz_%d_%d", j, idx), "hit.vz", Form("%g*(hit.det>=%d && hit.det < %d && hit.pid==%d)", scale*nbin/phigh[idx],1000*(j+1), 1000*(j+2), part[idx]));


	    T->Project(Form("hdose_%d_%d", j, idx), "sqrt(sum.x*sum.x+sum.y*sum.y):sum.z", Form("%g*sum.edep*(sum.det>=%d && sum.det < %d && sum.pid==%d)", scale*1.6e-10/massper_upbin, 1000*(j+1), 1000*(j+2), part[idx]));


	}
	npart++;
    }

    double power = 0.0;

    TCanvas *c3 = new TCanvas("c3", "Edep", 900, 500);
    c3->Divide(2, 1);
    c3->cd(1);
    gPad->SetLogy();
    hEdep[0]->Draw();

    // J per GeV = 1.6e-10
    power = hwEdep[0]->Integral()*1.6e-10;
    printf("Upstream power = %f\n", power);
    TPaveLabel *Plab0 = new TPaveLabel(0.37, 0.76, 0.85, 0.87, Form("%5.1f W @ %4.1f #muA", power, curr*1e6), "NDC");
    Plab0->Draw();

    c3->cd(2);
    gPad->SetLogy();
    hEdep[1]->Draw();

    power = hwEdep[1]->Integral()*1.6e-10;
    TPaveLabel *Plab1 = new TPaveLabel(0.37, 0.76, 0.85, 0.87, Form("%5.1f W @ %4.1f #muA", power, curr*1e6), "NDC");
    Plab1->Draw();


    c3->Print("20140312/edep.png");
    c3->Print("20140312/edep.pdf");


    double wsum1, wsum2;

    int i;

    wsum1=wsum2 = 0.0;
    for( i = 0; i < npart; i++ ){

	wsum1 += hwp[0][i]->Integral()*1.6e-10;
	wsum2 += hwp[1][i]->Integral()*1.6e-10;

	printf("%s\t%g\t%g\n", partname[i],  hwp[0][i]->Integral()*1.6e-10, hwp[1][i]->Integral()*1.6e-10 );
    }
    printf("%s\t%g\t%g\n", "Total",  wsum1, wsum2 );


    //////////////////////////////////////////////////////////////////////////
    gStyle->SetTitleSize(0.16);

    gStyle->SetTitleXSize(0.08);
    gStyle->SetTitleXOffset(0.3);

    gStyle->SetLabelSize(0.05, "x");
    gStyle->SetLabelOffset(0.3, "x");

    gStyle->SetTitleYSize(0.08);
    gStyle->SetTitleYOffset(0.3);

    gStyle->SetLabelSize(0.05, "y");
    gStyle->SetLabelOffset(0.3, "y");

    gStyle->SetTitleFontSize(0.1);

    TCanvas *c = new TCanvas("c0", "Upstream", 700, 900);
    c->Divide(2, npart);

    for( idx = 0; idx < npart; idx++ ){
	c->cd(1+2*idx);
	gPad->SetLogy();
	hp[0][idx]->Draw();

	c->cd(2+2*idx);
	gPad->SetLogy();
	hvz[0][idx]->Draw();
    }
    c->Print("20140312/upstream.png");
    c->Print("20140312/upstream.pdf");

    TCanvas *c2 = new TCanvas("c1", "Downstream", 700, 900);
    c2->Divide(2, npart);

    for( idx = 0; idx < npart; idx++ ){
	c2->cd(1+2*idx);
	gPad->SetLogy();
	hp[1][idx]->Draw();

	c2->cd(2+2*idx);
	gPad->SetLogy();
	hvz[1][idx]->Draw();
    }
    c2->Print("20140312/downstream.png");
    c2->Print("20140312/downstream.pdf");

    /////////////////////////////////////////////////////////
    // Doses

    TCanvas *cdose1 = new TCanvas("cdose1", "Dose", 900, 500);
    cdose1->Divide(2, 2);

    for( idx = 0; idx < 2; idx++ ){
	cdose1->cd(1+2*idx);
	gPad->SetLogy();
	hEdep[idx]->Draw();

	cdose1->cd(2+2*idx);
	htotaldose[idx]->Draw("COLZ");
    }
    cdose1->Print("20140312/totaldose.png");
    cdose1->Print("20140312/totaldose.pdf");

    TCanvas *cdose2 = new TCanvas("cdose2", "Upstream Dose", 700, 900);

    cdose2->Divide(2,npart);

    for( idx = 0; idx < npart; idx++ ){
	cdose2->cd(1+2*idx);
	gPad->SetLogy();
	hpEdep[0][idx]->Draw();

	cdose2->cd(2+2*idx);
	hdose[0][idx]->Draw("COLZ");
    }
    cdose2->Print("20140312/up_dose.png");
    cdose2->Print("20140312/up_dose.pdf");

    TCanvas *cdose3 = new TCanvas("cdose3", "Downstream Dose", 700, 900);
    cdose3->Divide(2,npart);

    for( idx = 0; idx < npart; idx++ ){
	cdose3->cd(1+2*idx);
	gPad->SetLogy();
	hpEdep[1][idx]->Draw();

	cdose3->cd(2+2*idx);
	hdose[1][idx]->Draw("COLZ");
    }
    cdose3->Print("20140312/dn_dose.png");
    cdose3->Print("20140312/dn_dose.pdf");

}
