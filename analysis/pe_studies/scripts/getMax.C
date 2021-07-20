void getMax(TString fName = "scans.root",Double_t baseline = 0.0) {
    TFile* fFile = TFile::Open(fName);
    if (!fFile) {
        std::cout<< "ERROR: No file named " << fName << std::endl;
        return;
    }   
    TChain* scans = (TChain*) fFile->Get("scans");
    TCanvas * c1 = new TCanvas();

    Double_t old_integral = baseline;
    Double_t new_integral = baseline;

    //(5 tiles of x width, 10 tiles of y length, and fill with the Theta chosen above)
    TH2F * improvementHistOld = new TH2F ("ImprovementOld","Original Yield", 5, 0, 5, 10, 0 , 10);
    TH2F * improvementHist = new TH2F ("Improvement","Improvement", 5, 0, 5, 10, 0 , 10);
    TH2F * oldHist_Theta = new TH2F ("OldThetas","Original Thetas", 5, 0, 5, 10, 0 , 10);
    TH2F * oldHist_Phi = new TH2F ("OldPhis","Original Phis", 5, 0, 5, 10, 0 , 10);
    TH2F * newHist_Theta = new TH2F ("OptimalThetas","Optimal Thetas", 5, 0, 5, 10, 0 , 10);
    TH2F * newHist_Phi = new TH2F ("OptimalPhis","Optimal Phis", 5, 0, 5, 10, 0 , 10);

    c1->cd(1);
    for (Int_t i = 0 ; i < 5; i++ ) { 
        for (Int_t j = 0 ; j < 10; j++ ) { 
            std::cout<< "Step " << i << " " << j << std::endl;
            scans->Draw("theta:phi",Form("avg_pes*(width_step==%d && length_step==%d)",i,j),"COLZ");

            //c1->SaveAs("test.pdf");
            TH2* hTemp = (TH2*) gROOT->FindObject("htemp");
            Int_t max_bin = hTemp->GetMaximumBin();
            Double_t max_value = hTemp->GetBinContent(max_bin);
            new_integral += max_value - baseline;
            Int_t xBin, yBin, zBin;
            hTemp->GetBinXYZ(max_bin,xBin,yBin,zBin);
            std::cout<< "Max bin = " << max_bin << ", and value = " << hTemp->GetBinContent(max_bin) << std::endl;
            std::cout<< "Phi max = " << hTemp->GetXaxis()->GetBinCenter(xBin) << std::endl;
            std::cout<< "Theta max = " << hTemp->GetYaxis()->GetBinCenter(yBin) << std::endl;
            newHist_Theta->Fill(i,j,hTemp->GetYaxis()->GetBinCenter(yBin));
            newHist_Phi->Fill(i,j,hTemp->GetXaxis()->GetBinCenter(xBin));

            scans->Draw("theta:phi",Form("avg_pes*(phi==0 && theta==0 && width_step==%d && length_step==%d)",i,j),"COLZ");

            //c1->SaveAs("test.pdf");
            hTemp = (TH2*) gROOT->FindObject("htemp");
            if (!hTemp) {continue;}
            Int_t old_max_bin = hTemp->GetMaximumBin();
            Double_t old_max_value = hTemp->GetBinContent(old_max_bin);
            old_integral += old_max_value - baseline;
            Int_t old_xBin, old_yBin, old_zBin;
            hTemp->GetBinXYZ(old_max_bin,old_xBin,old_yBin,old_zBin);
            std::cout<< "Old Max bin = " << old_max_bin << ", and value = " << hTemp->GetBinContent(old_max_bin) << std::endl;
            std::cout<< "Old Phi max = " << hTemp->GetXaxis()->GetBinCenter(old_xBin) << std::endl;
            std::cout<< "Old Theta max = " << hTemp->GetYaxis()->GetBinCenter(old_yBin) << std::endl;

            std::cout<< "Improvement factor = " << max_value/old_max_value << std::endl;
            improvementHistOld->Fill(i,j,old_max_value);
            improvementHist->Fill(i,j,max_value/old_max_value);
            oldHist_Theta->Fill(i,j,hTemp->GetYaxis()->GetBinCenter(old_yBin));
            oldHist_Phi->Fill(i,j,hTemp->GetXaxis()->GetBinCenter(old_xBin));
            max_value = 0.0;
            old_max_value = 0.0;
        }
    }
    std::cout<< "\n";
    std::cout<< "\n";
    std::cout<< "Old integral = " << 2*old_integral << ", and new integral = " << 2*new_integral << std::endl;
    std::cout<< "Improvement factor = " << new_integral/old_integral << std::endl;

    TCanvas * c2 = new TCanvas();
    c2->Divide(2,2);
    c2->cd(1);
    improvementHistOld->Draw("COLZ");
    c2->cd(2);
    improvementHist->Draw("COLZ");
    c2->cd(3);
    newHist_Theta->Draw("COLZ");
    c2->cd(4);
    newHist_Phi->Draw("COLZ");
    int dotPos = ((std::string)fName).rfind(".root");
    std::ostringstream os;
    os << ((std::string)fName).substr(0, dotPos) << ".pdf";
    std::string fileName = os.str();
    c2->SaveAs(fileName.c_str());
}

