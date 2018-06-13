#include "TF1.h"
#include<iostream>
#include<fstream>

    //takes input of remoll output files using moller, elastic, and inelastic generators
    //Pulls hit radius data from the root files and puts it into histograms, and then fits 
    //those histograms with parameterized functions that are output into remollGenFunctions.root
TH1F* getHist(char* fname){
    cout << "Opening file " << fname << endl;
    TFile *file = TFile::Open(fname);
    if (!file)
        cout << "File not found" << endl;
    TTree *T = file->Get("T");
    if (!T)
        cout << "TTree T not found" << endl;
    TH1F *rad = new TH1F[4]; 

    rad[0] = new TH1F("rad_m", "hit_r total",100,0.6,1.2);
    rad[1] = new TH1F("s1_m","sector 1",100,0.6,1.2);
    rad[2] = new TH1F("s2_m","sector 2",100,0.6,1.2);
    rad[3] = new TH1F("s3_m","sector 3",100,0.6,1.2);

    double hit_x[10000000]={0};
    double hit_y[10000000]={0};
    int hit_det[10000000]={0};
    double hit_r[10000000]={0};
    int hit_n;
    double rate;

    T->SetBranchAddress("hit.n",&hit_n);
    T->SetBranchAddress("hit.x",&hit_x);
    T->SetBranchAddress("hit.y",&hit_y);
    T->SetBranchAddress("hit.det",&hit_det);
    T->SetBranchAddress("hit.r",&hit_r);
    T->SetBranchAddress("rate",&rate);

    long nEntries = T->GetEntries();
    for (Int_t i = 0; i < nEntries; i++){
        T->GetEntry(i);
        for (Int_t j = 0; j < hit_n; j++){
            if (hit_det[j] == 28 && hit_r[j] > 0.6){
                 rad[0].Fill(hit_r[j],rate);
                 //determine sector number (conditions provided by Seamus)
                 double secphi;
                 int secnum;
                 double eigth = 360.0/56.0;
                 if (atan2(hit_y[j],hit_x[j])>0){
                     secphi = fmod(atan2(hit_y[j],hit_x[j]),2.0*3.14159/7.0)*180/3.14159;
                }
                 else{
                     secphi = fmod(atan2(hit_y[j],hit_x[j])+2.0*3.14159,2.0*3.14159/7.0)*180/3.14159;
                 }

                 if(secphi < eigth || secphi > 7*eigth)
                     secnum = 1;
                 if ((eigth < secphi && secphi < 3.0*eigth) || (5.0*eigth < secphi && secphi < 7.0*eigth))
                     secnum = 2;
                 if (3.0*eigth < secphi && secphi < 5.0*eigth)
                     secnum = 3;
             
                 rad[secnum].Fill(hit_r[j],rate);
            }
        }
    }
    return rad;
}

TF1* fitHist(char* fname, int sec, TH1F rad){
    TF1* fit;
    char *fitname = new char[15];
    cout << "fit func called" << endl;   
    //fit histograms of all sectors
        if ("remollout_moller10M.root"==fname){
            sprintf(fitname,"moller_%d",sec);
            cout << "Fitting moller sector " << sec << endl;
            fit = new TF1(fitname,"gaus",0.6,1.2);
            fit->SetParameters(rad->GetMaximum(),rad->GetMean(),rad->GetStdDev());
            
            rad.Fit(fitname,"QM");
            rad.Fit(fitname,"M");
        
        }

         else if ("remollout_elastic10M.root"==fname){
             cout << "Fitting elastic sector " << sec << endl;
           
            sprintf(fitname,"elastic_%d",sec);
            fit = new TF1(fitname, elasticFit, 0.6,1.2,6);
            
            double p; //adjust peak height
            double m; //adjust center of gaussian
            int y=-1; //adjust height quadratic vertex
            double quad = 0.8e5; //adjust curvature
            double qcenter = 0.975;//vertex of quadratic on x axis
            switch (sec){ //set fit parameters (this was done by hand)
                 case 0: p =rad.GetBinContent(25)/20 - 10e6; m = 0.1855; quad = 0.01e5; y = -0.9*rad.GetBinContent(65);  break;
                 case 1: p = rad.GetBinContent(25)/20.0 - 1e5 ; m = 0.1875; quad = 0.5e6; y = -0.5*rad.GetBinContent(65); qcenter = 0.83;  break;
                 case 2: p = rad.GetBinContent(25)/20.0 + 5.5e6; m = 0.175; quad = 0.8e6; qcenter = 0.92; y = -1*rad.GetBinContent(65);  break;
                 default: p = rad.GetBinContent(25)/20.0 -10e6; m = 0.185; y = -0.9*rad.GetBinContent(65);
            }

            fit->SetParameter(4,y);
            fit->SetParameter(0,p);
            fit->SetParameter(1,rad.GetMean()-m);
            fit->SetParameter(2,rad.GetStdDev()/10);
            fit->SetParameter(3,quad);
            fit->SetParameter(5,qcenter);

            rad.Fit(fitname,"QMRPEW");
            fit->SetParameters(fit->GetParameters());
            rad.Fit(fitname,"MRPEW");
         }
    
        else if ("remollout_inelastic10M.root"==fname){
            cout << "Fitting inelastic sector " << endl;
           
            sprintf(fitname,"inelastic_%d",sec);
            fit = new TF1(fitname,inelasticFit,0.6,1.2,6);
            
            // parameters fixed by hand (not great)
            double exp = 5.0;
            double center = 0.75;
            double h = 1000000;
            double mean = 0.773;
            double peak = rad.GetMaximum()*12;
            double sig = rad.GetStdDev()/3.5;
            switch (sec){
                case 0: exp = 6.5; h = 5.0e6; break;
                case 1: mean=0.73; exp = 7.0; sig = .02; center = 0.75; h = 350000.0; break;
                case 2: mean = 0.77; peak = peak * 100.0; exp = 3.0; center = 0.75; break;
                default: exp = 6.0; h = 3500000;
            }
            fit->SetParameters(peak,mean,sig,h,exp,center);
            
            rad.Fit(fitname,"QMRP");
            fit->SetParameter(3,1.e6);
            fit->SetParameter(4,8.0);
            if (sec == 1){
                fit->FixParameter(1,0.748);
                fit->FixParameter(2,1e-2);
                fit->FixParameter(0,8.5e4);
            }
            rad.Fit(fitname,"MRP");
        }

    return fit;
}

Double_t elasticFit(Double_t *x, Double_t *par){
    Double_t g = 0.0;
    Double_t q = 0.0;

    Double_t arg = (fabs(par[2])>1e-6)? (x[0] - (par[1]))/par[2] : 0.0;
    g = (x[0]<0.8)? fabs(par[0])*exp(-0.5*arg*arg)/(par[2]*sqrt(2.0*TMath::Pi())):0.0;
    q = (x[0]>0.73)? (TMath::Abs(par[3])*(x[0]-par[5])*(x[0]-par[5])-par[4]):0.0;
    if (g < q && x > 0.8){
        return q;
    }else{
        return g;
    }
}

Double_t inelasticFit(Double_t *x, Double_t *par){
    Double_t g = 0.0;
    Double_t e = 0.0;

    Double_t arg = (fabs(par[2])>1e-6)? (x[0] - (par[1]))/par[2] : 0.0;
    g = (x[0]<0.8)?par[0]*exp(-0.5*arg*arg)/(par[2]*sqrt(2.0*TMath::Pi())):0.0;
    if (x[0] > 0.76)
        e = par[3]/TMath::Power(x[0],par[4]) + par[5];
    if (g > e){
        return g;
    }
    else{
        return e;
    }
}


void remollDistAnalysis(){
    //takes input of remoll output files using moller, elastic, and inelastic generators
    //Pulls hit radius data from the root files and puts it into histograms, and then fits 
    //those histograms with parameterized functions that are output into remollGenFunctions.root

   
    TH1F* rad_m = getHist("remollout_moller10M.root");
    TH1F* rad_e = getHist("remollout_elastic10M.root");
    TH1F* rad_i = getHist("remollout_inelastic10M.root");

    TFile *output = new TFile("remollGenFunctions.root","RECREATE");
    
    TF1 *elastic = fitHist("remollout_elastic10M.root",0,rad_e[0]);
    TF1 *elastic_1 = fitHist("remollout_elastic10M.root",1,rad_e[1]);
    TF1 *elastic_2 = fitHist("remollout_elastic10M.root",2,rad_e[2]);
    TF1 *elastic_3 = fitHist("remollout_elastic10M.root",3,rad_e[3]);
    
    TF1 *moller = fitHist("remollout_moller10M.root",0,rad_m[0]);
    TF1 *moller_1 = fitHist("remollout_moller10M.root",1,rad_m[1]);
    TF1 *moller_2 = fitHist("remollout_moller10M.root",2,rad_m[2]);
    TF1 *moller_3 = fitHist("remollout_moller10M.root",3,rad_m[3]);
    
    TF1 *inelastic = fitHist("remollout_inelastic10M.root",0,rad_i[0]);
    TF1 *inelastic_1 = fitHist("remollout_inelastic10M.root",1,rad_i[1]);
    TF1 *inelastic_2 = fitHist("remollout_inelastic10M.root",2,rad_i[2]);
    TF1 *inelastic_3 = fitHist("remollout_inelastic10M.root",3,rad_i[3]);
    
    elastic->Write();
    elastic_1->Write();
    elastic_2->Write();
    elastic_3->Write();

    moller->Write();
    moller_1->Write();
    moller_2->Write();
    moller_3->Write();

    inelastic->Write();
    inelastic_1->Write();
    inelastic_2->Write();
    inelastic_3->Write();

    output->Close();
}
