hyperon_particles_plot (const Char_t * kin)
{
// plot the hyperon decay particles momentum and position distributions
/*
x,y,z, position of particle a, r = sqrt(x^2+y^2)
px,py,pz,prad,pmag momenta of particle a, prad=sqrt(px^2+py^2)
weight = weight per elecron for event
strtod = a C++ function converts string to double, uses stdlib.h, for nan problem
nansum = sum of cases where kinematics was impossible
notna = sum of cases where valid kinematics exist in input file
*/

  Int_t i, j, Nevents = 0, ncnts = 10000;
  Double_t x, y, z, r, px, py, pz, prad, pmag, weight, pKmax, nvertex;
/*
Char_t str1[40], str2[40], str3[40], str4[40], str5[40],str6[40],str7[40];
Char_t str8[40],str9[40],str10[40],str11[40];
TString nan = ("nan");
Char_t* pEnd;
*/
  Char_t bl = ' ', eol = '\n';

  TH1F *Z =
    new TH1F ("Z", "Distibution of pions along Z,h=-1", 350, -150, 200);
  TH1F *Rad =
    new TH1F ("Rad", "Distribution of pions radially, h=-1", 55, -5, 50);
  TH1F *Px = new TH1F ("Px", "Px distribution of pions,h=-1", 400, -2., 2.);
  TH1F *Py = new TH1F ("Py", "Py distribution of pions,h=-1", 400, -2., 2.);
  TH1F *Pz = new TH1F ("Pz", "Pz distribution of pions,h=-1", 210, -.1, 2.0);
  TH1F *Prad =
    new TH1F ("Prad", "Prad distribution of pions,h=-1", 200, 0, 2.);
  TH1F *Pmag =
    new TH1F ("Pmag", "Pmag distribution of pions,h=-1", 200, 0, 2.0);
  TH2F *h2 =
    new TH2F ("h2", "Zpos vs Pmag for pions,h=-1", 350, -150, 200, 200, 0,
	      2.);
  TH2F *h3 =
    new TH2F ("h3", "Radial_pos vs Pmag for pions,h=-1", 55, -5, 50, 200, .0,
	      2.);
  TH2F *h4 =
    new TH2F ("h4", "radial position vs Z for pions,h=-1", 55, -5, 50, 350,
	      -150, 200);

  ifstream hyperons ("../hyperons/hyperon_particle_decays.dat");

// if first two lines contains strings read through them
/*
hyperons>>str1>>str2>>str3>>str4>>str5>>str6>>str7>>str8>>str9>>str10;
std::cout<<str1<<bl<<str2<<bl<<str3<<bl<<str4<<bl<<str5<<bl<<str6<<bl<<str7<<bl<<str8<<bl<<str9<<bl<<str10<<eol;
std::cout<<str1<<eol;
std::cout<<str2<<eol;
std::cout<<str3<<eol;
std::cout<<str4<<eol;
std::cout<<str5<<eol;
std::cout<<str6<<eol;
std::cout<<str7<<eol;
std::cout<<str8<<eol;
std::cout<<str9<<eol;
std::cout<<str10<<eol;
std::cout<<"****************"<<eol;
nvertex=strtod(str10,&pEnd);
Nvertex = nvertex;

hyperons>>str1>>str2>>str3>>str4>>str5>>str6>>str7>>str8>>str9;
std::cout<<str1<<bl<<str2<<bl<<str3<<bl<<str4<<bl<<str5<<bl<<str6<<bl<<str7<<bl<<str8<<bl<<str9<<eol;
std::cout<<str1<<eol;
std::cout<<str2<<eol;
std::cout<<str3<<eol;
std::cout<<str4<<eol;
std::cout<<str5<<eol;
std::cout<<str6<<eol;
std::cout<<str7<<eol;
std::cout<<str8<<eol;
std::cout<<str9<<eol;
std::cout<<str10<<eol;
std::cout<<str11<<eol;
*/


  while (!hyperons.eof ())
    {
      hyperons >> x >> y >> z >> px >> py >> pz >> weight;
      Nevents = Nevents + 1;
      r = sqrt (x * x + y * y);
      prad = sqrt (px * px + py * py);
      pmag = sqrt (px * px + py * py + pz * pz);
      Z->Fill (z, weight);
      Rad->Fill (r, weight);
      Px->Fill (px, weight);
      Py->Fill (py, weight);
      Pz->Fill (pz, weight);
      Prad->Fill (prad, weight);
      Pmag->Fill (pmag, weight);
      h2->Fill (z, pmag, weight);
      h3->Fill (r, pmag, weight);
      h4->Fill (r, z, weight);
      if ((Nevents / ncnts) * ncnts == Nevents)
	std::cout << "Nevents " << Nevents << eol;
    }

  std::cout << "Nevents = " << Nevents << eol;
  TCanvas *c1 =
    new TCanvas ("c1", "decay particle Z position", 40, 40, 1000, 1000);
  c1->Divide (1);
  c1->cd (1);
  Z->Draw ();

  TCanvas *c2 =
    new TCanvas ("c2", " decay particle Rad position", 40, 40, 1000, 1000);
  c2->Divide (1);
  c2->cd (1);
  Rad->Draw ();

  TCanvas *c3 =
    new TCanvas ("c3", "Px of decay particle", 40, 40, 1000, 1000);
  c3->Divide (1);
  c3->cd (1);
  Px->Draw ();

  TCanvas *c4 =
    new TCanvas ("c4", "Py of decay particle", 40, 40, 1000, 1000);
  c4->Divide (1);
  c4->cd (1);
  Py->Draw ();

  TCanvas *c5 =
    new TCanvas ("c5", "Pz of decay particle", 40, 40, 1000, 1000);
  c5->Divide (1);
  c5->cd (1);
  Pz->Draw ();

  TCanvas *c6 =
    new TCanvas ("c6", "Prad of decay particles", 40, 40, 1000, 1000);
  c6->Divide (1);
  c6->cd (1);
  Prad->Draw ();

  TCanvas *c7 =
    new TCanvas ("c7", "Pmag of decay particles", 40, 40, 1000, 1000);
  c7->Divide (1);
  c7->cd (1);
  Pmag->Draw ();

  TCanvas *c8 =
    new TCanvas ("c8", "Zpos vs Pmag of deca particles", 40, 40, 1000, 1000);
  c8->Divide (1);
  c8->cd (1);
  h2->Draw ();

  TCanvas *c9 =
    new TCanvas ("c9", "Rpos vs Pmag of decay particles", 40, 40, 1000, 1000);
  c9->Divide (1);
  c9->cd (1);
  h3->Draw ();

  TCanvas *c10 =
    new TCanvas ("c10", "Zpos vs Pmag of decay particles", 40, 40, 1000,
		 1000);
  c10->Divide (1);
  c10->cd (1);
  h2->Draw ("SPEC dm(1,10) pa(2,1,1) ci(1,4,8) a(15,65,180)");

  TCanvas *c11 =
    new TCanvas ("c11", "Rpos vs Pmag of decay particles", 40, 40, 1000,
		 1000);
  c11->Divide (1);
  c11->cd (1);
  h3->Draw ("SPEC dm(1,10) pa(2,1,1) ci(1,4,8) a(15,65,180)");

  TCanvas *c12 =
    new TCanvas ("c12", "radial position vs Z of decay particles", 40, 40,
		 1000, 1000);
  c12->Divide (1);
  c12->cd (1);
  h4->Draw ();

  TCanvas *c13 =
    new TCanvas ("c13", "radial position vs Z of decay particles", 40, 40,
		 1000, 1000);
  c13->Divide (1);
  c13->cd (1);
  h4->Draw ("SPEC dm(1,10) pa(2,1,1) ci(1,4,8) a(15,65,180)");
}
