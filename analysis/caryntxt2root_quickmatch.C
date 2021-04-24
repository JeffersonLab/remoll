void caryntxt2root_quickmatch(){


  TString infilename1=Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_hybridusfields_100M/remoll_electrons_updateftsd_hybridusfields_quickmatch_det%d.txt",0);


  ifstream infile1;
  infile1.open(infilename1);
  cout<<infilename1<<endl;



  // TString outfilename = Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_100M/remoll_electrons_updateftsd_det%d.txt",detnum);
 TString outfilename = Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_hybridusfields_100M/count_uspdatefttsd_hybridusfields_quickmatch.root");
 cout<<"writing output to "<<outfilename.Data()<<endl;
 //  ofstream outfile;
 //  outfile.open(outfilename);

 // TString filename="/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_100M/remoll_bellows_beam_updatefttsd_100M_100kEv";
 //  TFile *_file0;
  //    _file0 = TFile::Open(Form("%s_00%d/remollout.root",filename.Data(),runnum));
    //    cout<<Form("%s_00%d/remollout.root",filename.Data(),runnum)<<endl;

    TFile myfile(outfilename, "RECREATE");
    TTree *c = new TTree("c","Count Treee");

    //Branch e, x, t, rmain, emain, tmain ...
    double irun, idet, ientry, itrack, ix, iy, iz, ipx, ipy, ipz, it, ie;
    double ix28, iy28, iz28, ipx28, ipy28, ipz28, it28, ie28;
    double ix5547, iy5547, iz5547, ipx5547, ipy5547, ipz5547, it5547, ie5547;

    double run, det, entry, track, x, y, z, px, py, pz, p, e, t, r;
    double xmain, ymain, zmain, pxmain, pymain, pzmain, pmain, emain, tmain, rmain;
    double x5547, y5547, z5547, px5547, py5547, pz5547, p5547, e5547, t5547, r5547;

    c->Branch("run",&run,"run/D");
    c->Branch("det",&det,"det/D");
    c->Branch("entry",&entry,"entry/D");
    c->Branch("track",&track,"track/D");
    c->Branch("x",&x,"x/D");
    c->Branch("y",&y,"y/D");
    c->Branch("z",&z,"z/D");
    c->Branch("px",&px,"px/D");
    c->Branch("py",&py,"py/D");
    c->Branch("pz",&pz,"pz/D");
    c->Branch("p",&p,"p/D");
    c->Branch("e",&e,"e/D");
    c->Branch("t",&t,"t/D");
    c->Branch("r",&r,"r/D");

    c->Branch("xmain",&xmain,"xmain/D");
    c->Branch("ymain",&ymain,"ymain/D");
    c->Branch("zmain",&zmain,"zmain/D");
    c->Branch("pxmain",&pxmain,"pxmain/D");
    c->Branch("pymain",&pymain,"pymain/D");
    c->Branch("pzmain",&pzmain,"pzmain/D");
    c->Branch("pmain",&pmain,"pmain/D");
    c->Branch("emain",&emain,"emain/D");
    c->Branch("tmain",&tmain,"tmain/D");
    c->Branch("rmain",&rmain,"rmain/D");

    c->Branch("x5547",&x5547,"x5547/D");
    c->Branch("y5547",&y5547,"y5547/D");
    c->Branch("z5547",&z5547,"z5547/D");
    c->Branch("px5547",&px5547,"px5547/D");
    c->Branch("py5547",&py5547,"py5547/D");
    c->Branch("pz5547",&pz5547,"pz5547/D");
    c->Branch("p5547",&p5547,"p5547/D");
    c->Branch("e5547",&e5547,"e5547/D");
    c->Branch("t5547",&t5547,"t5547/D");
    c->Branch("r5547",&r5547,"r5547/D");


  int icnt=0;
  while(1) {
    infile1 >> irun >> idet >> ientry >> itrack >> ix >> iy >> iz >> ipx >> ipy >> ipz >> ie >> it >> 
      ix28 >> iy28 >> iz28 >> ipx28 >> ipy28 >> ipz28 >> ie28 >> it28 >> 
      ix5547 >> iy5547 >> iz5547 >> ipx5547 >> ipy5547 >> ipz5547 >> ie5547 >> it5547;
    run=irun;
    det=idet;
    entry=ientry;
    track=itrack;
    x=ix;
    y=iy;
    z=iz;
    px=ipx;
    py=ipy;
    pz=ipz;
    t = it;
    e=ie;//sqrt(pow(p,2)+pow(0.511,2));
    p=sqrt(pow(ipx,2)+pow(ipy,2)+pow(ipz,2));
    r=sqrt(pow(ix,2)+pow(iy,2));

    xmain=ix28;
    ymain=iy28;
    zmain=iz28;
    pxmain=ipx28;
    pymain=ipy28;
    pzmain=ipz28;
    tmain=it28;
    emain=ie28;
    pmain=sqrt(pow(ipx28,2)+pow(ipy28,2)+pow(ipz28,2));
    rmain=sqrt(pow(ix28,2)+pow(iy28,2));

    x5547=ix5547;
    y5547=iy5547;
    z5547=iz5547;
    px5547=ipx5547;
    py5547=ipy5547;
    pz5547=ipz5547;
    t5547=it5547;
    e5547=ie5547;
    p5547=sqrt(pow(ipx5547,2)+pow(ipy5547,2)+pow(ipz5547,2));
    r5547=sqrt(pow(ix5547,2)+pow(iy5547,2));

    c->Fill();
      icnt++;
    if(!infile1.good()) break;
  }

  cout<<"Ne: "<<icnt-1<<endl;


  c->Write();
  myfile.Write();

}
