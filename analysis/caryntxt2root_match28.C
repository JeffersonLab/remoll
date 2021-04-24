void caryntxt2root_match28(){

  //  int detnum=74;
  //  TString infilename=Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_100M/remoll_electrons_updateftsd_det%d.txt",detnum);

  TString infilename1=Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_hybridusfields_100M/remoll_electrons_updateftsd_hybridusfields_match28_det%d.txt",70);
  TString infilename2=Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_hybridusfields_100M/remoll_electrons_updateftsd_hybridusfields_match28_det%d.txt",71);
  TString infilename3=Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_hybridusfields_100M/remoll_electrons_updateftsd_hybridusfields_match28_det%d.txt",72);
  TString infilename4=Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_hybridusfields_100M/remoll_electrons_updateftsd_hybridusfields_match28_det%d.txt",73);
  TString infilename5=Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_hybridusfields_100M/remoll_electrons_updateftsd_hybridusfields_match28_det%d.txt",74);
  TString infilename6=Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_hybridusfields_100M/remoll_electrons_updateftsd_hybridusfields_match28_det%d.txt",75);
  TString infilename7=Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_hybridusfields_100M/remoll_electrons_updateftsd_hybridusfields_match28_det%d.txt",76);


  ifstream infile1;
  infile1.open(infilename1);
  cout<<infilename1<<endl;



  // TString outfilename = Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_100M/remoll_electrons_updateftsd_det%d.txt",detnum);
 TString outfilename = Form("/volatile/halla/moller12gev/palatchi/remoll_bellows_beam_updateftsd_hybridusfields_100M/count_uspdatefttsd_hybridusfields_match28.root");
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
    double irun, idet, ientry, itrack, ix, iy, iz, ipx, ipy, ipz, it, it28, ir28, ie28;;
    double run, det, entry, track, x, y, z, px, py, pz, p, e, t, rmain, emain, tmain;

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
    c->Branch("rmain",&rmain,"rmain/D");
    c->Branch("emain",&emain,"emain/D");
    c->Branch("tmain",&tmain,"tmain/D");

  int icnt=0;
  while(1) {
    infile1 >> irun >> idet >> ientry >> itrack >> ix >> iy >> iz >> ipx >> ipy >> ipz >> it >> it28 >> ir28 >> ie28;
    //    cout<<irun<<" "<<idet<<" "<<ientry<<" "<<itrack<<" "<<ix<<" "<<iy<<" "<<iz<<" "<<ipx<<" "<<ipy<<" "<<ipz<<" "<<ip<<" "<<ie<<" "<<irmain<<endl;
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
    p=sqrt(pow(ipx,2)+pow(ipy,2)+pow(ipz,2));
    e=sqrt(pow(p,2)+pow(0.511,2));
    rmain=ir28;
    emain=ie28;
    tmain=it28;
    c->Fill();
      icnt++;
    if(!infile1.good()) break;
  }

  cout<<"Ne: "<<icnt-1<<endl;

  //file2
    ifstream infile2;
    infile2.open(infilename2);
    cout<<infilename2<<endl;

  icnt=0;
  while(1) {
    infile2 >> irun >> idet >> ientry >> itrack >> ix >> iy >> iz >> ipx >> ipy >> ipz >> it >> it28 >> ir28 >> ie28;
    //    infile2 >> irun >> idet >> ientry >> itrack >> ix >> iy >> iz >> ipx >> ipy >> ipz >> ip >> ie >> irmain;
    //    cout<<irun<<" "<<idet<<" "<<ientry<<" "<<itrack<<" "<<ix<<" "<<iy<<" "<<iz<<" "<<ipx<<" "<<ipy<<" "<<ipz<<" "<<ip<<" "<<ie<<" "<<irmain<<endl;
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
    p=sqrt(pow(ipx,2)+pow(ipy,2)+pow(ipz,2));
    e=sqrt(pow(p,2)+pow(0.511,2));
    rmain=ir28;
    emain=ie28;
    tmain=it28;
    c->Fill();
      icnt++;
    if(!infile2.good()) break;
  }

  cout<<"Ne: "<<icnt-1<<endl;

  //file3
    ifstream infile3;
    infile3.open(infilename3);
    cout<<infilename3<<endl;

  icnt=0;
  while(1) {
    infile3 >> irun >> idet >> ientry >> itrack >> ix >> iy >> iz >> ipx >> ipy >> ipz >> it >> it28 >> ir28 >> ie28;
    //    infile3 >> irun >> idet >> ientry >> itrack >> ix >> iy >> iz >> ipx >> ipy >> ipz >> ip >> ie >> irmain;
    //    cout<<irun<<" "<<idet<<" "<<ientry<<" "<<itrack<<" "<<ix<<" "<<iy<<" "<<iz<<" "<<ipx<<" "<<ipy<<" "<<ipz<<" "<<ip<<" "<<ie<<" "<<irmain<<endl;
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
    p=sqrt(pow(ipx,2)+pow(ipy,2)+pow(ipz,2));
    e=sqrt(pow(p,2)+pow(0.511,2));
    rmain=ir28;
    emain=ie28;
    tmain=it28;
    c->Fill();
      icnt++;
    if(!infile3.good()) break;
  }

  cout<<"Ne: "<<icnt-1<<endl;

  //file4
    ifstream infile4;
    infile4.open(infilename4);
    cout<<infilename4<<endl;

  icnt=0;
  while(1) {
    infile4 >> irun >> idet >> ientry >> itrack >> ix >> iy >> iz >> ipx >> ipy >> ipz >> it >> it28 >> ir28 >> ie28;
    //    infile4 >> irun >> idet >> ientry >> itrack >> ix >> iy >> iz >> ipx >> ipy >> ipz >> ip >> ie >> irmain;
    //    cout<<irun<<" "<<idet<<" "<<ientry<<" "<<itrack<<" "<<ix<<" "<<iy<<" "<<iz<<" "<<ipx<<" "<<ipy<<" "<<ipz<<" "<<ip<<" "<<ie<<" "<<irmain<<endl;
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
    p=sqrt(pow(ipx,2)+pow(ipy,2)+pow(ipz,2));
    e=sqrt(pow(p,2)+pow(0.511,2));
    rmain=ir28;
    emain=ie28;
    tmain=it28;
    c->Fill();
      icnt++;
    if(!infile4.good()) break;
  }

  cout<<"Ne: "<<icnt-1<<endl;

  //file5
    ifstream infile5;
    infile5.open(infilename5);
    cout<<infilename5<<endl;

  icnt=0;
  while(1) {
    infile5 >> irun >> idet >> ientry >> itrack >> ix >> iy >> iz >> ipx >> ipy >> ipz >> it >> it28 >> ir28 >> ie28;
    //    infile5 >> irun >> idet >> ientry >> itrack >> ix >> iy >> iz >> ipx >> ipy >> ipz >> ip >> ie >> irmain;
    //    cout<<irun<<" "<<idet<<" "<<ientry<<" "<<itrack<<" "<<ix<<" "<<iy<<" "<<iz<<" "<<ipx<<" "<<ipy<<" "<<ipz<<" "<<ip<<" "<<ie<<" "<<irmain<<endl;
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
    p=sqrt(pow(ipx,2)+pow(ipy,2)+pow(ipz,2));
    e=sqrt(pow(p,2)+pow(0.511,2));
    rmain=ir28;
    emain=ie28;
    tmain=it28;
    c->Fill();
      icnt++;
    if(!infile5.good()) break;
  }

  cout<<"Ne: "<<icnt-1<<endl;

  //file6
    ifstream infile6;
    infile6.open(infilename6);
    cout<<infilename6<<endl;

  icnt=0;
  while(1) {
    infile6 >> irun >> idet >> ientry >> itrack >> ix >> iy >> iz >> ipx >> ipy >> ipz >> it >> it28 >> ir28 >> ie28;
    //    infile6 >> irun >> idet >> ientry >> itrack >> ix >> iy >> iz >> ipx >> ipy >> ipz >> ip >> ie >> irmain;
    //    cout<<irun<<" "<<idet<<" "<<ientry<<" "<<itrack<<" "<<ix<<" "<<iy<<" "<<iz<<" "<<ipx<<" "<<ipy<<" "<<ipz<<" "<<ip<<" "<<ie<<" "<<irmain<<endl;
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
    p=sqrt(pow(ipx,2)+pow(ipy,2)+pow(ipz,2));
    e=sqrt(pow(p,2)+pow(0.511,2));
    rmain=ir28;
    emain=ie28;
    tmain=it28;
    c->Fill();
      icnt++;
    if(!infile6.good()) break;
  }

  cout<<"Ne: "<<icnt-1<<endl;

  //file7
    ifstream infile7;
    infile7.open(infilename7);
    cout<<infilename7<<endl;

  icnt=0;
  while(1) {
    infile7 >> irun >> idet >> ientry >> itrack >> ix >> iy >> iz >> ipx >> ipy >> ipz >> it >> it28 >> ir28 >> ie28;
    //    infile7 >> irun >> idet >> ientry >> itrack >> ix >> iy >> iz >> ipx >> ipy >> ipz >> ip >> ie >> irmain;
    //    cout<<irun<<" "<<idet<<" "<<ientry<<" "<<itrack<<" "<<ix<<" "<<iy<<" "<<iz<<" "<<ipx<<" "<<ipy<<" "<<ipz<<" "<<ip<<" "<<ie<<" "<<irmain<<endl;
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
    p=sqrt(pow(ipx,2)+pow(ipy,2)+pow(ipz,2));
    e=sqrt(pow(p,2)+pow(0.511,2));
    rmain=ir28;
    emain=ie28;
    tmain=it28;
    c->Fill();
      icnt++;
    if(!infile7.good()) break;
  }

  cout<<"Ne: "<<icnt-1<<endl;


  c->Write();
  myfile.Write();

}
