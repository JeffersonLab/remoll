void rootlogon(){
    FileStat_t buf;
    if( !gSystem->GetPathInfo("libremollroot.so", buf) ){
	gSystem->Load("libremollroot.so" ) ;
    }
    if( !gSystem->GetPathInfo("libremollroot.dylib", buf) ){
	gSystem->Load("libremollroot.dylib" ) ;
    }
}
