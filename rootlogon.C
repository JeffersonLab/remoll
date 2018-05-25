void rootlogon(){
    FileStat_t buf;
    if( !gSystem->GetPathInfo("libremoll.so", buf) ){
	gSystem->Load("libremoll.so" ) ;
    }
    if( !gSystem->GetPathInfo("libremoll.dylib", buf) ){
	gSystem->Load("libremoll.dylib" ) ;
    }
}
