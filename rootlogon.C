void rootlogon(){
    const char *retso = gSystem->FindFile("libremollroot.so");
    const char *retdy = gSystem->FindFile("libremollroot.dylib");

    if( retso ){
	gSystem->Load("libremollroot.so");
    }
    if( retdy ){
	gSystem->Load("libremollroot.dylib");
    }
}
