#ifndef remollIO_HH
#define remollIO_HH

#include "TROOT.h"
#include "TObject.h"
#include "G4Run.hh"
#include "remolltypes.hh"

class TFile;
class TTree;

#define MAXHITDATA 100

class remollIO {
    public:
	remollIO();
	~remollIO();

	void SetFilename(const char *fn){strcpy(fFilename, fn);}
	void SetTrackData(tr_t td){ trdata = td; }
	void SetEventData(ev_t ed){ evdata = ed; }
	void SetHitData(hit_t ht){ hitdata = ht; }
	void FillTree();
	void WriteTree();


	ev_t GetEventData(){ return evdata; }

	void InitializeTree();
    private:
	TFile *fFile;
	TTree *fTree;

	ev_t  evdata;
	gen_t gendata;
	tr_t  trdata;
	hit_t hitdata;

	char fFilename[255];

};

#endif//remollIO_HH
