#ifndef __REMOLLTEXTFILE_HH
#define __REMOLLTEXTFILE_HH

#define __STRLEN 1024

#include "TObject.h"

class remollTextFile : public TObject {
    public:
	 remollTextFile();
	 remollTextFile(const remollTextFile &);
	 const remollTextFile& operator=(const remollTextFile &);
	 remollTextFile(const char *);
	~remollTextFile();

	 void copyFileIn(const char *);

	void Print();

	const char *GetFilename(){ return fFilename; }
	unsigned long long int GetBufferSize(){ return fBufferSize; }
	
	void Recreate(const char *fn = NULL, bool clobber = false);
	void RecreateInDir(const char *path, bool clobber = false);

    private:
	int fFilenameSize;
	char *fFilename;

	unsigned long long int fBufferSize;
	char *fBuffer;

	const char *GetBaseFile(const char *fp = NULL);

	ClassDef(remollTextFile, 1);
};

#endif//__REMOLLTEXTFILE_HH
