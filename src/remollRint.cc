#include "remollRint.hh"

// Standard C and C++ headers
#include <iostream>
#include <cstdlib>

// ROOT headers
#include <TROOT.h>

// Global pointers
remollRint* gRemollRint = NULL;

// Pointer to self
remollRint* remollRint::fExists = NULL;


//--------------------------------------------------------------------------
remollRint::remollRint (const char* appClassName, int* argc, char** argv,
                        void* options, int numOptions, bool noLogo)
: TRint (appClassName, argc, argv, options, numOptions, noLogo)
{
  gRemollRint = this;

  // re-root command prompt
  SetPrompt("re-root [%d] ");

  // Pointer to self
  fExists = this;
}

//---------------------------------------------------------------------------
remollRint::~remollRint()
{
  // Reset point to self
  if (fExists == this)
    fExists = NULL;
}

