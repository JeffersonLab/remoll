// ROOT headers
#include <TROOT.h>
#include <TRint.h>
#include <TSystem.h>
#include <TString.h>

#include "remollRint.hh"

int main(int argc, char** argv)
{
  // Start Root command prompt
  remollRint* rint = new remollRint("remoll ROOT Analyzer", &argc, argv);

  // Setup include path
  gROOT->ProcessLine(".include include");
  rint->Run();
  delete rint;
}

