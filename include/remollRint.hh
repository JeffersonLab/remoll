#ifndef __remollRint_h__
#define __remollRint_h__

// ROOT headers
#include <TRint.h>
#include <TThread.h>

// Global pointers recognised by CINT
R__EXTERN class remollRint* gRemollRint;


/**
 * The remollRint class implements a ROOT command line interface.  It inherits
 * from the ROOT interactive C++ interpreter TRint.  All functionality available
 * in ROOT is available in this interface.  In particular, all histograms that
 * are defined in memory or in a ROOT file are accessible under the path
 * "/root/ROOT memory", while they are being filled.  All remoll-specific class
 * structures are available in an interactive mode.
 *
 */
class remollRint : public TRint {

  protected:

    static remollRint* fExists;       ///< Check whether interface already existing

  public:
    /// \brief Constructor
    remollRint (const char* appClassName, int* argc, char** argv,
                void* options = 0, int numOptions = 0, bool noLogo = kFALSE);

    /// \brief Destructor
    virtual ~remollRint();

}; // class remollRint

#endif // __remollRint_h__

