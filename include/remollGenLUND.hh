#ifndef __REMOLLGENLUND_HH 
#define __REMOLLGENLUND_HH 
/*!
 * event generator based on LUND files
 *
 * Rakitha Beminiwattha
 * Fri Apr 15 12:51:25 EDT 2016
 *
 */

// remoll includes
#include "remollVEventGen.hh"

// System includes
#include <map>
#include <fstream>

class remollRunData;

class remollGenLUND : public remollVEventGen {

  public:
    // constructor
    remollGenLUND();

    // virtual destructor
    virtual ~remollGenLUND();

    void SetParticleType(G4String t) { fParticleType = t; }
    void SetLUNDFile(G4String f);

  private:
    void SamplePhysics(remollVertex *, remollEvent *);

    G4String fParticleType;

    remollRunData *fRunData;

    std::ifstream LUNDfile;

    G4bool bLUND;
    std::map<int,G4String> pidname;

};

#endif//__REMOLLGENLUND_HH 
