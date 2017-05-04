/*
 * remollUserActionInitialization.hh
 *
 *  Created on: May 1, 2017
 *      Author: wdconinc
 */

#ifndef __REMOLLACTIONINITIALIZATION_HH
#define __REMOLLACTIONINITIALIZATION_HH

#include "G4VUserActionInitialization.hh"

class remollActionInitialization : public G4VUserActionInitialization {
  public:
    remollActionInitialization(): G4VUserActionInitialization() { };
    virtual ~remollActionInitialization() { };

    virtual void Build() const;
    virtual void BuildForMaster() const;
};

#endif // __REMOLLACTIONINITIALIZATION_HH
