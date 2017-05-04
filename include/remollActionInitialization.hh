/*
 * remollUserActionInitialization.hh
 *
 *  Created on: May 1, 2017
 *      Author: wdconinc
 */

#ifndef __REMOLLACTIONINITIALIZATION_HH
#define __REMOLLACTIONINITIALIZATION_HH

#include "G4VUserActionInitialization.hh"

class remollMessenger;
class remollIO;

class remollUserActionInitialization : public G4VUserActionInitialization {
  public:
    remollUserActionInitialization();
    virtual ~remollUserActionInitialization();

    void Build() const;
    void BuildForMaster() const;
};

#endif // __REMOLLACTIONINITIALIZATION_HH
