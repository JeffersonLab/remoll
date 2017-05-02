/*
 * remollUserActionInitialization.hh
 *
 *  Created on: May 1, 2017
 *      Author: wdconinc
 */

#ifndef __REMOLLUSERACTIONINITIALIZATION_HH
#define __REMOLLUSERACTIONINITIALIZATION_HH

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

#endif /* __REMOLLUSERACTIONINITIALIZATION_HH */
