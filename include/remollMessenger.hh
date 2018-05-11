#ifndef remollMessenger_HH
#define remollMessenger_HH

#include "remolltypes.hh"

#include "G4UImessenger.hh"

/*!
 *   Global messenger class
 */

class G4UIcmdWithAnInteger;

class remollMessenger : public G4UImessenger {

    private:
        // Singleton pointer
        static remollMessenger* gInstance;
        // Private constructor
        remollMessenger();

    public:
       	// Public destructor
        virtual ~remollMessenger();
        // Static instance getter
       	static remollMessenger* GetInstance();

	void SetNewValue(G4UIcommand* cmd, G4String newValue);

    private:

	G4UIcmdWithAnInteger *seedCmd;
};

#endif//remollMessenger_HH























