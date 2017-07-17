#ifndef __REMOLLGLOBALFIELD_HH
#define __REMOLLGLOBALFIELD_HH

/*!
   \class remollGlobalField
   \brief Global field interface
*/

#include "G4MagneticField.hh"

#include <vector>

class G4GenericMessenger;

class remollMagneticField;

class remollGlobalField : public G4MagneticField {
    public: 
        remollGlobalField();
	virtual ~remollGlobalField();

	void AddNewField(G4String& name);
	void SetFieldScaleByString(G4String& name_scale);
	void SetFieldScale(const G4String& name, G4double scale);
        void SetMagnetCurrentByString(G4String& name_scale);
	void SetMagnetCurrent(const G4String& name, G4double scale);

	void GetFieldValue(const G4double[], G4double*) const;

    private:
	std::vector<remollMagneticField*> fFields;

	remollMagneticField* GetFieldByName(const G4String& name);

	G4GenericMessenger* fMessenger;
};


#endif//__REMOLLGLOBALFIELD_HH
