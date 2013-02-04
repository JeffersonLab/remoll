#ifndef __REMOLLGLOBALFIELD_HH
#define __REMOLLGLOBALFIELD_HH

/*!
   \class remollGlobalField
   \brief Global field interface
*/

#include "G4MagneticField.hh"
#include "G4UImanager.hh"

class remollMagneticField;

class remollGlobalField : public G4MagneticField {
    public: 
	 remollGlobalField();
	~remollGlobalField();

	void AddNewField( G4String file );
	void SetFieldScale( G4String file, G4double scale  );
	void SetMagnetCurrent( G4String file, G4double scale  );

	void GetFieldValue( const G4double[], G4double *) const;

    private:
	std::vector<remollMagneticField *> fFields;

	remollMagneticField *GetFieldByName( G4String file );

};


#endif//__REMOLLGLOBALFIELD_HH
