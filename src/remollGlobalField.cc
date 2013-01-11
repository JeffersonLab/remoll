#include "remollGlobalField.hh"
#include "remollMagneticField.hh"

remollGlobalField::remollGlobalField(){
}

remollGlobalField::~remollGlobalField(){
}

void remollGlobalField::AddNewField( G4String name ){

    remollMagneticField *thisfield = new remollMagneticField(name);

    if( thisfield->IsInit() ){
	fFields.push_back( thisfield );
    } else {
	G4cerr << "WARNING " << __FILE__ << " line " << __LINE__
	    << ": field " << name << " was not initialized." << G4endl;
    }

    return;
}

void remollGlobalField::SetFieldScale( G4String name, G4double scale ){
    std::vector<remollMagneticField*>::iterator it = fFields.begin();

    while( it != fFields.end() && (*it)->GetName() != name ){ it++; }

    if( (*it)->GetName() == name ){ 
	(*it)->SetFieldScale(scale);
    } else {
	G4cerr << "WARNING " << __FILE__ << " line " << __LINE__
	    << ": field " << name << " not found." << G4endl;
    }

    return;
}

