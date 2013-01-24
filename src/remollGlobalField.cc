#include "remollGlobalField.hh"
#include "remollMagneticField.hh"

#define __GLOBAL_NDIM 3

remollGlobalField::remollGlobalField(){
}

remollGlobalField::~remollGlobalField(){
}

void remollGlobalField::AddNewField( G4String name ){

    remollMagneticField *thisfield = new remollMagneticField(name);

    if( thisfield->IsInit() ){
	fFields.push_back( thisfield );
	G4cout << __PRETTY_FUNCTION__ << ": field " << name << " was added." << G4endl;
    } else {
	G4cerr << "WARNING " << __FILE__ << " line " << __LINE__
	    << ": field " << name << " was not initialized." << G4endl;
    }

    return;
}

void remollGlobalField::GetFieldValue( const G4double p[], G4double *resB) const {
    G4double Bsum [__GLOBAL_NDIM], thisB[__GLOBAL_NDIM];
    int i;

    for( i = 0; i < __GLOBAL_NDIM; i++ ){
	Bsum[i] = 0.0;
    }	

    std::vector<remollMagneticField*>::const_iterator it = fFields.begin();
    for( it = fFields.begin(); it != fFields.end(); it++){
	(*it)->GetFieldValue( p, thisB );
	for( i = 0; i < __GLOBAL_NDIM; i++ ){
	    Bsum[i] += thisB[i];
	}	
    }

    for( i = 0; i < __GLOBAL_NDIM; i++ ){
	resB[i] = Bsum[i];
    }	

    return;
}


void remollGlobalField::SetFieldScale( G4String name, G4double scale ){
    std::vector<remollMagneticField*>::iterator it = fFields.begin();

    while( it != fFields.end() ){ 
	if( (*it)->GetName() == name ) break; 
	it++; 
    }

    if( it != fFields.end() ){ 
	G4cout << "Setting field " << name << " scale to " << scale << G4endl;
	(*it)->SetFieldScale(scale);
    } else {
	G4cerr << "WARNING " << __FILE__ << " line " << __LINE__
	    << ": field " << name << " not found." << G4endl;
    }

    return;
}

