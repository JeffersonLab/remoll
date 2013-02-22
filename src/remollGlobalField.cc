#include "remollGlobalField.hh"
#include "G4TransportationManager.hh"
#include "G4FieldManager.hh"
#include "remollMagneticField.hh"

#include <remolltypes.hh>
#include <remollRun.hh>
#include <remollRunData.hh>

#include <TMD5.h>
#include <sys/stat.h>

#include <stdio.h>

#define __GLOBAL_NDIM 3

remollGlobalField::remollGlobalField(){
}

remollGlobalField::~remollGlobalField(){
}

void remollGlobalField::AddNewField( G4String name ){

    remollMagneticField *thisfield = new remollMagneticField(name);

    if( thisfield->IsInit() ){
	fFields.push_back( thisfield );

	// I don't know why it's necessary to do the following - SPR 1/24/13
	// Recreating the chord finder makes stepping bearable
	// in cases where you change the geometry. 
	G4TransportationManager::GetTransportationManager()->GetFieldManager()->CreateChordFinder(this);

	G4cout << __FUNCTION__ << ": field " << name << " was added." << G4endl;

	// Add file data to output data stream

	remollRunData *rd = remollRun::GetRun()->GetData();

	TMD5 *md5 = TMD5::FileChecksum(name.data());

	filedata_t fdata;

	strcpy(fdata.filename, name.data());
	strcpy(fdata.hashsum, md5->AsString() );

	G4cout << "MD5 checksum " << md5->AsString() << G4endl;

	delete md5;

	struct stat fs;
	stat(name.data(), &fs);
	fdata.timestamp = TTimeStamp( fs.st_mtime );

	fdata.timestamp.Print();

	rd->AddMagData(fdata);

    } else {
	G4cerr << "WARNING " << __FILE__ << " line " << __LINE__
	    << ": field " << name << " was not initialized." << G4endl;
    }

    return;
}

remollMagneticField *remollGlobalField::GetFieldByName(G4String name) {
    std::vector<remollMagneticField*>::iterator it = fFields.begin();

    while( it != fFields.end() ){ 
	if( (*it)->GetName() == name ) break; 
	it++; 
    }

    if( it != fFields.end() ){ 
	return (*it);
    } else {
	G4cerr << "WARNING " << __FILE__ << " line " << __LINE__
	    << ": field " << name << " not found." << G4endl;
	return NULL;
    }
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
	remollMagneticField *field = GetFieldByName(name);
	if( field ){
	    field->SetFieldScale(scale);
	} else {
	    G4cerr << "WARNING " << __FILE__ << " line " << __LINE__
		<< ": field " << name << " scaling failed" << G4endl;
	}

    return;
}

void remollGlobalField::SetMagnetCurrent( G4String name, G4double scale ){
	remollMagneticField *field = GetFieldByName(name);
	if( field ){
	    field->SetMagnetCurrent(scale);
	} else {
	    G4cerr << "WARNING " << __FILE__ << " line " << __LINE__
		<< ": field " << name << " scaling failed" << G4endl;
	}

    return;
}
