#include "remollGlobalField.hh"

#include "G4TransportationManager.hh"
#include "G4FieldManager.hh"
#include "G4UImanager.hh"
#include "G4GenericMessenger.hh"

#include "remollMagneticField.hh"

#include <remolltypes.hh>
#include <remollRun.hh>
#include <remollRunData.hh>

#include <TMD5.h>
#include <sys/stat.h>

#include <stdio.h>

#define __GLOBAL_NDIM 3

remollGlobalField::remollGlobalField()
{
    G4TransportationManager* transportationmanager = G4TransportationManager::GetTransportationManager();
    G4FieldManager* fieldmanager = transportationmanager->GetFieldManager();
    fieldmanager->SetDetectorField(this);
    fieldmanager->CreateChordFinder(this);

    // Create generic messenger
    fMessenger = new G4GenericMessenger(this,"/remoll/","Remoll properties");
    fMessenger->DeclareMethod("addfield",&remollGlobalField::AddNewField,"Add magnetic field");
    fMessenger->DeclareMethod("scalefield",&remollGlobalField::SetFieldScaleByString,"Scale magnetic field");
    fMessenger->DeclareMethod("magcurrent",&remollGlobalField::SetMagnetCurrentByString,"Scale magnetic field by current");
}

remollGlobalField::~remollGlobalField() { }

void remollGlobalField::AddNewField(G4String& name)
{
    remollMagneticField *thisfield = new remollMagneticField(name);

    if (thisfield->IsInit()) {
        fFields.push_back(thisfield);

        // I don't know why it's necessary to do the following - SPR 1/24/13
        // Recreating the chord finder makes stepping bearable
        // in cases where you change the geometry.
        G4TransportationManager::GetTransportationManager()->GetFieldManager()->CreateChordFinder(this);

        G4cout << __FUNCTION__ << ": field " << name << " was added." << G4endl;

        // Add file data to output data stream

        remollRunData *rd = remollRun::GetRunData();

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
}

remollMagneticField* remollGlobalField::GetFieldByName(const G4String& name)
{
    std::vector<remollMagneticField*>::iterator it = fFields.begin();
    while (it != fFields.end()) {
        if ((*it)->GetName() == name) break;
        it++;
    }

    if (it != fFields.end()) {
        return (*it);
    } else {
        G4cerr << "WARNING " << __FILE__ << " line " << __LINE__
            << ": field " << name << " not found." << G4endl;
        return NULL;
    }
}

void remollGlobalField::GetFieldValue( const G4double p[], G4double *resB) const
{
    G4double Bsum [__GLOBAL_NDIM], thisB[__GLOBAL_NDIM];

    for (int i = 0; i < __GLOBAL_NDIM; i++) {
        Bsum[i] = 0.0;
    }

    std::vector<remollMagneticField*>::const_iterator it = fFields.begin();
    for (it = fFields.begin(); it != fFields.end(); it++) {
        (*it)->GetFieldValue(p, thisB);
        for (int i = 0; i < __GLOBAL_NDIM; i++) {
          Bsum[i] += thisB[i];
        }
    }

    for (int i = 0; i < __GLOBAL_NDIM; i++) {
        resB[i] = Bsum[i];
    }
}

void remollGlobalField::SetFieldScaleByString(G4String& name_scale)
{
  std::istringstream iss(name_scale);

  G4String name, scalestr;
  iss >> name;
  iss >> scalestr;

  G4double scaleval = atof(scalestr);
  SetFieldScale(name, scaleval);
}

void remollGlobalField::SetFieldScale(const G4String& name, G4double scale)
{
    remollMagneticField *field = GetFieldByName(name);
    if (field) {
        field->SetFieldScale(scale);
    } else {
        G4cerr << "WARNING " << __FILE__ << " line " << __LINE__
            << ": field " << name << " scaling failed" << G4endl;
    }
}

void remollGlobalField::SetMagnetCurrentByString(G4String& name_scale)
{
  std::istringstream iss(name_scale);

  G4String name, scalestr, scaleunit;
  iss >> name;
  iss >> scalestr;
  iss >> scaleunit;

  if (scaleunit != "A") {
    // FIXME: less snark and more functionality?
    G4cerr << __FILE__ << " line " << __LINE__ <<  ":\n\tGraaaah - just put the current for " <<  name <<  " in amps..." << G4endl;
    exit(1);
  }

  G4double scaleval = atof(scalestr);
  SetMagnetCurrent(name, scaleval);
}

void remollGlobalField::SetMagnetCurrent(const G4String& name, G4double scale)
{
    remollMagneticField *field = GetFieldByName(name);
    if (field) {
        field->SetMagnetCurrent(scale);
    } else {
        G4cerr << "WARNING " << __FILE__ << " line " << __LINE__
            << ": field " << name << " scaling failed" << G4endl;
    }
}
