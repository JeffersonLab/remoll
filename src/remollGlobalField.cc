#include "remollGlobalField.hh"

#include "G4TransportationManager.hh"
#include "G4FieldManager.hh"
#include "G4UImanager.hh"
#include "G4GenericMessenger.hh"

#include "G4PropagatorInField.hh"

#include "G4Mag_UsualEqRhs.hh"
#include "G4EqMagElectricField.hh"
#include "G4Mag_SpinEqRhs.hh"
#include "G4EqEMFieldWithSpin.hh"

#include "G4ExplicitEuler.hh"
#include "G4ImplicitEuler.hh"
#include "G4SimpleRunge.hh"
#include "G4SimpleHeum.hh"
#include "G4ClassicalRK4.hh"
#include "G4CashKarpRKF45.hh"

#include "remollMagneticField.hh"
#include "remollSystemOfUnits.hh"

#include <remolltypes.hh>
#include <remollRun.hh>
#include <remollRunData.hh>

#include <TMD5.h>
#include <sys/stat.h>

#include <stdio.h>

#define __GLOBAL_NDIM 3

G4ThreadLocal remollGlobalField* remollGlobalField::fObject = 0;

remollGlobalField* remollGlobalField::GetObject()
{
  if (!fObject) new remollGlobalField();
  return fObject;
}

remollGlobalField::remollGlobalField()
// NOTE: when changing defaults below, also change guidance in messenger commands
: fEquationType(0),fStepperType(4),
  fMinStep(0.01*mm),fDeltaChord(3.0*mm),
  fDeltaOneStep(0.01*mm),fDeltaIntersection(0.1*mm),
  fEpsMin(1.0e-5*mm),fEpsMax(1.0e-4*mm),
  fEquation(0),fEquationDoF(0),
  fFieldManager(0),fFieldPropagator(0),
  fStepper(0),fChordFinder(0)
{
    // Set static pointer
    fObject = this;

    // Get field propagator and managers
    G4TransportationManager* transportationmanager = G4TransportationManager::GetTransportationManager();
    fFieldPropagator = transportationmanager->GetPropagatorInField();
    fFieldManager = transportationmanager->GetFieldManager();

    // Connect field manager to this global field
    fFieldManager->SetDetectorField(this);

    // Create equation, stepper, and chordfinder
    SetEquation();
    SetStepper();
    SetChordFinder();
    SetAccuracyParameters();

    // Create generic messenger
    fMessenger = new G4GenericMessenger(this,"/remoll/","Remoll properties");
    fMessenger->DeclareMethod("addfield",&remollGlobalField::AddNewField,"Add magnetic field");
    fMessenger->DeclareMethod("scalefield",&remollGlobalField::SetFieldScaleByString,"Scale magnetic field");
    fMessenger->DeclareMethod("magcurrent",&remollGlobalField::SetMagnetCurrentByString,"Scale magnetic field by current");

    // Create global field messenger
    fGlobalFieldMessenger = new G4GenericMessenger(this,"/remoll/field/","Remoll global field properties");
    fGlobalFieldMessenger->DeclareMethod("equationtype",&remollGlobalField::SetEquationType,"Set equation type: \n 0: B-field, no spin (default); \n 1: EM-field, no spin; \n 2: B-field, with spin; \n 3: EM-field, with spin");
    fGlobalFieldMessenger->DeclareMethod("steppertype",&remollGlobalField::SetStepperType,"Set stepper type: \n 0: ExplicitEuler; \n 1: ImplicitEuler; \n 2: SimpleRunge; \n 3: SimpleHeum; \n 4: ClassicalRK4 (default); \n 5: CashKarpRKF45");
    fGlobalFieldMessenger->DeclareMethod("print",&remollGlobalField::PrintAccuracyParameters,"Print the accuracy parameters");
    fGlobalFieldMessenger->DeclareProperty("epsmin",fEpsMin,"Set the minimum epsilon of the field propagator");
    fGlobalFieldMessenger->DeclareProperty("epsmax",fEpsMax,"Set the maximum epsilon of the field propagator");
    fGlobalFieldMessenger->DeclareProperty("minstep",fMinStep,"Set the minimum step of the chord finder");
    fGlobalFieldMessenger->DeclareProperty("deltachord",fDeltaChord,"Set delta chord for the chord finder");
    fGlobalFieldMessenger->DeclareProperty("deltaonestep",fDeltaOneStep,"Set delta one step for the field manager");
    fGlobalFieldMessenger->DeclareProperty("deltaintersection",fMinStep,"Set delta intersection for the field manager");
}

remollGlobalField::~remollGlobalField()
{
  delete fMessenger;
  delete fGlobalFieldMessenger;

  if (fEquation)        delete fEquation;
  if (fStepper)         delete fStepper;
  if (fChordFinder)     delete fChordFinder;
}

void remollGlobalField::SetAccuracyParameters()
{
  // Set accuracy parameters
  fChordFinder->SetDeltaChord(fDeltaChord);

  fFieldManager->SetAccuraciesWithDeltaOneStep(fDeltaOneStep);
  fFieldManager->SetDeltaIntersection(fDeltaIntersection);

  fFieldPropagator->SetMinimumEpsilonStep(fEpsMin);
  fFieldPropagator->SetMaximumEpsilonStep(fEpsMax);
}

void remollGlobalField::PrintAccuracyParameters()
{
  G4cout << "Accuracy Parameters:" <<
            " MinStep = " << fMinStep <<
            " DeltaChord = " << fDeltaChord <<
            " DeltaOneStep = " << fDeltaOneStep << G4endl;
  G4cout << "                    " <<
            " DeltaIntersection = " << fDeltaIntersection <<
            " EpsMin = " << fEpsMin <<
            " EpsMax = " << fEpsMax <<  G4endl;
}

void remollGlobalField::SetEquation()
{
  if (fEquation) delete fEquation;

  switch (fEquationType)
  {
    case 0:
      G4cout << "G4Mag_UsualEqRhs is called with 6 dof" << G4endl;
      fEquation = new G4Mag_UsualEqRhs(this);
      fEquationDoF = 6;
      break;
    case 1:
      G4cout << "G4EqMagElectricField is called with 6 dof" << G4endl;
      fEquation = new G4EqMagElectricField(this);
      fEquationDoF = 6;
      break;
    case 2:
      G4cout << "G4Mag_SpinEqRhs is called with 12 dof" << G4endl;
      fEquation = new G4Mag_SpinEqRhs(this);
      fEquationDoF = 12;
      break;
    case 3:
      G4cout << "G4EqEMFieldWithSpin is called with 12 dof" << G4endl;
      fEquation = new G4EqEMFieldWithSpin(this);
      fEquationDoF = 12;
      break;
    default: fEquation = 0;
  }

  SetStepper();
}

void remollGlobalField::SetStepper()
{
  if (fStepper) delete fStepper;

  switch (fStepperType)
  {
    case 0:
      fStepper = new G4ExplicitEuler(fEquation, fEquationDoF);
      G4cout << "G4ExplicitEuler is called" << G4endl;
      break;
    case 1:
      fStepper = new G4ImplicitEuler(fEquation, fEquationDoF);
      G4cout << "G4ImplicitEuler is called" << G4endl;
      break;
    case 2:
      fStepper = new G4SimpleRunge(fEquation, fEquationDoF);
      G4cout << "G4SimpleRunge is called" << G4endl;
      break;
    case 3:
      fStepper = new G4SimpleHeum(fEquation, fEquationDoF);
      G4cout << "G4SimpleHeum is called" << G4endl;
      break;
    case 4:
      fStepper = new G4ClassicalRK4(fEquation, fEquationDoF);
      G4cout << "G4ClassicalRK4 (default) is called" << G4endl;
      break;
    case 5:
      fStepper = new G4CashKarpRKF45(fEquation, fEquationDoF);
      G4cout << "G4CashKarpRKF45 is called" << G4endl;
      break;
    default: fStepper = 0;
  }

  SetChordFinder();
}

void remollGlobalField::SetChordFinder()
{
  if (fChordFinder) delete fChordFinder;

  fChordFinder = new G4ChordFinder(this,fMinStep,fStepper);
  fFieldManager->SetChordFinder(fChordFinder);
}

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

        // FIXME disabled TMD5 functionality as long as CentOS 7.2 is common
        // due to kernel bug when running singularity containers

        //TMD5 *md5 = TMD5::FileChecksum(name.data());

        filedata_t fdata;

        strcpy(fdata.filename, name.data());
        strcpy(fdata.hashsum, "no hash" ); // md5->AsString() );

        //G4cout << "MD5 checksum " << md5->AsString() << G4endl;

        //delete md5;

        struct stat fs;
        stat(name.data(), &fs);
        fdata.timestamp = TTimeStamp( fs.st_mtime );

        G4cout << __FUNCTION__ << ": field timestamp = " << fdata.timestamp << G4endl;

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
