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

#include "G4Threading.hh"
#include "G4AutoLock.hh"
namespace { G4Mutex remollGlobalFieldMutex = G4MUTEX_INITIALIZER; }

std::vector<remollMagneticField*> remollGlobalField::fFields;

remollGlobalField::remollGlobalField()
// NOTE: when changing defaults below, also change guidance in messenger commands
: fEquationType(0),fStepperType(4),
  fMinStep(0.01*mm),fDeltaChord(3.0*mm),
  fDeltaOneStep(0.01*mm),fDeltaIntersection(0.1*mm),
  fEpsMin(1.0e-5*mm),fEpsMax(1.0e-4*mm),
  fEquation(0),fEquationDoF(0),
  fFieldManager(0),fFieldPropagator(0),
  fStepper(0),fChordFinder(0),
  fVerboseLevel(0)
{
    // Get field propagator and managers
    G4TransportationManager* transportationmanager = G4TransportationManager::GetTransportationManager();
    fFieldPropagator = transportationmanager->GetPropagatorInField();
    fFieldManager = transportationmanager->GetFieldManager();

    // Create equation, stepper, and chordfinder
    SetEquation();
    SetStepper();
    SetChordFinder();
    SetAccuracyParameters();

    // Connect field manager to this global field
    fFieldManager->SetDetectorField(this);

    // Create generic messenger
    fMessenger = new G4GenericMessenger(this,"/remoll/","Remoll properties");
    fMessenger->DeclareMethod("addfield",&remollGlobalField::AddNewField,"Add magnetic field");

    // Create global field messenger
    fGlobalFieldMessenger = new G4GenericMessenger(this,"/remoll/field/","Remoll global field properties");
    fGlobalFieldMessenger->DeclareMethod("equationtype",&remollGlobalField::SetEquationType,"Set equation type: \n 0: B-field, no spin (default); \n 2: B-field, with spin");
    fGlobalFieldMessenger->DeclareMethod("steppertype",&remollGlobalField::SetStepperType,"Set stepper type: \n 0: ExplicitEuler; \n 1: ImplicitEuler; \n 2: SimpleRunge; \n 3: SimpleHeum; \n 4: ClassicalRK4 (default); \n 5: CashKarpRKF45");
    fGlobalFieldMessenger->DeclareMethod("print",&remollGlobalField::PrintAccuracyParameters,"Print the accuracy parameters");
    fGlobalFieldMessenger->DeclareProperty("epsmin",fEpsMin,"Set the minimum epsilon of the field propagator");
    fGlobalFieldMessenger->DeclareProperty("epsmax",fEpsMax,"Set the maximum epsilon of the field propagator");
    fGlobalFieldMessenger->DeclareProperty("minstep",fMinStep,"Set the minimum step of the chord finder");
    fGlobalFieldMessenger->DeclareProperty("deltachord",fDeltaChord,"Set delta chord for the chord finder");
    fGlobalFieldMessenger->DeclareProperty("deltaonestep",fDeltaOneStep,"Set delta one step for the field manager");
    fGlobalFieldMessenger->DeclareProperty("deltaintersection",fMinStep,"Set delta intersection for the field manager");
    fGlobalFieldMessenger->DeclareMethod("zoffset",&remollGlobalField::SetZOffset,"Set magnetic field z offset");
    fGlobalFieldMessenger->DeclareMethod("scale",&remollGlobalField::SetFieldScale,"Scale magnetic field by factor");
    fGlobalFieldMessenger->DeclareMethod("current",&remollGlobalField::SetMagnetCurrent,"Scale magnetic field by current");
    fGlobalFieldMessenger->DeclareMethod("value",&remollGlobalField::PrintFieldValue,"Print the field value at a given point (in m)");
    fGlobalFieldMessenger->DeclareProperty("verbose",fVerboseLevel,"Set the verbose level");
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
      if (fVerboseLevel > 0) G4cout << "G4Mag_UsualEqRhs is called with 6 dof" << G4endl;
      fEquation = new G4Mag_UsualEqRhs(this);
      fEquationDoF = 6;
      break;
    case 2:
      if (fVerboseLevel > 0) G4cout << "G4Mag_SpinEqRhs is called with 12 dof" << G4endl;
      fEquation = new G4Mag_SpinEqRhs(this);
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
      if (fVerboseLevel > 0) G4cout << "G4ExplicitEuler is called" << G4endl;
      break;
    case 1:
      fStepper = new G4ImplicitEuler(fEquation, fEquationDoF);
      if (fVerboseLevel > 0) G4cout << "G4ImplicitEuler is called" << G4endl;
      break;
    case 2:
      fStepper = new G4SimpleRunge(fEquation, fEquationDoF);
      if (fVerboseLevel > 0) G4cout << "G4SimpleRunge is called" << G4endl;
      break;
    case 3:
      fStepper = new G4SimpleHeum(fEquation, fEquationDoF);
      if (fVerboseLevel > 0) G4cout << "G4SimpleHeum is called" << G4endl;
      break;
    case 4:
      fStepper = new G4ClassicalRK4(fEquation, fEquationDoF);
      if (fVerboseLevel > 0) G4cout << "G4ClassicalRK4 (default) is called" << G4endl;
      break;
    case 5:
      fStepper = new G4CashKarpRKF45(fEquation, fEquationDoF);
      if (fVerboseLevel > 0) G4cout << "G4CashKarpRKF45 is called" << G4endl;
      break;
    default: fStepper = 0;
  }

  SetChordFinder();
}

void remollGlobalField::SetChordFinder()
{
  if (fChordFinder) delete fChordFinder;

  fChordFinder = new G4ChordFinder(this,fMinStep,fStepper);
  fChordFinder->GetIntegrationDriver()->SetVerboseLevel(0);
  fFieldManager->SetChordFinder(fChordFinder);
}

void remollGlobalField::AddNewField(G4String& name)
{
  // Lock mutex to ensure only 1 thread is loading a field
  G4AutoLock lock(&remollGlobalFieldMutex);

  // If this field has already been loaded
  if (GetFieldByName(name) != 0) return;
  
  // Load new field
  remollMagneticField *thisfield = new remollMagneticField(name);
  if (thisfield->IsInit()) {
    fFields.push_back(thisfield);

    if (fVerboseLevel > 0)
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

    if (fVerboseLevel > 0)
      G4cout << __FUNCTION__ << ": field timestamp = " << fdata.timestamp << G4endl;

    rd->AddMagData(fdata);

  } else {
    G4cerr << "WARNING " << __FILE__ << " line " << __LINE__
           << ": field " << name << " was not initialized." << G4endl;
  }
}

remollMagneticField* remollGlobalField::GetFieldByName(const G4String& name) const
{
    for (auto it = fFields.begin(); it != fFields.end(); it++)
        if ((*it)->GetName() == name)
          return (*it);

    return 0;
}

void remollGlobalField::PrintFieldValue(const G4ThreeVector& r)
{
    G4double B[__GLOBAL_NDIM];
    G4double p[] = {r.x()*m, r.y()*m, r.z()*m, 0.0};
    GetFieldValue(p, B);
    G4cout << "At r" << r << " [m]: B = ";
    for (int i = 0; i < __GLOBAL_NDIM; i++) {
        G4cout << B[i] << " ";
    }
    G4cout << "T" << G4endl;
}

void remollGlobalField::GetFieldValue(const G4double p[], G4double *resB) const
{
    G4double Bsum [__GLOBAL_NDIM] = {0};

    std::vector<remollMagneticField*>::const_iterator it = fFields.begin();
    for (it = fFields.begin(); it != fFields.end(); it++) {
        G4double thisB[__GLOBAL_NDIM] = {0};
        (*it)->GetFieldValue(p, thisB);

        for (int i = 0; i < __GLOBAL_NDIM; i++) {
          Bsum[i] += thisB[i];
        }
    }

    for (int i = 0; i < __GLOBAL_NDIM; i++) {
        resB[i] = Bsum[i];
    }
}

void remollGlobalField::SetZOffset(const G4String& name, G4double offset)
{
  remollMagneticField *field = GetFieldByName(name);
  if (field) {
    G4AutoLock lock(&remollGlobalFieldMutex);
    field->SetZoffset(offset);
  } else {
    G4cerr << "WARNING " << __FILE__ << " line " << __LINE__
           << ": field " << name << " offset failed" << G4endl;
  }
}

void remollGlobalField::SetFieldScale(const G4String& name, G4double scale)
{
  remollMagneticField *field = GetFieldByName(name);
  if (field) {
    G4AutoLock lock(&remollGlobalFieldMutex);
    field->SetFieldScale(scale);
  } else {
    G4cerr << "WARNING " << __FILE__ << " line " << __LINE__
           << ": field " << name << " scaling failed" << G4endl;
  }
}

void remollGlobalField::SetMagnetCurrent(const G4String& name, G4double current)
{
  remollMagneticField *field = GetFieldByName(name);
  if (field) {
    G4AutoLock lock(&remollGlobalFieldMutex);
    field->SetMagnetCurrent(current);
  } else {
    G4cerr << "WARNING " << __FILE__ << " line " << __LINE__
           << ": field " << name << " scaling failed" << G4endl;
  }
}
