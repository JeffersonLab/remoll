#include "remollGenericDetectorSum.hh"

G4ThreadLocal G4Allocator<remollGenericDetectorSum>* remollGenericDetectorSumAllocator = 0;

remollGenericDetectorSum::remollGenericDetectorSum(int detid, int copyid)
: fDetID(detid),fCopyID(copyid) {
  fEdep   = 0.0;
}

remollGenericDetectorSum::~remollGenericDetectorSum() { }

void remollGenericDetectorSum::AddEDep( int pid, G4ThreeVector pos, double ene ){
    sumdata_t data;

    data.edep = ene;
    data.x    = pos.x();
    data.y    = pos.y();
    data.z    = pos.z();
    data.pid  = pid;

    fData.push_back(data);

    return;
}

double remollGenericDetectorSum::GetEdep( int pid ){
    double esum = 0.0;

    for (std::vector<sumdata_t>::iterator it = fData.begin() ; it != fData.end(); ++it){
	if( pid==0 || (*it).pid == pid ){
	    esum += (*it).edep;
	}
    }

    return esum;
}

G4ThreeVector remollGenericDetectorSum::GetPos( int pid ){
    double esum = 0.0;
    double xsum, ysum, zsum;
    xsum = ysum = zsum = 0.0;

    for (std::vector<sumdata_t>::iterator it = fData.begin() ; it != fData.end(); ++it){
	if( pid==0 || (*it).pid == pid ){
	    xsum += (*it).x*(*it).edep;
	    ysum += (*it).y*(*it).edep;
	    zsum += (*it).z*(*it).edep;
	    esum += (*it).edep;
	}
    }

    return G4ThreeVector(xsum/esum, ysum/esum, zsum/esum);
}

remollGenericDetectorSum::remollGenericDetectorSum(const remollGenericDetectorSum &right)
: G4VHit(right) {
  // copy constructor
  fDetID  = right.fDetID;
  fCopyID = right.fCopyID;
  fEdep   = right.fEdep;
  fData   = right.fData;
}

const remollGenericDetectorSum& remollGenericDetectorSum::operator=(const remollGenericDetectorSum &right){
  (*this) = right;
  return *this;
}

G4int remollGenericDetectorSum::operator==(const remollGenericDetectorSum &right) const {
  return (this==&right) ? 1 : 0;
}
