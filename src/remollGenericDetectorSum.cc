#include "remollGenericDetectorSum.hh"

G4ThreadLocal G4Allocator<remollGenericDetectorSum>* remollGenericDetectorSumAllocator = 0;

remollGenericDetectorSum::remollGenericDetectorSum(int detid, int copyid)
: fDetID(detid),fCopyID(copyid) {
  fEdep   = 0.0;

  // Particles that are payed attention to in our sums, 0 means all types
  std::vector<int> particles_to_track;
  particles_to_track.push_back(0);
  particles_to_track.push_back(-11);
  particles_to_track.push_back( 11);
  particles_to_track.push_back( 22);
  particles_to_track.push_back(2112);
  particles_to_track.push_back(2212);
  particles_to_track.push_back(-211);
  particles_to_track.push_back( 211);
}

remollGenericDetectorSum::~remollGenericDetectorSum() { }

void remollGenericDetectorSum::AddEDep(int pid, G4ThreeVector pos, double ene)
{
    remollGenericDetectorSumByPID_t sum_by_pid;

    sum_by_pid.edep = ene;
    sum_by_pid.x    = pos.x();
    sum_by_pid.y    = pos.y();
    sum_by_pid.z    = pos.z();
    sum_by_pid.pid  = pid;

    fSumByPID.push_back(sum_by_pid);
}

double remollGenericDetectorSum::GetEdep(int pid) const
{
    double esum = 0.0;

    for (std::vector<remollGenericDetectorSumByPID_t>::const_iterator it = fSumByPID.begin() ; it != fSumByPID.end(); ++it){
	if( pid==0 || (*it).pid == pid ){
	    esum += (*it).edep;
	}
    }

    return esum;
}

G4ThreeVector remollGenericDetectorSum::GetPos(int pid) const
{
    double esum = 0.0;
    double xsum, ysum, zsum;
    xsum = ysum = zsum = 0.0;

    for (std::vector<remollGenericDetectorSumByPID_t>::const_iterator it = fSumByPID.begin() ; it != fSumByPID.end(); ++it){
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
  fSumByPID   = right.fSumByPID;
}

const remollGenericDetectorSum& remollGenericDetectorSum::operator=(const remollGenericDetectorSum &right){
  (*this) = right;
  return *this;
}

G4int remollGenericDetectorSum::operator==(const remollGenericDetectorSum &right) const {
  return (this==&right) ? 1 : 0;
}
