#ifndef radDamage_h
#define radDamage_h 1
#include <vector>

class radDamage{
private:
  std::vector<std::vector<double> > xValNIEL;
  std::vector<std::vector<double> > yValNIEL;
  std::vector<std::vector<double> > xValMREM;
  std::vector<std::vector<double> > yValMREM;
  void InitNIEL();
  void InitMREM();
  double interpolate(std::vector<double> xV,
                     std::vector<double> yV,
                     double energy);

public:
  radDamage(){InitNIEL();InitMREM();};
  ~radDamage(){};

  double GetNIEL(int partType,double energy,double theta=0);
  double GetMREM(int partType,double energy,double theta=0);

};

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

#endif
