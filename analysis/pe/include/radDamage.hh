#ifndef radDamage_h
#define radDamage_h 1
#include <vector>

class radDamage{
private:
  std::vector<std::vector<double> > xValNEIL;
  std::vector<std::vector<double> > yValNEIL;
  std::vector<std::vector<double> > xValMREM;
  std::vector<std::vector<double> > yValMREM;
  void InitNEIL();
  void InitMREM();
  double interpolate(std::vector<double> xV,
                     std::vector<double> yV,
                     double energy);

public:
  radDamage(){InitNEIL();InitMREM();};
  ~radDamage(){};

  double getNEIL(int partType,double energy,double theta=0);
  double getMREM(int partType,double energy,double theta=0);

};

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

#endif
