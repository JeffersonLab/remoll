#include "remollFileReader.hh"

remollFileReader::remollFileReader(G4String filename, G4int skip)
{
  fInputFile.open(filename.data());
  while (skip-- > 0) {
    std::string line;
    std::getline(fInputFile, line);
    G4cout << line << G4endl;
  }
}

remollFileReader::~remollFileReader()
{
  fInputFile.close();
}

remollFileEvent remollFileReader::GetAnEvent()
{
  if (fEventList.size() == 0)
  {
    for (int i = 0; i < 100; i++)
    {
      G4double vx, vy, vz, px, py, pz, w;
      fInputFile >> vx >> vy >> vz >> px >> py >> pz >> w;
      fEventList.push_back(remollFileEvent(G4ThreeVector(vx,vy,vz),G4ThreeVector(px,py,pz),w));
    }
  }
  remollFileEvent event = fEventList.front();
  fEventList.pop_front();
  return event;
}
