#include "remollGenLUND.hh"

// Geant4 includes
#include "G4Material.hh"
#include "G4GenericMessenger.hh"

// remoll includes
#include "remollBeamTarget.hh"
#include "remollEvent.hh"
#include "remollVertex.hh"
#include "remolltypes.hh"
#include "remollRun.hh"
#include "remollRunData.hh"

// System includes
#include <iostream>
#include <sstream>
#include <iterator>
#include <string>


remollGenLUND::remollGenLUND()
: remollVEventGen("pion_LUND") {
  // Initialize fRunData
  fRunData = remollRun::GetRunData();

  // Load table of particle types by LUND number
  pidname[111] = "pi0";
  pidname[211] = "pi+";
  pidname[-211] = "pi-";
  pidname[2112] = "neutron";
  pidname[2212] = "proton";

  // Start up with initialization flag to false
  bLUND = false;
  LUNDfile_linecount = 0;

  // Add to generic messenger
  fThisGenMessenger->DeclareMethod("LUND_filename",&remollGenLUND::SetLUNDFile,"LUND Input filename");
}

remollGenLUND::~remollGenLUND()
{
  LUNDfile.close();
}

void remollGenLUND::SamplePhysics(remollVertex * /*vert*/, remollEvent *evt)
{
  // check if valid LUND file loaded and number of thrown events is less than file length
  if (bLUND &&
      fRunData->GetNthrown() <= LUNDfile_linecount) {

    //read two lines from the LUND file
    G4double line1[10];
    G4double line2[14];

    //line1[9] is the rate factor
    LUNDfile >> line1[0] >> line1[1] >> line1[2] >> line1[3] >> line1[4] >> line1[5] >> line1[6] >> line1[7] >> line1[8] >> line1[9];
    //line2[3]-pid, line[6,7,8] - momentum, line[9]-energy,line[11,12,13] - vertex
    LUNDfile >> line2[0] >> line2[1] >> line2[2] >> line2[3] >> line2[4] >> line2[5] >> line2[6] >> line2[7] >> line2[8] >> line2[9] >> line2[10] >> line2[11] >> line2[12] >> line2[13];

    G4double ratefactor = line1[9];
    G4ThreeVector position(line2[11] * cm, line2[12] * cm, line2[13] * cm);
    G4ThreeVector momentum(line2[6] * GeV, line2[7] * GeV, line2[8] * GeV);

    G4int pid = (Int_t) line2[3];


    evt->SetRate(ratefactor);
    evt->SetAsymmetry(0.0);
    evt->SetEffCrossSection(1.0);
    evt->ProduceNewParticle(position, momentum, pidname[pid]);

  } else
    G4cerr << __FILE__ << " line " << __LINE__ << " - ERROR : LUND file events " << LUNDfile_linecount << " mis-match with G4,fRunData->GetNthrown() " <<  fRunData->GetNthrown()<< G4endl;
}

void remollGenLUND::SetLUNDFile(G4String& filename)
{
  // open the LUND file by name
  LUNDfile.open(filename, std::ios::in);

    // check if LUND file is open
  if (!LUNDfile.good()) { 
    G4cerr << "LUND file " << filename << " does not exist." << G4endl;
    bLUND = false;
    return;
  } else {
    // print file name
    G4cout << "LUND file " << filename << " opened successfully." << G4endl;
    bLUND = true;
  }

  // Make sure that number of events thrown out by G4 is less or equal to that in LUND file
  std::string line;
  std::vector<std::string> words;
  while (std::getline(LUNDfile, line)) {
    std::stringstream stream(line);
    int line_len = std::distance(std::istream_iterator<std::string>(stream), std::istream_iterator<std::string>());
    //stream.clear(); // reset stream flags
    if (line_len==10){
      //use stream2.clear() to clear all the flags but sometimes it fails
      //therefore always have a new string stream to avoid unexpected results
      std::stringstream stream2(line);
      //read the ratefactor to the variable words
      words.clear();
      std::copy(std::istream_iterator<std::string>(stream2),std::istream_iterator<std::string>(),std::back_inserter(words));
      break;
    }//else
    G4cerr << " line len "<<line_len<< G4endl;
  }

  // reset the file back to first line
  LUNDfile.clear();
  LUNDfile.seekg(0, std::ios::beg);

  //this simple trick only works for when there are one particle per event as input in the lund file
  //size_t linecount = std::count(std::istreambuf_iterator<char>(LUND_in2),std::istreambuf_iterator<char>(), '\n')/2;
  //count no.of lines in LUND file with the ratefactor
  LUNDfile_linecount = std::count(std::istream_iterator<std::string>(LUNDfile), std::istream_iterator<std::string>(),words[9]);

  // reset the file back to first line
  LUNDfile.clear();
  LUNDfile.seekg(0, std::ios::beg);
}


