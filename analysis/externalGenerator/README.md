Both the external generator and the remoll generator require user input files.

--- External Generator --

The input file must be a ROOT file containing a TTree with the remoll v2.0.0 formatting

In your macro:

/run/initialize
/remoll/evgen/set moller
/remoll/filename remollin.root
/run/beamOn 1000

/remoll/filename remollout.root

/remoll/evgen/set external
/run/beamOn 0
/remoll/evgen/external/file <input file name>
/remoll/evgen/external/detid <detector ID>
/remoll/evgen/external/startEvent 0
/remoll/filename remollout.root
/run/beamOn 1

--- Remoll Generator ---

This generator takes in a ROOT file containing parameterized functions of the radius (TF1) and generates
electrons according to the function you choose (IN DEVELOPMENT)

The default input file is remollGenFunctions which contains TF1s that describe moller, inelastic, and elastic
scattering for the whole ring as well as for sectors 1, 2, and 3 individually. The function names are formatted
as <scattering type> _ <sector number> where 0 is all three sectors.

In your macro:

/remoll/evgen/set remoll
/run/beamOn 0
/remoll/evgen/external/input <file name>:<function name>

*THIS GENERATOR IS STILL BEING DEVELOPED*
