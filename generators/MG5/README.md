# MadGraph5 event generator

The MadGraph5 generator information can be used to generate Les Houches event files
for any arbitrary processes. Only cross section weights are generated, no asymmetries.

## Installing MadGraph5
MadGraph5 is distributed using the Bazaar version control system. Download it using
```
cd /usr/local
bzr branch lp:mg5amcnlo
```
For convienience, add the following script to your `/usr/local/bin` directory as `mg5.sh`
```
export PATH=/usr/local/mg5amcnlo/bin:$PATH
```
and start each session in which you wish to use MadGraph5 with `source mg5.sh`.

## Installing MadAnalysis5
MadAnalysis is also distributed using the Bazaar version control system. Download it using
```
cd /usr/local
bzr branch lp:madanalysis5
```
and create a script `ma5.sh` in your `/usr/local/bin` directory with
```
export PATH=/usr/local/madanalysis5/bin:$PATH
```

## Running MadGraph5
MadGraph5 uses a mixture of command line and web-based interfaces. The files stored in
this directory define a number of processes in ee and ep scattering, at leading order.

As an example, to create Moller scattering events using the card `proc_card_mg5_ee_ee.dat`
use the command
```
mg5_aMC proc_card_mg5_ee_ee.dat
```

## Running MadEvent
After running MadGraph5 to determine the relevant diagrams, we want to generate events
using MadEvent5. The executable for this is inside the directory structure created by
MadGraph5 under `bin/madevent`. The files distributed here start MadEvent automatically
with the `launch` command in the last line. You can just repeat the MadGraph5 calls
above.

The generated files are gzipped and need to be unzipped before moving on to the next step.

## Conversion from Les Houches to HepMC event format
Since MadGraph5 generates files in the Les Houches event format, we need to convert
them to the HepMC ASCII format first. We use the `lhef2hepmc` tool for this.

### Installing lhef2hepmc
After downloading from (https://rivet.hepforge.org/)[rivet.hepforge.org], extract and build:
```
wget http://www.hepforge.org/archive/rivet/lhef2hepmc.tar.gz
tar -zxvf lhef2hepmc.tar.gz
cd lhef2hepmc
make HEPMC_PREFIX=/usr/local
make PREFIX=/usr/local/lhef2hepmc/bin install
```
You can specify the location where HepMC is installed (if it is not installed systemwide),
and where to install `lhef2hepmc` (directory must exist or otherwise you will end up with
an executable with the name `bin`).

### Converting using lhef2hepmc
Now we can convert using
```
lhef2hepmc unweighted_events.lhe unweighted_events.hepmc
```
or, since we start from a gzipped file,
```
zcat generators/MG5/ep/ep_ep/Events/run_01/unweighted_events.lhe.gz | lhef2hepmc > generators/MG5/ep/ep_ep/Events/run_01/unweighted_events.hepmc
```

There may be NaN values in the Les Houches event file (for alpha_s, when the scale is
below Lambda_QCD), which you will need to replace with valid numbers before conversion.
```
sed -i 's/nan/0.0/g'
```
or
```
zcat generators/MG5/ep/ep_ep/Events/run_01/unweighted_events.lhe.gz | sed 's/nan/0.0/g' | lhef2hepmc > generators/MG5/ep/ep_ep/Events/run_01/unweighted_events.hepmc
```

A helper script is provided in `scripts/lhef2hepmc.sh`:
```
Usage: lhef2hepmc.sh input.lhe.gz output.hepmc
```
