## MadGraph5 event generator

The MadGraph5 generator information can be used to generate Les Houches event files
for any arbitrary processes. Only cross section weights are generated, no asymmetries.

## Running MadGraph5
MadGraph5 uses a mixture of command line and web-based interfaces. The files stored in
this directory define a number of processes in ee and ep scattering, at leading order.

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

If there are any NaN values in the Les Houches event file, you will need to replace
those with valid numbers before conversion.
```
sed -i 's/nan/0.0/g' unweighted_events.lhe
```
