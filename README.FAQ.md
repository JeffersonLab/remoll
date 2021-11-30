# FAQ

We compile a list of frequently asked questions which will be integrated in the documentation at a future time.

## 1. During compilation I encountered a problem related to a missing `gitinfo.hh` file

In order to store the git revision information, we parse the git output when compiling the simulation. If you get errors about a missing `gitinfo.hh` file during compilation, it likely means that this file must be regenerated. Run the following commands in your `build` directory:
```
cmake ..
make
```
If this does resolve the problem, delete your `build` directory and recompile the code.

## 2. When should I use `/run/initialize`?

There may be some confusion on when to run `/run/initialize`. Other simulations
in Geant4 may not require you to run this command since all information is
available to the simulation upon startup.

### What does initialization do?

When you run `/run/initialize`, Geant4 does the following things (of relevance to us,
for full details look in the ActionInitialization class and in the Geant4 documentation):
- geometry is initialized (through DetectorConstruction), both material and parallel,
  and both volumes and sensitive detectors,
- physics lists are loaded.

### Why do we not run initialization upon startup?

Since we want to be able to load different geometries from within remoll, not by
specifying them on the command line, we hold off on running the initialization step
so you can use
```
/remoll/geometry/setfile geometry/mollerMother.gdml
/remoll/parallel/setfile geometry/mollerParallel.gdml
```
before the initialization. If you don't run these commands, defaults are used.

Since we want to allow flexibility in which physics is included (e.g. optical
photons or not), we hold off on running the initialization so you can enable or
disable certain physics processes:
```
/remoll/physlist/register QGSP_BERT_HP
/remoll/physlist/parallel/enable
/remoll/physlist/optical/enable
```
