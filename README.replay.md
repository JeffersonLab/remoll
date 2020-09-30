## Replaying simulated events

These instructions explain how to replay exactly some previously simulated events.

### Run the original large-N simulations

Not every simulation output file can automatically be used to replay certain events.

You must enable storing the random seed status for each event. This is done by default on recent versions of remoll, but on older versions you will need to run with the following flag enabled:
```
/run/storeRndmStatToEvent 1
```

### Select events of interest

Now, we select a small-ish number of events in the ROOT tree. You will need to use `reroot` for this.

Example:

Suppose you have identified that all events in the following histogram are of interest for replaying:
```
T->Draw("hit.r","hit.det==8001 && hit.pid!=22 && hit.trid>2 && hit.r<1100*mm")
```
You can now tell ROOT to write out the random engine state files for those events:
```
T->Draw("hit.r*seed.Save()","hit.det==8001 && hit.pid!=22 && hit.trid>2 && hit.r<1100*mm")
```
The `seed.Save()` function always just returns 1, and writes the random engine state files.

You should end up with a bunch of files with names like `run0evt118.state`.

### Convert random state file format

Next, we convert the state files into a different file format and set them up to go to individual runs. This will allow you to click through them one by one.
```
scripts/convert-mixmaxrng-states-to-runs.sh run0evt*.state
```
You should end up with a bunch of files with names like `run0evt0.rndm`, `run1evt0.rndm`, etc. They will all be event number 0 in consecutive runs.

Note that the wildcard will sort alphabetically, not by event number, so it will sort `run0evt225.state` before `run0evt31.state`. That is the order that the `run0evt0.rndm`, `run1evt0.rndm` files will be in.

### Replay the events in remoll

Finally, start remoll interactively and execute exactly the same setup as in the first step: same geometry, physics list, etc.

Before starting an actual event simulation, tell it to load the `run0evt0.rndm`, `run1evt0.rndm` files with
```
/random/resetEngineFromEachEvent 1
```
Now you can rerun each event again with `/run/beamOn 1`, or the play button on the graphical interface. Whenever it is starting a new run (of 1 event, in this case) and a new event, it will first check for the `run0evt0.rndm`, `run1evt0.rndm` files.

Note: If you click play beyond the end of any stored events, it will just generate regular random events (and it will not warn you about this).
