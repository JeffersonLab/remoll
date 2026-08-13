## Selecting hits from certain detectors

All sensitive detectors are represented internally by a positive integer.

Although this is not a requirement by Geant4 (which assigns a name to the
sensitive detectors), having an integer make it easier to store hits in a
flat tree with a single integer variable representing the detector. This
then allows:
```
T->Draw("hit.y:hit.x","hit.pid < 20")
```

## Obtaining a listing of all detector numbers

The full listing of detector numbers can be obtained with a macro
```
build/remoll macros/printgeometry.mac
```
The number in square brackets at the end of the line indicates the
detector number for those volumes. If there is no number in square
brackets, that means that the volume does not have a sensitive detector
associated with it.

### Using volume name aliases for the detector numbers

To connect the numbers to more human readable detector names it is also
possible to use convenient aliases in the output tree:
```
T->Draw("hit.y:hit.x","hit.pid == dets.lv.logicDScoll_5")
T->Draw("hit.y:hit.x","hit.pid == dets.sd.pionLuciteDet")
```
In the first case (`dets.lv`) the logical volume name is used. In the second
case (`dets.sd`) the sensitive detector name is used.

### Quartz tile numbering scheme in remoll/develop branch
```
detector type front flush = 0xFF = decimal 15 x 10000
detector type back flush = 0xBF = decimal 11 x 10000
then segment number x 100
then ring number x 10
and then for ring 5: 1 = left, 2 = center, 3 = right as you look in beam direction.
For example;
151152 is FF, segment 11, ring 5, center
110430 is BF, segment 4, ring 3 (the last 0 means there is no left, right, or center)
```

