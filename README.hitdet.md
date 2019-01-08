## Selecting hits from certain detectors

All sensitive detectors are represented internally by a positive integer.

Although this is not a requirement by Geant4 (which assigns a name to the
sensitive detectors), having an integer make it easier to store hits in a
flat tree with a single integer variable representing the detector. This
then allows:
```
T->Draw("hit.y:hit.x","hit.pid < 20")
```

To connect the numbers to more human readable detector names it is also
possible to use convenient aliases in the output tree:
```
T->Draw("hit.y:hit.x","hit.pid == dets.lv.logicDScoll_5")
T->Draw("hit.y:hit.x","hit.pid == dets.sd.pionLuciteDet")
```
In the first case (`dets.lv`) the logical volume name is used. In the second
case (`dets.sd`) the sensitive detector name is used.
