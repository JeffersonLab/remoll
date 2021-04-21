## Output tree variables in remoll

The output trees are organized into several independent arrays:
- `ev.*`    - primary event information
- `part.*`  - primary particles
- `bm.*`    - beam information
- `hit.*`   - detector hits
- `sum.*`   - detector sums
- `units.*` - unit conversions
- `rate`    - weight variable for conversion to event rates

All fields are filled in Geant4's internal system of units. This ensures
consistency when combining variables with different units together.

To plot variables in other units when using ROOT, a helper branch `units`
is stored as well. If you wish to plot variables not in their stored units,
you can use the `units` branch to renormalize them:
```
T->Draw("hit.pz/GeV:hit.z/m")
```
This will plot momentum in GeV versus position in meters (and even label the
axes accordingly with "hit.pz/GeV").

### Primary event information

The branch `ev` is filled once for each event with information related to the
entire event.

- ev.A		Physics asymmetry [ppb]
- ev.Am		Measured asymmetry (scaled by beam polarization) [ppb]
- ev.xs		Physics process cross section [ub]
- ev.Q2		Momentum transfer Q2 [MeV^2] (not available for all processes)
- ev.W2		Invariant mass squared [MeV^2] (not available for all processes)
- ev.thcom	Polar angle theta in center-of-mass [rad] (not available for all processes)
- ev.beamp	Beam momentum magnitude at primary vertex [MeV]

Examples:
```
T->Draw("ev.A/ppb","ev.thcom > 90*deg")
```

- rate		Weight variable for conversion to event rates [???]

Examples:
```
T->Draw("ev.A/ppb","rate * (ev.thcom > 90*deg)")
```

### Primary particles

The branch `part` is filled with as many entries as the number of primary particles.
In the case of the Moller generator this means there will be two `part` entries per
event.

- part.pid	Geant4 particle type per http://pdg.lbl.gov/2018/reviews/rpp2018-rev-monte-carlo-numbering.pdf
- part.v[xyz]	Particle initial position, global coordinates [mm]
- part.p	Particle initial momentum magnitude, lab frame [MeV]
- part.p[xyz]	Particle initial momentum components, lab frame [MeV]
- part.s[xyz]	Particle initial polarization components, lab frame [1]
- part.th	Particle initial polar angle, lab frame [rad]
- part.ph	Particle initial azimuthal angle, lab frame [rad]
- part.tp[xyz]	Particle "true" momentum (if no multiple scattering effects)
- part.trid	Geant4 track ID number (1 = first particle created, stored per tj[xyz] entry)

Examples:
```
T->Draw("part.th/deg","part.pid == 11")
```

- part.tj[xyz]  std::vector of particle trajectory positions [mm]

### Beam information

The branch `beam` is filled once for each event with the position
and direction of the primary vertex after going through multiple
scattering in the target.

- bm.[xyz]	Beam interaction vertex, global coordinates [mm]
- bm.d[xyz]	Beam interaction direction, lab frame [rad]
- bm.th		Beam direction polar angle, lab frame [rad]
- bm.ph		Beam direction azimuthal angle, lab frame [rad]

Examples:
```
T->Draw("bm.th/deg")
```

### Hit information

The branch `hit` is filled for every hit.

- hit.id	Hit identifier
- hit.det	Detector identifier
- hit.vid	Volume ID number (not yet implemented)
- hit.pid	Geant4 particle type per http://pdg.lbl.gov/2018/reviews/rpp2018-rev-monte-carlo-numbering.pdf
- hit.trid	Geant4 track identifier (1 = first particle created)
- hit.mtrid	Geant4 mother track identifier (0 = particle from gun)
- hit.t		Hit time [ns]
- hit.[xyz]	Hit position, global coordinates [mm]
- hit.[xyz]l	Hit position, local coordinates [mm]
- hit.r		Hit position radial, global coordinates [mm]
- hit.ph	Hit position azimuthal, global coordinates [mm]
- hit.p		Particle momentum magnitude [MeV]
- hit.p[xyz]	Particle momentum components, lab frame [MeV]
- hit.s[xyz]	Particle polarization components, lab frame [1]
- hit.v[xyz]	Particle creation vertex position [mm]
- hit.e		Particle energy [MeV]
- hit.m		Particle rest mass [MeV]
- hit.edep	Energy deposited in this detector by this hit [MeV]

Examples:
```
T->Draw("hit.x/m:hit.y/m","hit.pid < 20")
```

### Detector sum

The branch `sum` is filled for every detector that had hits during
the current event.

- sum.det	Detector identifier
- sum.vid	Volume identifier (not yet implemented)
- sum.edep	Energy deposited in this detector by all hits [MeV]

Examples:
```
T->Scan("sum.det:sum.edep","sum.det < 20")
```

The branch `sum.by_pid` is filled for every detector that had hits
and for every particle type among those hits.

- sum.by_pid.[xyz]	Energy deposited hit position, global coordinates [mm]
- sum.by_pid.pid	Geant4 particle type per http://pdg.lbl.gov/2018/reviews/rpp2018-rev-monte-carlo-numbering.pdf
- sum.by_pid.edep	Energy deposited in this detector by all hits of this particle type [MeV]

Examples:
```
T->Scan("sum.det:sum.by_pid.x:sum.by_pid.pid:sum.by_pid.edep","sum.det < 20")
```
