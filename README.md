# Simulations for the MOLLER Experiment at Jefferson Lab

[![Build Status](https://github.com/JeffersonLab/remoll/actions/workflows/docker-image.yml/badge.svg?branch=develop)](https://github.com/JeffersonLab/remoll/actions/workflows/docker-image.yml)

## Community

Simulations are coordinated on the [12gevmoller_sim@jlab.org](https://mailman.jlab.org/mailman/listinfo/12gevmoller_sim) mailing list and the [JLab 12 GeV Slack workspace](https://jlab12gev.slack.com) (in particular, the `#moller_simulation` channel). Anyone with a jlab.org email address can join without invitation. Feel free to contact developers there with questions.

## Running simulations

There are several options for running simulations:
- [downloading and compiling the code](README.Compiling.md),
- [using a docker container on any operating system](README.Docker.md),
- [using a singularity container on linux systems](README.Singularity.md).

Simulations can be run in interactive mode when not specifying arguments, or in batch mode when specifying a macro:
```
Usage:
 remoll [-g geometry] [-m macro] [-u session] [-r seed] [-t nthreads] [macro]
```

## Physical argon GEM layers and trigger planes

This branch places four physical GEM layers and two trigger planes in
[`geometry/mollerMother.gdml`](geometry/mollerMother.gdml). Each plane has seven
sectors spaced by `360/7` degrees. The front module contains GEM layers 1 and 2
and the front trigger; the back module contains layers 3 and 4 and the back
trigger. These physical volumes are separate from the scoring planes in
`geometry/mollerParallel.gdml` and from upstream's `GEMTSDetectors.gdml`.

### Change a layer or trigger position

Edit [`geometry/tracking/argonGEMConfig.xml`](geometry/tracking/argonGEMConfig.xml).
Each GEM layer and trigger has its own `Z`, `Radius`, and `Phi` block. Editing a
block moves all seven sectors of that plane together.

| Setting | Meaning | Unit |
| --- | --- | --- |
| `Z` | Absolute hall Z coordinate along the beam axis | mm |
| `Radius` | Radial translation of each sector's placement origin | mm |
| `Phi` | Azimuth of the first sector's placement; also sets the module orientation | degrees |

`Radius` is an offset, not the distance of the active gas from the beam. The
sector shapes already span radii of several hundred mm. Subsequent sectors use
`Phi + (i-1)*360/7`, for `i=1..7`; the GDML rotation is `90 degrees - azimuth`.

| Plane | Z (mm) | Radius offset (mm) | Phi (degrees) |
| --- | ---: | ---: | ---: |
| GEM layer 1 | 19507.9 | 0 | 540/7 |
| GEM layer 2 | 19947.9 | 20 | 540/7 |
| GEM layer 3 | 20521.2 | 30 | 540/7 |
| GEM layer 4 | 20961.2 | 50 | 540/7 |
| Front trigger | 19827.9 | 10 | 540/7 |
| Back trigger | 20841.2 | 40 | 540/7 |

For example, to move layer 2 to Z = 20000 mm, use a 25 mm radial offset, and
rotate its sector pattern to 80 degrees:

```xml
<quantity name="ArgonGEMLayer2Z" type="length" value="20000" unit="mm"/>
<quantity name="ArgonGEMLayer2Radius" type="length" value="25" unit="mm"/>
<quantity name="ArgonGEMLayer2Phi" type="angle" value="80" unit="deg"/>
```

To move only the front trigger, edit `ArgonGEMFrontTriggerZ`,
`ArgonGEMFrontTriggerRadius`, and `ArgonGEMFrontTriggerPhi`. The back trigger has
the equivalent `ArgonGEMBackTrigger*` settings. Trigger positions are independent;
moving a GEM layer does not move a trigger automatically.

Pair centers and the longitudinal air-envelope limits are derived from the GEM
layer positions. Keep layer 1 upstream of layer 2, layer 3 upstream of layer 4,
and each trigger inside its pair's envelope. `ArgonGEMEnvelopeMargin` supplies
30 mm of clearance beyond each pair's outer layer centers. Radial envelope bounds
are defined in the two module GDML files; check containment after large radial
or thickness changes. The parallel-world scoring plane positions remain in
`geometry/positions.xml` and can be adjusted separately if needed.

### What is placed, and which materials are used?

The active module files are
[`trackingDaughter_argon12.gdml`](geometry/tracking/trackingDaughter_argon12.gdml)
and [`trackingDaughter_argon34.gdml`](geometry/tracking/trackingDaughter_argon34.gdml).
Both contain the following placed components:

| Component | Material | Geometry and detector ID |
| --- | --- | --- |
| Pair envelope | `G4_AIR` | Annular air volume containing two GEM layers and one trigger plane |
| GEM sector envelope | `G10` | 50 mm thick annular sector; front radii 635–1350 mm, back radii 655–1400 mm; azimuth 72–107 degrees before placement rotation |
| Frame | `G10` | 8 mm thick clipped annular frame with an active-area cutout; center at local Z = −20 mm; detector **520** |
| Support ribs | `G10` | Three intersecting rectangular bars, each 1 mm thick; center at local Z = −20 mm; no sensitive detector |
| Active gas | `ArCO2` | 16 mm thick clipped annular active area; center at local Z = 0; detector **577** |
| Trigger sector | `Scintillator` | Separate 16 mm thick solid with the same transverse footprint as the gas; detector **578** |

The gas footprint is the intersection of an annular sector (radii 635–1150.63 mm,
start angle 76.75 degrees, span 26.5 degrees) and a 520 × 494.18 mm clipping box
centered at local Y = 872 mm. The frame's outer footprint uses radii 625–1280.63 mm,
a 30-degree span starting at 75 degrees, and a 565 × 574.18 mm clipping box at
local Y = 900 mm. Frame and gas receive a further local Y offset of 60 mm in
the front pair and 40 mm in the back pair; ribs are offset by another 878 mm.

Material definitions use **mass fractions**:

| Material | Density | Composition used by this geometry |
| --- | --- | --- |
| `ArCO2` | 0.0018 g/cm³ at 293.15 K | 80% `G4_Ar`, 20% `G4_CARBON_DIOXIDE` by mass |
| `G10` | 1.3 g/cm³ | 77.3% `G4_SILICON_DIOXIDE`, 14.7% `Epoxy`, 8% `G4_Cl` by mass; defined in `geometry/materials.xml` |
| `Scintillator` | 1.032 g/cm³ | 47.37% C and 52.63% H by mass, with the formula label `C9H10` |
| `G4_AIR` | Geant4 material definition | Standard Geant4 air |

The scintillator's formula label and supplied mass fractions are recorded here
as implemented; they do not represent the same chemical composition. Likewise,
the Ar/CO₂ fractions above are not an 80/20 volume mixture.

This is a physical geometry and material model with sensitive gas and scintillator
volumes. It has no placed Kapton foils, copper electrodes, FR4 readout board,
aluminum frame, or electronics, and it does not model GEM charge amplification.
The G10 sector envelope fills space outside its daughter volumes, so that
material contributes to particle transport. Unused foil, drift-volume, and
alternative-frame definitions have been removed from these two active files.

### Change dimensions or materials

The shared dimensions in `argonGEMConfig.xml` include `ArgonGEMGasThickness`,
`ArgonGEMTriggerThickness`, `ArgonGEMFrameThickness`, and `ArgonGEMModuleThickness`.
Gas and trigger thicknesses can be changed independently. The active-area clipping
box is 40 mm thick; increasing either active thickness beyond 40 mm also requires
increasing that box's Z dimension in both module files. Keep the frame, gas, and
ribs within the sector envelope when changing thicknesses or local offsets.
The frame cutout uses the gas solid, so keep the gas thickness at least as large
as the frame thickness to retain a through-opening in the frame.

Edit the `ArCO2` and `Scintillator` material blocks in **both** module files to
change their density or composition. Edit `geometry/materials.xml` for G10;
that material is shared with other detector systems. Change a component's
`materialref` to assign a different material. The solid and volume names identify
the active gas, trigger, frame, and ribs directly.

Restart remoll after GDML/XML edits; recompilation is unnecessary. From the
repository root, run the existing macro with the executable from your build:

```bash
build/remoll -t 1 -o gem_check.root argon_gem_gas_volume.mac
```

That macro runs 1000 events and enables detailed gas hits for detector 577. To
store the same categories of trigger hits, add these lines after initialization
and before `/run/beamOn`:

```text
/remoll/SD/enable 578
/remoll/SD/detect lowenergyneutral 578
/remoll/SD/detect secondaries 578
/remoll/SD/detect boundaryhits 578
```

Detector 577 is shared by all four gas layers; detector 578 is shared by both
trigger planes. These IDs do not identify individual layers. For a geometry
check, enable `/remoll/geometry/overlapcheck true` and
`/remoll/parallel/overlapcheck true` before `/run/initialize`, and inspect the
reported overlaps. Use a short run before starting a production simulation.

## Analyzing the output

You can access the output file with a regular root installation (it will warn about non-perfect support). A listing of the [output variables](README.variables.md) is available for reference.

To take advantage of dedicated functionality for the data types in the output file, you will need to follow the more detailed [analysis instructions](analysis/README.md). To simplify this, you can also use the helper executable `reroot` which is available wherever `remoll` is available.

## Known issues

### Error: LLVM SYMBOLS ARE EXPOSED TO CLING

You may encounter the following error message when running in graphical mode:
```
 Error in <UnknownClass::InitInterpreter()>: LLVM SYMBOLS ARE EXPOSED TO CLING!
 This will cause problems; please hide them or dlopen() them after the call to
 TROOT::InitInterpreter()!
```
This is a (generally harmless) [known issue](https://github.com/JeffersonLab/remoll/issues/40). A workaround is to run remoll with OpenGL disabled:
```
LIBGL_ALWAYS_INDIRECT=1 build/remoll
```
