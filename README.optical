## Optical physics simulations with remoll

Optical physics simulations are slow for two reasons:
- optical physics processes occur with a short mean free path so
  the simulation of primary tracks is slowed down,
- typically a large number of secondary photons are created which
  must subsequently be tracked through the geometry.

For this reason they are not enabled by default in remoll.

To enable optical physics, execute the following macro command
before initialization:
```
/remoll/physlist/optical/enable
```

Furthermore, due to the large number of steps in primary tracks
there will be many hits registered in the sensitive volumes. The
large number of secondaries also gives rise to a large number of
hits in the sensitive volumes. Since we do not always want to
store all hits, you can turn off the storage of optical photon
hits independently of their generation at a volume-by-volume level.

There are two relevant gdml tags to be used inside `<volume>` tags:
- `<auxiliary auxtype="DetType" auxvalue="opticalphoton"/>` will enable
  the storage of all hits by optical photons in this volume,
- `<auxiliary auxtype="DetType" auxvalue="boundaryhits"/>` will restrict
  the storage of hits to only the entering and exiting hits.

You may want to use the `opticalphoton` type for reflection layers or
photo-cathodes, while using `boundaryhits` for the volume where optical
photons are created (e.g. quartz pieces).

### Tweaking the optical simulation

There are various options you can set to tweak the optical physics
simulation.

You can enable or disable specific optical physics processes with the
following macro command:
```
/process/optical/processActivation <process> <flag>
```
where `flag` is `true` or `false` and `process` is one of `Cerenkov`,
`Scintillation`, `OpWLS`, `OpAbsorption`, `OpBoundary`, `OpRayleigh`,
or `OpMieHG`. For simulations where you wish to disentangle the effects
of the Scintillation and Cerenkov processes, you could turn on these
processes one by one.

For many processes there are additional macro commands. Often there is
a macro command with `default` that can be used before initialization,
and one without that can be used after initialization. Several macro
commands have a version that is specific to a process, indicated by the
`<process>` tag below, where `<process>` can be 'cerenkov`, `scintillation`,
`wls`, `absorption`, `boundary`, `rayleigh`, or `mie`.

#### Verbosity
To change the verbosity of optical physics processes (default = 1):
```
/process/optical/verbose <level>
/process/optical/<process>/verbose <level>
```
Since optical physics is a process with short step length, any verbosity
above zero will likely cause lots of output.

#### Track secondaries first
To immediately track secondaries before continuing with the primary track:
```
/process/optical/setTrackSecondariesFirst <process> <flag>
/process/optical/<process>/setTrackSecondariesFirst <process> <flag>
```

#### Cerenkov tweaks
To set a maximum number of Cerenkov photons generated per step (default = 100):
```
/process/optical/[default/]cerenkov/setMaxPhotons <n>
```

To set a maximum change in beta of the primary particle per step (default = 10):
```
/process/optical/[default/]cerenkov/setMaxBetaChange <factor>
```

#### Boundary tweaks
To invoke the sensitive detector routines when a boundary is encountered by
an optical photon:
```
/process/optical/default/boundary/setInvokeSD <flag>
```
This command must be used before initialization.
