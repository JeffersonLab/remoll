# Visualization Macros

The macros in the `vis` directory aid in visualization of simulations.

You can load one of these macros with:
```
/control/execute vis/macro.mac
```

## Starting visualization sessions

### Stored Qt visualization: `SQt.mac` (recommended)

This starts an OpenGL session in the Qt interface using 'stored' or 'retained' mode (the graphics buffers are filled and then control is handed over to OpenGL, allowing optimal performance).

### Immediate Qt visualization: `IQt.mac`

This starts an OpenGL session in the Qt interface using 'immediate' mode (the graphics buffers are shared between the simulation and OpenGL, allowing instant updates).

### X windows visualization: `X.mac`

This starts an OpenGL session in the X11 interface using 'immediate' mode (the graphics buffers are shared between the simulation and OpenGL, allowing instant updates).

### OpenInventor visualization: `OIX.mac`

This starts the OpenInventor interface, if support is present in your geant4 installation.

## Modifying visualization sessions

### Setup default visualization options: `vis.mac`

This sets up some useful colors and visualization options.

### Using a YZ cutaway plane: `cutaway.mac`

This sets up a vertical (Y) cutaway plane along the experiment's axis (Z).
