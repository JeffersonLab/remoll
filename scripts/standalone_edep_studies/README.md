# README

To generate a new config:
```python generateStandalone.py -c <config_file>```

Important variables:

Position and Direction:
For upstream coils, leave x_rotation, y_rotation and z_rotation as 0. To change direction of beam hitting a spot on the coil, you need to modify the beam pos and beam dir variables.

For downstream coils, set x_rotation to 90. Leave the other two as 0. To change direction of beam hitting a spot on the coil, you need to modify the beam pos and beam dir variables.


Charged particle Step Length:
You can check the effect of varying the charged particle step length. This is set using the max_step_limit_charged_particles variable in the config file. Note that this will not set limits on gammas.

Running simulations:
Short jobs (nEvents<2000) can be run from command line:
./build/remoll macros/run.mac

For long jobs (nEvents), adapt and use the given job submission script.
