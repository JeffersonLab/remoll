Glitches:
- /share/library will refuse to show up unless you qlogin to compute-0-0 and ls it directly
- Similarly, you need to log in to compute-0-0 and execute remoll at least once by hand to get all the libraries in place

Once remoll is built add these two lines into bin/remoll.sh to get it to work properly
    export PATH="/share/library":${PATH}
    export LD_LIBRARY_PATH="/share/library/":${LD_LIBRARY_PATH}

== Available Analyses ==

=== Look up table prediction of signal to background ratios in MOLLER detectors ===
* First, produce a flux at the detector array to sample from
\*\* Run scripts/produce.sh for moller, epelastic, and epinelastic - 5 million events (50x 100k jobs) is a good high statistics goal
\*\* Note that mocha.physics.umass.edu often segfaults jobs, and so you may need to redo some of them by hand
\*\* Then hadd moller.root out\_moller\_\*/remollout\_\*.root
\*\* Again for epelastic and epinelastic
\*\* Move these sample files to the folder of choice for doing subsequent analysis
* Run scripts/full-scan.sh to create a look up table of light guide PE yields for the geometry
* Then run bkgd_pe on the flux sample root files you have to generate outputs




Always check for segfault failed runs before proceeding.
Delete core-dump failed rootfiles and when confident with results also delete the root files systematically to save disk space.
