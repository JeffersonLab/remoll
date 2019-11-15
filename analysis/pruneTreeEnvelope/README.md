Envelope production:

simulation -> prune tree envelope -> (new statistics optimization 
with FOAM or machine learning or just using rate weighting) -> 
envelope.sh/reader.cc(manual.cc) -> VBA macro -> manual fixes.


Final product results in SolidWorks:

Envelope with many lofts connecting sketches. The problem with 
sketches is that they have lots of points, and not the same number
for all. The VBA macro makes 2 passes over sketches to make lofts, 
and when both passes fail you need to go back over manually and either
delete a sketch and try again or delete points within a sketch.

The automated lofting will produce jagged features, but by editing
the lines in the loft or editing points in the sketch these features
can be reduced.

Running the VBA macro:

Use MakeEnvelopes.swp with obvious steps. Try to add the triangle
simplification update first: Triangle Algorithm: For points 0..n, 
go through all points 1...n-1 and calculate the area of the triangle 
formed by points i-1, i, i+1. Find the point that forms the lowest 
area triangle and delete it. Repeat until the desired number of points 
remain/are deleted.

Generating the envelope points:

Use pruneTreeEnvelope (with cuts set in cut arrays) on ROOT output to
generate a text file that can be read by SolidWorks VBAs.

Simulation:

Create a ROOT simulation output using macros and remoll. See 
runexample_limit_envelope.mac included here for an example of how 
to optimially produce envelope simulation output in remoll
