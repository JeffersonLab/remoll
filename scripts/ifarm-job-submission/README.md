# Instruction for Using Analysis Scripts

Before following the next steps, copy reroot from your remoll/build directory into the current location and load the environment.
```
cp <DirectoryContainingRemoll>/remoll/build/reroot .
source /site/12gev_phys/softenv.csh 2.4
```
 
  
## Making standard plots

The macro.py script is provided as an example and should be modified to suit the users needs. For example, you might want to add additional options to the existing set. To view the currently available options:
```
./macro.py --help
```
 
In the example below, we run the rate analysis for a 100 raw root files simultaneously. You can run upto a 1000 jobs simultaneously on ifarm. 

We choose:

          -- home rate      (type of analysis. By default, we only look at primary electrons (hit.trid<=2 for moller and hit.trid<=1 for everything else) on ring 5 main detector (hit.det=28) 900-1060 mm in radius. But you can modify macro.py to change the radial range or add additional macro options to automate it. Changing cuts require modifying rate/analysis.C.)
          
          -s directory where your remoll output files are located. 
          
          -j job submission script directory
          
          -t temporary directory with output logs
          
          -o output directory
          
          -w work directory
          
          -g generator
          
          --time allocated farm time. 30 minutes is enough most of the time for a raw rootfile with 100k primary events.
          
          -r 1-100                 (run range. here we analyze the first 100 root files in our source directory)
          
```
./macro.py --home rate -s ../../V2U.1a.50cm.parallel_V2DSg.9.75cm.parallel -o /volatile/halla/moller12gev/rahmans/work_test/out -j /volatile/halla/moller12gev/rahmans/work_test/jsub -t /volatile/halla/moller12gev/rahmans/work_test/tmp -w /volatile/halla/moller12gev/rahmans/work_test/work --time 00:30:00 -r 1 -g moller 
```


After the analysis finishes running, combine the 100 rootfiles into 1 rootfile.
```
hadd moller.root /volatile/halla/moller12gev/rahmans/work_test/out/moller_* 
```      
Remember to scale your histograms by the inverse of number of analyzed histograms (1/100) before plotting or calculating integrals. This is because combining histograms with hadd is cumulative (unlike trees which are automatically normalized).

Then use the plotting scripts inside the plot directory to look at different plots. The simplest is the radial plot. 
