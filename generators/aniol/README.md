Note: this directory and included files are obtained from Konrad Aniol's personal page at:
  https://userweb.jlab.org/~aniol/moller/scripts/readme.dat

They represent the code as of 19-Jun-2013 (accessed 16-Jul-2018).

Since the order of output of these codes is strongly correlated with physical parameters
(in particular position in target), it makes sense to shuffle the data before using it.
---

In order to create the code which produces the lambda distribution file (x,y,v,px,py,pz,weight) simply
compile 
```
g++ -o hyperon-gen hyperon-gen.cpp
```

There is an input file used by hyperon-gen.cpp called hyperon-gen.h. You can set the parameters used in the code here.
Select the hyperon line, lambda, sigma plus, sigma zero, which you want to create.

The code produces a file called hyperon_outp.dat. This file should be transferred to ../hyperons/hyperon_outp.dat.

The function hyperon_plot.cxx can be read into an analysis using root. It has sample histograms.
Start root and execute as normal.
```
root .x hyperon_plot.cxx("4")
```

Once the lambda distribution file exists, for example, it can be used to create a hyperon decay file. For example,
the code used to create the helicity dependent pion files from lambda->p+piminus is lambda_decay.cpp.

Since the lambda polarization depends on the helicity of the incident electron, 
edit lambda_decay.cpp to set the electron helicity, h. This is found in the following lines:

```
// setup for lambda->proton+piminus
//double alpha=0.645,Sr=0.75,h=-1.,frac=0.64,mhyp=mLambda,ma=mpion,mb=mproton,sign=-1.;
```

```
g++ -o lambda_decay lambda_decay.cpp
```

The include file, hyperon_decay.h must be adjusted to indicate the desired decay products. 

The function lambda_pi_plot.cxx can be used in a root run to analyze the file created by lambda_decay.cpp
