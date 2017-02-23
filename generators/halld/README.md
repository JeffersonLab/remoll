bggen is the Hall D photoproduction code that Rakitha Beminiwattha <rakithab@jlab.org> imported for 
SoLID background simulations. The modifications are made to allow electroproduction event generation 
with this code. For now Hall D code can only produce events for proton target only.

## Running with the Hall D environment (by Rakitha Beminiwattha, rakithab@jlab.org)
For now full Hall D simulation environment needs to be setup to run this code. I have not put any 
instruction to do this step here.

In particular you can use the Cernlib installation at /site/cernlib/x86_64_rhel6 and CERN_LEVEL 2005

I was following these steps:
https://halldweb1.jlab.org/wiki/index.php/Getting_started_with_GlueX_Software

I installed openmotif as it was suggested under cernlib section in the above link.

## Running without the Hall D environment (by Wouter Deconinck, wdconinc@jlab.org)
You can also compile the code with just cmake and the 32-bit cernlib libraries installed. Even if you
have a 64-bit workstation, you will need to install the 32-bit libraries and compile the code as a
32-bit binary since the cernlib packlib library does not support writing HBOOK files on 64-bit machines.

On RHEL 6.8 (JLab CUE level 2 machines) you should install the following 32-bit packages:

    yum install cernlib.i686 cernlib-devel.i686 cernlib-static.i686 cernlib-utils.i686

You will need to find the cernlib-utils package online on rpmfind.net. It is the package that provides
the helper script 'cernlib' that is required for this process.

Similarly you will need to install 32-bit system libraries:

    yum install lapack.i686 blas.i686 glibc.i686 glibc-devel.i686

Once you have installed these packages you can navigate to bggen/ and run the following commands

    mkdir build && cd build
    cmake ../code
    make

This will create the executable bggen in the directory build.

## Compiling on the JLab ifarm (by Wouter Deconinck, wdconinc@jlab.org)

Setup the cernlib environment for the cernlib libraries (possibly add this to your .login file):

    export CERN=/site/cernlib/x86_64_rhel7
    export CERN_LEVEL=2005
    export PATH=$CERN/$CERN_LEVEL/bin:$PATH

or for you tcsh users:

    setenv CERN /site/cernlib/x86_64_rhel7
    setenv CERN_LEVEL 2005
    set path = ( $path $CERN/$CERN_LEVEL/bin)

The command `cernlib` will allow you to check whether these settings have taken effect correctly.

Proceed as per my insturctions above. The cmake command will call the cernlib command and all will
be good.

## Running the code
Once the code is running there modify beam current and target length for the proton target by 
modifying the FORTRAN file,

    bggen/code/bremsbeam_ini.F

This generator will produce paw ntuple called bggen.nt that will contain the generated events

In the macro file fort.15 modify the random number and no.of events to be generated 

Once the bggen.nt file is generated ntuple can be converted to a root TTree using h2root executable 
provided by the root software package. Once converted use the script scripts/HallD_LH_xs.cc to 
generate LUND files and kinematics plots


## List of files modfied in bggen to enable electro-production

```
A       code/bremsbeam_ini.F
M       code/cohbeam_ini.F
M       code/pythia_h.F
M       code/bg_evec.inc
M       code/lowen_ini.F
M       code/lowen_eve.F
M       code/bg_ntup_ini.F
M       code/pyth_eve.F
M       code/bg_eve.F
M       code/bg_ini.F
```

Most of the changes are minor except code/bremsbeam_ini.F file where main code to allow electroproduction implemented.

Contact information Rakitha Beminiwattha, rakithab@jlab.org
