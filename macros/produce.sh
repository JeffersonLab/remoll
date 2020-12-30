#!/bin/bash

# Takes 5 arguments
# 
# Fixed value of non-scanned variable - default = 0.0
# Variable to scan ("angle" or "x")   - default = "angle"
# Min of scanned variable             - default = -30
# Max of scanned variable             - default = 30
# Step size                           - default = 0.5

if [ "$#" -lt 1 ] ; then
    echo "  ERROR, requires at least one input
    "
    echo "  usage: ./produce.sh numberRepeat macroName.mac"
    exit
fi

num=0
macro="detector_runexample.mac"
if [ "$#" -ge 1 ] ; then
    num=$1
fi
if [ "$#" -ge 2 ] ; then
    macro="$2"
fi
if [ "$#" -ge 3 ] ; then
    stub="$3"
else 
    stub="sim"
fi


for currentStep in `seq 1 $num`;
do
    tmpFolder="produce/out_${stub}_${currentStep}"
    if [ ! -d produce ] ; then
        mkdir produce
    fi
    if [ ! -d $tmpFolder ] ; then
        mkdir $tmpFolder
    fi
    cd $tmpFolder
    cp -p ../../../build/remoll .
    cp -p ../../../bin/remoll.sh .

    cp ../../${macro} .
    ln -s ../../../geometry geometry
    ln -s ../../../macros macros
    ln -s ../../../map_directory map_directory
    echo "#!/bin/bash
#
#$ -cwd
#$ -j y
#$ -S /bin/bash
source remoll.sh
./remoll ${macro}
    " > runscript_${num}.sh
    chmod 755 runscript_${num}.sh
    qsub runscript_${num}.sh
    cd -
done

