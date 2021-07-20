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
    echo "  This script generates a geometry from cadp.csv (passed as an argument), then pulls the flat-parallel world version of that geometry into the standard position inside remoll/geometry_sandbox, and then generates a flux of particles incident on that geometry for use in later downstream flux simulations
    "
    echo "  usage: ./produce.sh numberRepeat (no path, just name)macroName.mac nameStub cadp.csv"
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
    stub="produce"
fi
if [ "$#" -ge 4 ] ; then
    csv="$4"
else 
    csv="cadp.csv"
fi

cd ../../../remoll-detector-generator/
outString=""
OLDIFS=$IFS
IFS=","
while read ring qR qL overlap qThick num2 num3 refL ref_angle lg_angle pmtR tilt pmtRad wallThickness extraPMT1 extraPMT2 z1 z2
do
    if [[ $outString == "" ]]; then
        echo "test 1 "
        outString="$ring,$qR,$qL,$overlap,$qThick,$num2,$num3,$refL,$ref_angle,$lg_angle,$pmtR,$tilt,$pmtRad,$wallThickness,$extraPMT1,$extraPMT2,$z1,$z2"
    else
        echo "test 2 "
        outString="$outString
$ring,$qR,$qL,$overlap,$qThick,$num2,$num3,$refL,$ref_angle,$lg_angle,$pmtR,$tilt,$pmtRad,$wallThickness,$extraPMT1,$extraPMT2,$z1,$z2"
    fi
done < $csv
IFS=$OLDIFS

echo "$outString" > cadp_${stub}.csv
perl cadGeneratorV1.pl -F cadp_${stub}.csv
perl gdmlGeneratorV1_materials_reflector.pl -M detectorMotherP.csv -D parameter.csv -P qe.txt -U UVS_45total.txt -R MylarRef.txt -T _${stub}
cd -
mv -f ../../../remoll-detector-generator/cadp_${stub}.csv . 
cp -f ../../../remoll-detector-generator/flat_*${stub}.* ../../geometry/flat_segmented_detector_produce.gdml
# Local saved geometry file with the "flat" geometry turned on
cp -f active-SD-mollerParallel.gdml ../../geometry/mollerParallel.gdml

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
    cp -p ../../../../build/remoll .
    cp -p ../../../../bin/remoll.sh .

    cp -p ../../macros/${macro} .
    ln -s ../../../../geometry geometry
    ln -s ../../../../macros macros
    ln -s ../../../../map_directory map_directory
    echo "#!/bin/bash
#
#$ -cwd
#$ -j y
#$ -S /bin/bash
source remoll.sh
./remoll ${macro}
    " > runscript_${currentStep}.sh
    chmod 755 runscript_${currentStep}.sh
    qsub runscript_${currentStep}.sh
    cd -
done

