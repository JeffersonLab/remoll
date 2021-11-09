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
    echo "  This script generates geometry from cadp.csv - feed it as an argument either \"all\" or the ring name you want to specifically create (\"R5o\", etc.)
    "
    exit
fi


geom=$1
user_ring="5.0"
script_ring="5"
if [[ $geom == "R1" ]] ; then
    user_ring="1"
    script_ring="1"
elif [[ $geom == "R2" ]] ; then
    user_ring="2"
    script_ring="2"
elif [[ $geom == "R3" ]] ; then
    user_ring="3"
    script_ring="3"
elif [[ $geom == "R4" ]] ; then
    user_ring="4"
    script_ring="4"
elif [[ $geom == "R5o" ]] ; then
    user_ring="5.0"
    script_ring="5"
elif [[ $geom == "R5t" ]] ; then
    user_ring="5.1"
    script_ring="5trans"
elif [[ $geom == "R5c" ]] ; then
    user_ring="5.2"
    script_ring="5closed"
elif [[ $geom == "R6" ]] ; then
    user_ring="6"
    script_ring="6"
fi

cd ../../../remoll-detector-generator/
outString=""
OLDIFS=$IFS
IFS=","
while read ring qR qL overlap qThick num2 num3 refL ref_angle lg_angle pmtR tilt pmtRad wallThickness extraPMT1 extraPMT2 z1 z2
do
    if [[ $ring != $user_ring ]] ; then 
        outString="$outString
        $ring,$qR,$qL,$overlap,$qThick,$num2,$num3,$refL,$ref_angle,$lg_angle,$pmtR,$tilt,$pmtRad,$wallThickness,$extraPMT1,$extraPMT2,$z1,$z2"
        continue
    fi
    outString="$outString
    $ring,$qR,$qL,$overlap,$qThick,$num2,$num3,$refL,$ref_angle,$lg_angle,$pmtR,$tilt,$pmtRad,$wallThickness,$extraPMT1,$extraPMT2,$z1,$z2"
done < cadp.csv
IFS=$OLDIFS

echo "$outString" > cadp_${geom}.csv
perl cadGeneratorV1.pl -F cadp_${geom}.csv
perl gdmlGeneratorV1_materials.pl -M detectorMotherP.csv -D parameter.csv -P qe.txt -U UVS_45total.txt -R MylarRef.txt -L ${script_ring} -T _${geom}

cd -
cp -p ../../../remoll-detector-generator/materialsOptical.xml ../../geometry_sandbox
mv ../../../remoll-detector-generator/cadp_${geom}.csv ../../geometry_sandbox
mv ../../../remoll-detector-generator/*${geom}.* ../../geometry_sandbox
