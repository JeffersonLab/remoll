#!/bin/bash

geom="R5o"
if [ "$#" -lt 1 ] ; then
    echo "  ERROR, requires at least one input
    "
    echo "  This script will generate a geometry from the csv file you feed with the geometry name you give (smart look up which ring to use, read the script for a list) - it then executes scan.sh once for each step in the length along the light guide you chose. 
    "
    echo "  This is intended to be used to produce look up tables for background/signal comparisons for MOLLER detectors that have already been optimized.
    "
    echo "  usage: ./full-scan.sh \"geometry = R5o\" [analysis pass = 1] [reflectivity = 0.7] [x step size (cm) = 1.0] [angle step size (degrees) = 0.1] [csv file = \"cadp.csv\"]"
    exit
else
    geom=$1
fi

analysis=1 # Submit jobs = 1, analyze jobs = 2
reflectivity=0.8
xPosStep=1.0  # 1 cm steps
angleStep=0.1 # 60 steps per x pos
csv="cadp.csv"
if [ "$#" -gt 1 ] ; then
    analysis=$2
fi
if [ "$#" -gt 2 ] ; then
    reflectivity=$3
fi
if [ "$#" -gt 3 ] ; then
    xPosStep=$4
fi
if [ "$#" -gt 4 ] ; then
    angleStep=$5
fi
if [ "$#" -gt 5 ] ; then
    csv=$6
fi
xPosMin=1040.0 # Reflector begin
xPosMax=1350.0 # PMT begin
angleMin=-6.0 # 3 degrees high
angleMax=0.0  # 3 degrees low
z_pos=0.0
z_p=0.0
lgAngle=6.0

user_ring="5.0"
det=540210

script_ring="5"
if [[ $geom == "R1" ]] ; then
    user_ring="1"
    script_ring="1"
    det=140210
elif [[ $geom == "R2" ]] ; then
    user_ring="2"
    script_ring="2"
    det=240210
elif [[ $geom == "R3" ]] ; then
    user_ring="3"
    script_ring="3"
    det=340210
elif [[ $geom == "R4" ]] ; then
    user_ring="4"
    script_ring="4"
    det=440210
elif [[ $geom == "R5o" ]] ; then
    user_ring="5.0"
    script_ring="5"
    det=540210
elif [[ $geom == "R5t" ]] ; then
    user_ring="5.1"
    script_ring="5trans"
    det=540110
elif [[ $geom == "R5c" ]] ; then
    user_ring="5.2"
    script_ring="5closed"
    det=504010
elif [[ $geom == "R6" ]] ; then
    user_ring="6"
    script_ring="6"
    det=640210
fi

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
    xPosMax=$(printf "%.2f" "$(bc -l <<< ${pmtR}-10.0)")
    xPosMin=$(printf "%.2f" "$(bc -l <<< ${qR}+0.5*${qL}+5.0)")
    lgAngle=$lg_angle
    z_p=$(printf "%.2f" "$(bc -l <<< ${z1}-0.5*$qThick-$refL*s\(\(${ref_angle}-${lg_angle}\)*3.14159/180.0\))")
    echo "$ring $xPosMin $xPosMax z = $z_p starting z = $z1 and second = $z2"
    outString="$outString
$ring,$qR,$qL,$overlap,$qThick,$num2,$num3,$refL,$ref_angle,$lg_angle,$pmtR,$tilt,$pmtRad,$wallThickness,$extraPMT1,$extraPMT2,$z1,$z2"
done < $csv
IFS=$OLDIFS

if [[ ! -f ../../geometry_sandbox/mollerMother_${geom}.gdml ]] ; then 
    if [[ "$analysis" == "1" ]] ; then
        cd ../../../remoll-detector-generator/
        echo "$outString" > cadp_${geom}.csv
        perl cadGeneratorV1.pl -F cadp_${geom}.csv
        perl gdmlGeneratorV1_materials.pl -M detectorMotherP.csv -D parameter.csv -P qe.txt -U UVS_45total.txt -R MylarRef.txt -L ${script_ring} -T _${geom}
        cd -
        mv ../../../remoll-detector-generator/*_${geom}.* ../../geometry_sandbox/
        mv ../../../remoll-detector-generator/cadp_${geom}.csv ../../geometry_sandbox/
    fi
fi

# Rescale numbers to cm from mm
numSteps=$(printf "%.0f" "$(bc -l <<< 0.1*\(${xPosMax}-${xPosMin}\)/$xPosStep)")
echo "$numSteps steps"
for currentStep in `seq 0 $numSteps`;
do
    x_pos=$(printf "%.1f" "$(bc -l <<< -0.1*$xPosMin-1.0*$currentStep*$xPosStep)")
    z_pos=$(printf "%.1f" "$(bc -l <<< 0.1*$z_p-\(-1.0*$x_pos-0.1*$xPosMin\)*s\(${lgAngle}*3.14159/180.0\))")

    echo "$x_pos $z_pos"
    ./scripts/scan.sh $x_pos angle $angleMin $angleMax $angleStep $reflectivity 1 1 $z_pos $geom ${analysis} $det &
done

if [[ ! -d results ]] ; then 
    mkdir results
fi
if [[ ! -f results/bkgd_pe ]] ; then 
    cp ../build/bkgd_pe/bkgd_pe results/bkgd_pe
fi

