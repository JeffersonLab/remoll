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
    echo "  usage: ./scan.sh fixed-non-scanned \"variable-to-scan\" min-of-scan (-30) max-of-scan (30) step-size (0.5)
        Takes 11 arguments

        Fixed value of non-scanned variable - default = 0.0
        Variable to scan (\"angle\" or \"x\")   - default = \"angle\"
        Min of scanned variable             - default = -30
        Max of scanned variable             - default = 30
        Step size                           - default = 0.5
        Reflectivity of light guide         - default = 0.9
        Cerenkov                            - default = 1.0
        Scintillation                       - default = 1.0
        z Position of beam origin           - default = -11.0
        geometry file name                  - default = \"Mainz\"
        det number                          - default = 540210"
    exit
fi
fixed="$1"
scanned="angle"
scanMin=-30.0
scanMax=30.0
scanStep=0.5
reflectivity=0.9
cerenkov=1
scintillation=1
#z_pos=-52.0
z_pos=0.0
geom="Mainz"
det=540210
if [ "$#" -gt 1 ] ; then
    scanned="$2"
fi
if [ "$#" -gt 2 ] ; then
    scanMin=$3
fi
if [ "$#" -gt 3 ] ; then
    scanMax=$4
fi
if [ "$#" -gt 4 ] ; then
    scanStep=$5
fi
if [ "$#" -gt 5 ] ; then
    reflectivity=$6
fi
if [ "$#" -gt 6 ] ; then
    cerenkov=$(printf "%.0f" "$7")
fi
if [ "$#" -gt 7 ] ; then
    scintillation=$(printf "%.0f" "$8")
fi
if [ "$#" -gt 8 ] ; then
    z_pos=$9
fi
if [ "$#" -gt 9 ] ; then
    geom="${10}"
fi
if [ "$#" -gt 10 ] ; then
    det="${11}"
fi

angle=0.0
x_pos=0.0

numSteps=$(printf "%.0f" "$(bc -l <<< \(${scanMax}-\(${scanMin}\)\)/$scanStep)")
fixed=$(printf "%.1f" "$(bc -l <<< 1.0*$fixed)")
z_point=0.0

defaultName="qsimout.root"
outputName="qsimout.root"
name="0.0_degrees_0.0_x"

cer="true"
scint="true"
if [[ $cerenkov > 0.0 ]] ;  then
    cer="true"
else
    cer="false"
fi
if [[ $scintillation > 0.0 ]] ;  then
    scint="true"
else
    scint="false"
fi

#tmpFolder="${scanned}_${fixed}"
#if [ ! -d $tmpFolder ] ; then
#    mkdir $tmpFolder
#fi

for currentStep in `seq 0 $numSteps`;
do
    point=$(printf "%.1f" "$(bc -l <<< \($scanMin+\(1.0*$currentStep*$scanStep\)\))")
    if [[ "$scanned" == "angle" ]] ; then
        x_pos=$fixed
        angle=$point
    fi
    if [[ "$scanned" == "x" ]] ; then
        x_pos=$point
        angle=$fixed
    fi
    name="${angle}_degrees_${x_pos}_x_${z_pos}_z_${reflectivity}_ref_${cerenkov}_cer_${scintillation}_scint"
    cp preserve_scans.mac scans_${geom}_${name}.mac
    # FIXME This angle is hardcoded to the qsim-matching case!!
    # 0.1994 = sin(11.5 degrees), $fixed is the distance from 0.0, + is farther towards PMT, further back away from +z axis
    # 0.104528 = sin(6 degrees)
    z_point=$z_pos
    # FIXME doing z_pos as external input only now, for full detector array setting
    #z_point=$(printf "%.1f" "$(bc -l <<< ${z_pos}+\($x_pos*0.1045\))")
    # FIXME z_point and x offset relationships assume the detector is immediately downstream of the origin, and ideally centered at the middle of the lightguide
    #z_point=$(printf "%.1f" "$(bc -l <<< -11.0-\($fixed*0.1994\))")
    sed -i 's;'"/remoll/evgen/beam/th -11.5 deg"';'"/remoll/evgen/beam/th ${angle} deg"';g' scans_${geom}_${name}.mac
    sed -i 's;'"/remoll/evgen/beam/origin 0.0 0.0 0.0 mm"';'"/remoll/evgen/beam/origin ${x_pos} 0.0 ${z_point} cm"';g' scans_${geom}_${name}.mac

    sed -i 's;'"/remoll/setgeofile geometry_Mainz/mollerMother_Mainz.gdml"';'"/remoll/setgeofile mollerMother_${geom}.gdml"';g' scans_${geom}_${name}.mac

    sed -i 's;'"/remoll/evgen/beam/rasterRefZ 0.0 mm"';'"/remoll/evgen/beam/rasterRefZ ${z_point} cm"';g' scans_${geom}_${name}.mac

    sed -i 's;'"/process/optical/processActivation Cerenkov false"';'"/process/optical/processActivation Cerenkov ${cer}"';g' scans_${geom}_${name}.mac
    sed -i 's;'"/process/optical/processActivation Scintillation false"';'"/process/optical/processActivation Scintillation ${scint}"';g' scans_${geom}_${name}.mac
    sed -i 's;'"Mainz_0.0_degrees_0.0_x.root"';'"${geom}_${name}.root"';g' scans_${geom}_${name}.mac
    tmpFolder="scans/$geom/out_${geom}_${name}"
    if [ ! -d scans/${geom} ] ; then
        mkdir scans/${geom}
    fi
    if [ ! -d $tmpFolder ] ; then
        mkdir $tmpFolder
    fi
    cd $tmpFolder
    cp -p ../../../../build/remoll .
    cp -p ../../../../geometry_Mainz/materialsOptical.xml .
    cp -p ../../../../geometry_Mainz/*${geom}.* .
    cp ../../../../geometry_Mainz/matrices_${geom}.xml matrices_${geom}.xml
    sed -i 's;'"2.00214948263953\*eV 0.9"';'"2.00214948263953\*eV ${reflectivity}"';g' matrices_${geom}.xml
    sed -i 's;'"<matrix name=\"Mylar_Surf_Reflectivity\" coldim=\"2\" values=\"2.00214948263954\*eV 0.7"';'"<matrix name=\"Mylar_Surf_Reflectivity\" coldim=\"2\" values=\"2.00214948263954\*eV ${reflectivity} \n7.75389038185113\*eV ${reflectivity}\"/> \n<matrix name=\"Mylar_Surf_Reflectivity_Original\" coldim=\"2\" values=\"2.00214948263954\*eV 0.7"';g' matrices_${geom}.xml
    ##';'"2.00214948263953\*eV ${reflectivity}"';g' matrices_${geom}.xml
    ##sed -i 's;'"7.75389038185112\*eV 0.9"';'"7.75389038185112\*eV ${reflectivity}"';g' matrices_${geom}.xml
    cp ../../../pe .
    mv ../../../scans_${geom}_${name}.mac . 
    ##source /share/apps/root-5.34.36-build/bin/thisroot.sh
    ./remoll scans_${geom}_${name}.mac
    ##cd ../
    ##./build/remoll macros/scans_${geom}_${name}.mac
    ##cd -
    ##source /share/apps/root-6.14.06-build/bin/thisroot.sh
    ##cd $tmpFolder
    ./pe remollout_${geom}_${name}.root ${det} ${angle} ${x_pos} ${reflectivity} ${cerenkov} ${scintillation} ${z_point}
    ##../pe ../../remollout_${geom}_${name}.root 540210 ${reflectivity} ${cerenkov} ${scintillation} ${z_point}
    ##source /share/apps/root-5.34.36-build/bin/thisroot.sh
    convert remollout_${geom}_${name}*.png remollout_${geom}_${name}.pdf
    rm remollout_${geom}_${name}*.png
    rm remollout_${geom}_${name}.root
    rm remollout_${geom}_${name}_PEs_det_${det}.root
    cd -
done
