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
    echo "  This script assumes a geometry exists in remoll/geometry_sandbox and then computes the PEs for a given detector due to a beam of electrons hitting the detector
    "
    echo "  Assumes you have already created the geometry (in ../../../remoll-detector-generator/) with the correct parameters and name you want before proceding
    "
    echo "  usage: ./scan.sh fixed-non-scanned \"variable-to-scan\" min-of-scan (-30) max-of-scan (30) step-size (0.5)
        Takes 12 arguments

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
        pass                                - default = 1
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
pass="1"
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
if [[ $geom == "R1" ]] ; then
    det=140210
elif [[ $geom == "R2" ]] ; then
    det=240210
elif [[ $geom == "R3" ]] ; then
    det=340210
elif [[ $geom == "R4" ]] ; then
    det=440210
elif [[ $geom == "R5o" ]] ; then
    det=540210
elif [[ $geom == "R5t" ]] ; then
    det=540110
elif [[ $geom == "R5c" ]] ; then
    det=504010
elif [[ $geom == "R6" ]] ; then
    det=640210
fi
if [ "$#" -gt 10 ] ; then
    pass="${11}"
fi
secondpass="$pass"
if [ "$#" -gt 11 ] ; then
    det="${12}"
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
    if [[ "$pass" == "1" ]] ; then
        cp macros/preserve_scans.mac macros/scans_${geom}_${name}.mac
        # FIXME This angle is hardcoded to the qsim-matching case!!
        # 0.1994 = sin(11.5 degrees), $fixed is the distance from 0.0, + is farther towards PMT, further back away from +z axis
        # 0.104528 = sin(6 degrees)
        z_point=$z_pos
        # FIXME doing z_pos as external input only now, for full detector array setting
        #z_point=$(printf "%.1f" "$(bc -l <<< ${z_pos}+\($x_pos*0.1045\))")
        # FIXME z_point and x offset relationships assume the detector is immediately downstream of the origin, and ideally centered at the middle of the lightguide
        #z_point=$(printf "%.1f" "$(bc -l <<< -11.0-\($fixed*0.1994\))")
        sed -i 's;'"/remoll/evgen/beam/th -11.5 deg"';'"/remoll/evgen/beam/th ${angle} deg"';g' macros/scans_${geom}_${name}.mac
        sed -i 's;'"/remoll/evgen/beam/origin 0.0 0.0 0.0 mm"';'"/remoll/evgen/beam/origin ${x_pos} 0.0 ${z_point} cm"';g' macros/scans_${geom}_${name}.mac

        sed -i 's;'"/remoll/setgeofile geometry_sandbox/mollerMother_Mainz.gdml"';'"/remoll/setgeofile mollerMother_${geom}.gdml"';g' macros/scans_${geom}_${name}.mac

        sed -i 's;'"/remoll/evgen/beam/rasterRefZ 0.0 mm"';'"/remoll/evgen/beam/rasterRefZ ${z_point} cm"';g' macros/scans_${geom}_${name}.mac

        sed -i 's;'"/process/optical/processActivation Cerenkov false"';'"/process/optical/processActivation Cerenkov ${cer}"';g' macros/scans_${geom}_${name}.mac
        sed -i 's;'"/process/optical/processActivation Scintillation false"';'"/process/optical/processActivation Scintillation ${scint}"';g' macros/scans_${geom}_${name}.mac
        sed -i 's;'"Mainz_0.0_degrees_0.0_x.root"';'"${geom}_${name}.root"';g' macros/scans_${geom}_${name}.mac
    fi
    tmpFolder="scans/$geom/out_${geom}_${name}"
    if [ ! -d scans ] ; then
        mkdir scans
    fi
    if [ ! -d scans/${geom} ] ; then
        mkdir scans/${geom}
    fi
    if [ ! -d $tmpFolder ] ; then
        mkdir $tmpFolder
    fi
    cd $tmpFolder
    if [[ "$pass" == "2" ]] ; then
        if [[ ! -f remollout_${geom}_${name}.pdf ]] ; then 
            cp ../../../../../analysis/bin/pe .
            if [[ ! -f remollout_${geom}_${name}.root ]] ; then
                echo "Error, no remollout_${geom}_${name}.root file, retrying analysis"
                secondpass="1"
            else
                echo "./pe remollout_${geom}_${name}.root ${det} angle\=${angle} x_pos\=${x_pos} reflectivity\=${reflectivity} cerenkov\=${cerenkov} scintillation\=${scintillation} z_pos\=${z_point}"
                ./pe remollout_${geom}_${name}.root ${det} angle\=${angle} x_pos\=${x_pos} reflectivity\=${reflectivity} cerenkov\=${cerenkov} scintillation\=${scintillation} z_pos\=${z_point}
                convert remollout_${geom}_${name}*.png remollout_${geom}_${name}.pdf
                rm remollout_${geom}_${name}*.png
                rm remollout_${geom}_${name}.root
                rm remollout_${geom}_${name}_PEs_det_${det}.root
                rm remollout_${geom}_${name}_PEs_det_${det}_plots.root
            fi
        fi
    fi
    if [[ "$pass" == "1" ]] ; then
        cp -p ../../../../../build/remoll .
        cp -p ../../../../../bin/remoll.sh .
        cp -p ../../../../../geometry_sandbox/materialsOptical.xml .
        cp -p ../../../../../geometry_sandbox/*${geom}.* .
        cp -p ../../../../../geometry_sandbox/matrices_${geom}.xml matrices_${geom}.xml
        sed -i 's;'"<matrix name=\"Mylar_Surf_Reflectivity\" coldim=\"2\" values=\"2.00214948263954\*eV 0.7"';'"<matrix name=\"Mylar_Surf_Reflectivity\" coldim=\"2\" values=\"2.00214948263954\*eV ${reflectivity} \n7.75389038185113\*eV ${reflectivity}\"/> \n<matrix name=\"Mylar_Surf_Reflectivity_Original\" coldim=\"2\" values=\"2.00214948263954\*eV 0.7"';g' matrices_${geom}.xml
        cp ../../../../../analysis/bin/pe .
        mv ../../../macros/scans_${geom}_${name}.mac . 
        echo "#!/bin/bash
#
#$ -cwd
#$ -j y
#$ -S /bin/bash
source remoll.sh
./remoll scans_${geom}_${name}.mac
        " > runscript_${geom}_${name}.sh
        chmod 755 runscript_${geom}_${name}.sh
    fi
    if [[ "$pass" == "1" || "$secondpass" == "1" ]] ; then
        qsub runscript_${geom}_${name}.sh
    fi
    cd -
    secondpass="$pass"
done
