#!/bin/bash

# Takes 6 arguments
# 
# Fixed value of non-scanned length - default = 80.0
# Fixed value of non-scanned depth - default = -1
# Variable to scan ("angle" or "x")   - default = "angle"
# Min of scanned variable             - default = -30
# Max of scanned variable             - default = 30
# Step size                           - default = 0.5

if [ "$#" -lt 1 ] ; then
    echo "  ERROR, requires at least one input
    "
    echo "  This script generates geometry from cadp.csv (with options for which ring) and makes a series of analyses at various reflector lengths and angles
    "
    echo "  usage: ./ref-scan.sh fixed-non-scanned \"variable-to-scan\" min-of-scan (0) max-of-scan (30) step-size (1.0)
        Takes 10 arguments

        Fixed value of non-scanned length - default = 80.0
        Fixed value of non-scanned depth - default = -1
        Variable to scan (\"angle\" or \"x\" or \"d\")   - default = \"angle\"
        Min of scanned variable             - default = 0
        Max of scanned variable             - default = 30
        Step size                           - default = 1.0
        Reflectivity of light guide         - default = 0.7
        Cerenkov                            - default = 1.0
        Scintillation                       - default = 1.0
        geometry file name                  - default = \"R5o\"
        pass                                - default = 1
        det number (optional, automatic)    - default = 540210"
    exit
fi

fixed1="$1"
fixed2="$2"
scanned="angle"
q_r=0.0
scanMin=0.0
scanMax=30.0
scanStep=1.0
reflectivity=0.7
cerenkov=1
scintillation=1
geom="R5o"
pass="1"
det=540210
if [ "$#" -gt 1 ] ; then
    scanned="$3"
fi
if [ "$#" -gt 2 ] ; then
    scanMin=$4
fi
if [ "$#" -gt 3 ] ; then
    scanMax=$5
fi
if [ "$#" -gt 4 ] ; then
    scanStep=$6
fi
if [ "$#" -gt 5 ] ; then
    reflectivity=$7
fi
if [ "$#" -gt 6 ] ; then
    cerenkov=$(printf "%.0f" "$8")
fi
if [ "$#" -gt 7 ] ; then
    scintillation=$(printf "%.0f" "$9")
fi
if [ "$#" -gt 8 ] ; then
    geom="${10}"
fi
if [ "$#" -gt 9 ] ; then
    pass="${11}"
fi
user_ring="5.0"
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
secondpass="$pass"
if [ "$#" -gt 10 ] ; then
    det="${12}"
fi



tiltAngle=3.5
refAngle=15.0 # Updated
refLength=88.5 # Updated
refDepth=-1
z_p=0.0
x_p=0.0
y_p=0.0
numSteps=$(printf "%.0f" "$(bc -l <<< \(${scanMax}-\(${scanMin}\)\)/$scanStep)")
fixed1=$(printf "%.1f" "$(bc -l <<< 1.0*$fixed1)")
fixed2=$(printf "%.1f" "$(bc -l <<< 1.0*$fixed2)")
z_point=0.0
# Added to adjust how the beam hits the quartz. Raster is set to 0.1 to minimize it. x_p and y_p are new as well
raster_x=0.1
raster_y=0.1
xspread=0.0
yspread=0.0

defaultName="refScanOut.root"
outputName="refScanOut.root"
name="refScan_0.0_degrees_0.0_L_0.0_D"

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
        refLength=$fixed1
        refAngle=$point
        refDepth=$fixed2
    fi
    if [[ "$scanned" == "x" ]] ; then
        refLength=$point
        refAngle=$fixed1
        refDepth=$fixed2
    fi
    if [[ "$scanned" == "d" ]] ; then
        refLength=$fixed1
        refAngle=$fixed2
        refDepth=$point
    fi


    name="${refAngle}_degrees_${refLength}_L_${refDepth}_D_${reflectivity}_ref_${cerenkov}_cer_${scintillation}_scint"

    cd ../../../remoll-detector-generator/
    outString=""
    OLDIFS=$IFS
    IFS=","
    while read ring qR qL overlap qThick num2 num3 refL ref_angle refD lg_angle pmtR tilt pmtRad wallThickness extraPMT1 extraPMT2 z1 z2
    do
        if [[ $ring != $user_ring ]] ; then 
            outString="$outString
$ring,$qR,$qL,$overlap,$qThick,$num2,$num3,$refL,$ref_angle,$refD,$lg_angle,$pmtR,$tilt,$pmtRad,$wallThickness,$extraPMT1,$extraPMT2,$z1,$z2"
            continue
        fi
        #xPosMax=$(printf "%.2f" "$(bc -l <<< ${pmtR}-10.0)")
        #xPosMin=$(printf "%.2f" "$(bc -l <<< ${qR}+0.5*${qL}+5.0)")
        z_p=$(printf "%.2f" "$(bc -l <<< ${z1}-0.55*$qThick)")
        x_p=$(printf "%.2f" "$(bc -l <<< ${qL})") #Sets beam x-width to quartz width
        #y_p=$(printf "%.2f" "$(bc -l <<< ${qR}*2.0*3.14159/28.0 + 0.5*${qL}*2.0*3.14159/28.0)") #sets beam y-width
        y_p=$(printf "%.2f" "$(bc -l <<< ${qR}*0.22440+0.5*${qL}*0.22440)") #sets beam y-width
        #echo "$ring $xPosMin $xPosMax z = $z_p starting z = $z1 and second = $z2"
        ref_angle=$refAngle
        refL=$refLength
        refD=$refDepth
        tiltAngle=$tilt
        q_r=$qR
        outString="$outString
$ring,$qR,$qL,$overlap,$qThick,$num2,$num3,$refL,$ref_angle,$refD,$lg_angle,$pmtR,$tilt,$pmtRad,$wallThickness,$extraPMT1,$extraPMT2,$z1,$z2"
    done < cadp.csv
    IFS=$OLDIFS

    z_point=$z_p
    x_spread=$x_p
    y_spread=$y_p

    if [[ "$pass" == "1" ]] ; then
        echo "$outString" > cadp_${geom}_${name}.csv
        perl cadGeneratorV1.pl -F cadp_${geom}_${name}.csv
        perl gdmlGeneratorV1_materials.pl -M detectorMotherP.csv -D parameter.csv -P qe.txt -U UVS_45total.txt -R MylarRef.txt -L ${script_ring} -T _${geom}
    fi
    cd -

    if [[ "$pass" == "1" ]] ; then
        cp macros/preserve_ref_scans.mac macros/scans_${geom}_${name}.mac
        sed -i 's;'"/remoll/evgen/beam/th -11.5 deg"';'"/remoll/evgen/beam/th -${tiltAngle} deg"';g' macros/scans_${geom}_${name}.mac
        sed -i 's;'"/remoll/evgen/beam/origin 0.0 0.0 0.0 mm"';'"/remoll/evgen/beam/origin -${q_r} 0.0 ${z_point} mm"';g' macros/scans_${geom}_${name}.mac

        sed -i 's;'"/remoll/setgeofile geometry_sandbox/mollerMother_Mainz.gdml"';'"/remoll/setgeofile mollerMother_${geom}.gdml"';g' macros/scans_${geom}_${name}.mac

        sed -i 's;'"/remoll/evgen/beam/rasterRefZ 0.0 mm"';'"/remoll/evgen/beam/rasterRefZ ${z_point} mm"';g' macros/scans_${geom}_${name}.mac

        #Setting the beam raster to be minimal

        sed -i 's;'"/remoll/evgen/beam/rasx 25.0 mm"';'"/remoll/evgen/beam/rasx ${raster_x} mm"';g' macros/scans_${geom}_${name}.mac
        sed -i 's;'"/remoll/evgen/beam/rasy 25.0 mm"';'"/remoll/evgen/beam/rasy ${raster_y} mm"';g' macros/scans_${geom}_${name}.mac

        #Setting the beam spread to the full dimension of the quartz
        sed -i 's;'"/remoll/evgen/beam/xspread 25.0 mm"';'"/remoll/evgen/beam/rasx ${x_spread} mm"';g' macros/scans_${geom}_${name}.mac
        sed -i 's;'"/remoll/evgen/beam/xspread 25.0 mm"';'"/remoll/evgen/beam/rasx ${y_spread} mm"';g' macros/scans_${geom}_${name}.mac

        #End of changes to beam

        sed -i 's;'"/process/optical/processActivation Cerenkov false"';'"/process/optical/processActivation Cerenkov ${cer}"';g' macros/scans_${geom}_${name}.mac
        sed -i 's;'"/process/optical/processActivation Scintillation false"';'"/process/optical/processActivation Scintillation ${scint}"';g' macros/scans_${geom}_${name}.mac
        sed -i 's;'"Mainz_0.0_degrees_0.0_x_0.0_d.root"';'"${geom}_${name}.root"';g' macros/scans_${geom}_${name}.mac
    fi

    tmpFolder="ref-scans/$geom/out_${geom}_${name}"
    if [ ! -d ref-scans ] ; then
        mkdir ref-scans
    fi
    if [ ! -d ref-scans/${geom} ] ; then
        mkdir ref-scans/${geom}
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
                echo "./pe remollout_${geom}_${name}.root ${det} angle\=${refAngle} reflength\=${refLength} refdepth\=${refDepth} reflectivity\=${reflectivity} cerenkov\=${cerenkov} scintillation\=${scintillation} z_pos\=${z_point}"
                ./pe remollout_${geom}_${name}.root ${det} angle\=${refAngle} reflength\=${refLength} refdepth\=${refDepth} reflectivity\=${reflectivity} cerenkov\=${cerenkov} scintillation\=${scintillation} z_pos\=${z_point}
                convert remollout_${geom}_${name}*.png remollout_${geom}_${name}.pdf
                rm remollout_${geom}_${name}*.png
                #rm remollout_${geom}_${name}.root
                rm remollout_${geom}_${name}_PEs_det_${det}.root
                rm remollout_${geom}_${name}_PEs_det_${det}_plots.root
            fi
        fi
    fi
    if [[ "$pass" == "1" ]] ; then
        cp -p ../../../../../build/remoll .
        cp -p ../../../../../bin/remoll.sh .
        cp -p ../../../../../../remoll-detector-generator/materialsOptical.xml .
        mv ../../../../../../remoll-detector-generator/cadp_${geom}_${name}.csv . 
        mv ../../../../../../remoll-detector-generator/*${geom}.* .
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
