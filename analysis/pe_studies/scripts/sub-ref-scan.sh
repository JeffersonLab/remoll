#!/bin/bash

if [ "$#" -lt 1 ] ; then
    echo "  ERROR, requires at least one input
    "
    echo "  This script generates geometry from cadp.csv (with options for which ring) and makes a series of analyses at various reflector lengths and angles.
    "
    echo "  This one will also scan across sub-segments of the reflector, and therefore takes much longer and is a more consuming analysis to undertake. The specific technique is to make the reflector only a small segment of it's full self, with the orientation of that subsegment scanned. The non-reflector bounced flux will be counted in each analysis, so it is a good idea to measure the non-reflector signal explicitly and subtract it from each of this analysis for sanity. The segmentation scale is hardcoded in here, but can be changed if you like. Also note that the detector is symmetric about the YZ plane, so only half the segments in the Y direction (width) must be simulated.
    "
    echo "  usage: ./sub-ref-scan.sh reflectivity=0.7 Cerenkov=1.0 Scintillation=1.0 geometry=R5o analysis-pass=1 det-number=540210
        Takes 6 arguments

        Reflectivity of light guide         - default = 0.7
        Cerenkov                            - default = 1.0
        Scintillation                       - default = 1.0
        geometry file name                  - default = \"R5o\"
        pass                                - default = 1
        det number (optional, automatic)    - default = 540210"
    exit
fi

tiltAngle=3.5
refAngle=18.0
thetaSubRef=0.0
phiSubRef=0.0
refLength=80.0
z_p=0.0
z_point=0.0
q_r=0.0
reflectivity=0.7
cerenkov=1
scintillation=1
geom="R5o"
det=540210
pass="1"
if [ "$#" -gt 0 ] ; then
    reflectivity=$1
fi
if [ "$#" -gt 1 ] ; then
    cerenkov=$(printf "%.0f" "$2")
fi
if [ "$#" -gt 2 ] ; then
    scintillation=$(printf "%.0f" "$3")
fi
if [ "$#" -gt 3 ] ; then
    geom="${4}"
fi
if [ "$#" -gt 4 ] ; then
    pass="${5}"
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
missing=0
started=0
if [ "$#" -gt 5 ] ; then
    det="${6}"
fi


defaultName="refScanOut.root"
outputName="refScanOut.root"
name="refScan_0.0_degrees_0.0_L"

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

# On the first pass just do the loop once (started == 0)
# Then, if $pass == 2 the $missing number could be > 0, in which case loop again
while (( $missing!=0 )) || (( $started==0 )) ;
do
    missing=0
    started=1

#for wStep in `seq 0 4`;
# seq 0 to 9 is redundant, as there is a symmetry about the midplane where width = 4.5
for wStep in `seq 0 4`;
do
    for lStep in `seq 0 9`;
    do
        for thetaStepSeq in `seq 0 10`;
        do
            thetaSubRef=$(printf "%.1f" "$(bc -l <<< \(-9.0+\(1.0*$thetaStepSeq*3.0\)\))")
            for phiStepSeq in `seq 0 12`;
            do
                phiSubRef=$(printf "%.1f" "$(bc -l <<< \(-30.0+\(1.0*$phiStepSeq*5.0\)\))")


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
    refAngle=$ref_angle
    refLength=$refL
    tiltAngle=$tilt
    #xPosMax=$(printf "%.2f" "$(bc -l <<< ${pmtR}-10.0)")
    #xPosMin=$(printf "%.2f" "$(bc -l <<< ${qR}+0.5*${qL}+5.0)")
    z_p=$(printf "%.2f" "$(bc -l <<< ${z1}-0.55*$qThick)")
    #echo "$ring $xPosMin $xPosMax z = $z_p starting z = $z1 and second = $z2"
    q_r=$qR
    outString="$outString
$ring,$qR,$qL,$overlap,$qThick,$num2,$num3,$refL,$ref_angle,$lg_angle,$pmtR,$tilt,$pmtRad,$wallThickness,$extraPMT1,$extraPMT2,$z1,$z2"
done < cadp.csv
IFS=$OLDIFS

z_point=$z_p
name="${refAngle}_degrees_${refLength}_L_${reflectivity}_ref_${cerenkov}_cer_${scintillation}_scint"

if [[ "$pass" == "1" ]] ; then
    echo "$outString" > cadp_${geom}_${name}.csv
    perl cadGeneratorV1.pl -F cadp_${geom}_${name}.csv
    perl gdmlGeneratorV1_materials.pl -M detectorMotherP.csv -D parameter.csv -P qe.txt -U UVS_45total.txt -R MylarRef.txt -L ${script_ring} -T _${geom} -i $lStep -j $wStep -t $thetaSubRef -p $phiSubRef
fi
cd -

if [[ "$pass" == "1" ]] ; then
    cp macros/preserve_ref_scans.mac macros/scans_${geom}_${name}.mac
    sed -i 's;'"/remoll/evgen/beam/th -11.5 deg"';'"/remoll/evgen/beam/th -${tiltAngle} deg"';g' macros/scans_${geom}_${name}.mac
    sed -i 's;'"/remoll/evgen/beam/origin 0.0 0.0 0.0 mm"';'"/remoll/evgen/beam/origin -${q_r} 0.0 ${z_point} mm"';g' macros/scans_${geom}_${name}.mac

    sed -i 's;'"/remoll/setgeofile geometry_sandbox/mollerMother_Mainz.gdml"';'"/remoll/setgeofile mollerMother_${geom}.gdml"';g' macros/scans_${geom}_${name}.mac

    sed -i 's;'"/remoll/evgen/beam/rasterRefZ 0.0 mm"';'"/remoll/evgen/beam/rasterRefZ ${z_point} mm"';g' macros/scans_${geom}_${name}.mac

    sed -i 's;'"/process/optical/processActivation Cerenkov false"';'"/process/optical/processActivation Cerenkov ${cer}"';g' macros/scans_${geom}_${name}.mac
    sed -i 's;'"/process/optical/processActivation Scintillation false"';'"/process/optical/processActivation Scintillation ${scint}"';g' macros/scans_${geom}_${name}.mac
    sed -i 's;'"Mainz_0.0_degrees_0.0_x.root"';'"${geom}_${name}.root"';g' macros/scans_${geom}_${name}.mac
fi

tmpFolder="sub-scans/${geom}/out_${geom}_${wStep}_widthStep_${lStep}_lengthStep_${thetaSubRef}_theta_${phiSubRef}_phi_${name}"
if [ ! -d sub-scans ] ; then
    mkdir sub-scans
fi
if [ ! -d sub-scans/${geom} ] ; then
    mkdir sub-scans/${geom}
fi
if [ ! -d $tmpFolder ] ; then
    mkdir $tmpFolder
fi
cd $tmpFolder
if [[ "$pass" == "2" ]] ; then
    if [[ ! -f remollout_${geom}_${name}.pdf ]] ; then 
        cp ../../../../../analysis/bin/pe .
        if [[ ! -f finished.txt ]] ; then
        #if [[ ! -f remollout_${geom}_${name}.root ]] ; then
            echo "Not finished with ${geom} ${name}: ${wStep} widthStep, ${lStep} lengthStep, ${thetaSubRef} theta, ${phiSubRef} phi, will retry analysis"
            let missing=$missing+1
            #echo "$missing"
            # Do missing check instead
            #secondpass="1"
        else
            echo "./pe remollout_${geom}_${name}.root ${det} angle\=${refAngle} reflength\=${refLength} reflectivity\=${reflectivity} cerenkov\=${cerenkov} scintillation\=${scintillation} z_pos\=${z_point} length_step\=$lStep width_step\=$wStep theta\=$thetaSubRef phi\=$phiSubRef"
            ./pe remollout_${geom}_${name}.root ${det} angle\=${refAngle} reflength\=${refLength} reflectivity\=${reflectivity} cerenkov\=${cerenkov} scintillation\=${scintillation} z_pos\=${z_point} length_step\=$lStep width_step\=$wStep theta\=$thetaSubRef phi\=$phiSubRef
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
echo \"
Doing ${geom} ${name} job
\"
while [[ ! -f remollout_${geom}_${name}.root ]] ;
do 
    echo \"Executing ./remoll scans_${geom}_${name}.mac
    \"
    ./remoll scans_${geom}_${name}.mac
    rm core.*
done
echo \"Done, touching finished.txt to signal pass 2 to work
\"
touch finished.txt
    " > runscript_${geom}_${name}.sh
    chmod 755 runscript_${geom}_${name}.sh
fi
#if [[ "$pass" == "1" || "$secondpass" == "1" ]] ; then
if [[ "$pass" == "1" ]] ; then
    qsub runscript_${geom}_${name}.sh
fi
cd -
secondpass="$pass"
            done
        done
    done
done
if [[ "$pass" == "1" ]] ; then
    echo "Pass 1 finished, feel free to rerun immediately with pass 2 (will loop until all jobs finish)"
    missing=0
fi
if (( $missing!=0 )) ; then
    echo "Pass 2 finished, waiting on $missing incomplete jobs for another pass
    "
    echo "Sleeping for 10 minutes"
    sleep 60
    echo "Sleeping for 9 minutes"
    sleep 60
    echo "Sleeping for 8 minutes"
    sleep 60
    echo "Sleeping for 7 minutes"
    sleep 60
    echo "Sleeping for 6 minutes"
    sleep 60
    echo "Sleeping for 5 minutes"
    sleep 60
    echo "Sleeping for 4 minutes"
    sleep 60
    echo "Sleeping for 3  minutes"
    sleep 60
    echo "Sleeping for 2 minutes"
    sleep 60
    echo "Sleeping for 1 minutes"
    sleep 57
    echo "Retrying analysis"
    sleep 3
fi
done
