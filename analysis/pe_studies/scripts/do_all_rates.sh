#!/bin/bash

if [[ ! -d results ]] ; then 
    mkdir results
fi
if [[ ! -f results/bkgd_pe ]] ; then 
    cp -p ../build/bkgd_pe/bkgd_pe results/bkgd_pe
fi
if [[ ! -d results/pdfs ]] ; then 
    mkdir results/pdfs
fi
if [[ ! -f results/scans_R5o.root ]] ; then 
    echo "Error: No results/scans_R5o.root exists, trying to do the hadd now. But please verify files are in the \"results\" folder and try again
    "
    hadd -f scans_R5o.root scans/R5o/out_R5o*/scans.root
    mv scans_R5o.root results/scans_R5o.root
    exit
fi

if [ "$#" -lt 3 ] ; then
    echo "  ERROR, requires at least three inputs, of the names of the ROOT files for sampling rates out of (give the name of the file, not the relative path, sorry)
    "
    echo "  This script will loop over all 6 rings of the MOLLER detector and generate bkgd_pe outputs for moller, epelastic, and epinelastic generators.
    "
    echo "  Warning: This script assumes that there are outputs from \"scripts/produce.sh\" living in the \"results\" folder, as well as results of \"scripts/full-scan.sh\" for each ring R1, R2, R3, R4, R5o (assumes open = trans = closed), and R6, all named as \"scans_R\#.root\"
    "
    echo "  usage: ./do_all_rates.sh mollerRootFileName epelasticRootFileName epinelasticRootFileName [rootFile hadd multiplicity=50.0] [beamCurrent=65.0] [reflectivity=0.7] [cerenkov=1] [scintillation=1]"
    exit
else
    moller=$1
fi
if [ "$#" -ge 2 ] ; then
    epelastic=$2
fi
if [ "$#" -ge 3 ] ; then
    epinelastic=$3
fi
if [ "$#" -ge 4 ] ; then
    repeatCount=$4
fi
if [ "$#" -ge 5 ] ; then
    beamCurrent=$5
fi
if [ "$#" -ge 6 ] ; then
    reflectivity=$6
fi
if [ "$#" -ge 7 ] ; then
    cer=$7
fi
if [ "$#" -ge 8 ] ; then
    scint=$8
fi

cd results

detectorNumberArray=("104010" "204010" "304010" "404010" "504000" "504010" "504020" "604010" "140110" "240110" "340110" "440110" "540100" "540110" "540120" "640110" "140210" "240210" "340210" "440210" "540200" "540210" "540220" "640210")
detectorNameArray=("R1" "R2" "R3" "R4" "R5o" "R5o" "R5o" "R6" "R1" "R2" "R3" "R4" "R5o" "R5o" "R5o" "R6" "R1" "R2" "R3" "R4" "R5o" "R5o" "R5o" "R6")
analysesArray=("$moller" "$epelastic" "$epinelastic")
# Treat open, trans, and closed as identical geometry for now... seems to be true anyway

if [[ ! -f $moller ]] ; then
    echo "Error: This script is working out of $pwd folder, and $moller does not exist in this \"results\" folder, please place it there and try again. Note that the script input should be the root file name, not the path to it (sorry).
    "
fi
if [[ ! -f $epelastic ]] ; then
    echo "Error: This script is working out of $pwd folder, and $epelastic does not exist in this \"results\" folder, please place it there and try again. Note that the script input should be the root file name, not the path to it (sorry).
    "
fi
if [[ ! -f $epinelastic ]] ; then
    echo "Error: This script is working out of $pwd folder, and $epinelastic does not exist in this \"results\" folder, please place it there and try again. Note that the script input should be the root file name, not the path to it (sorry).
    "
fi

for ana in ${analysesArray[@]}; do
    for i in ${!detectorNumberArray[@]}; do
        if [[ -f scans_${detectorNameArray[${i}]}.root ]] ; then 
            ./bkgd_pe $ana ${detectorNumberArray[${i}]} ${detectorNameArray[${i}]} signals $reflectivity $cer $scint $beamCurrent $repeatCount
            convert ${ana%".root"}_signals_${detectorNumberArray[${i}]}*.png ${ana%".root"}_signals_${detectorNumberArray[${i}]}.pdf
            rm *.png
        else
            echo "No scans_${detectorNameArray[${i}]}.root, skipping to try the next ring"
        fi
    done
    for i in ${!detectorNumberArray[@]}; do
        if [[ -f scans_${detectorNameArray[${i}]}.root ]] ; then 
            ./bkgd_pe $ana ${detectorNumberArray[${i}]} ${detectorNameArray[${i}]} backgrounds $reflectivity $cer $scint $beamCurrent $repeatCount
            convert ${ana%".root"}_backgrounds_${detectorNumberArray[${i}]}*.png ${ana%".root"}_backgrounds_${detectorNumberArray[${i}]}.pdf
            rm *.png
        else
            echo "No scans_${detectorNameArray[${i}]}.root, skipping to try the next ring"
        fi
    done
done
cd -
mv results/*.pdf results/pdfs/
