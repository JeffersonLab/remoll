#!/bin/bash
#
dir=$HOME/gitdir/dose_remoll/build/output
i=1
read -p 'Filename Modifier: ' mod
mkdir ../../output/${mod}_10M
mkdir ../../output/Plots_${mod}_10M
while [ $i -le 10 ];
do
  #destination, add in, add original
	mv $dir/out_${mod}$i ../../output/${mod}_10M/
	let i=i+1
done
export DISPLAY=""
/home/cameronc/gitdir/dose_remoll/rad_analysis/rad_dose_10M ${mod}
sleep 5
export DISPLAY="localhost:13.0"
