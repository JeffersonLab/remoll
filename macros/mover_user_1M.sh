#!/bin/bash
#
dir=$HOME/gitdir/dose_remoll/build/output
i=1
read -p 'Filename Modifier: ' mod
mkdir ../../output/${mod}_1M
mkdir ../../output/Plots_${mod}_1M
while [ $i -le 1 ];
do
  #destination, add in, add original
	mv $dir/out_${mod}$i ../../output/${mod}_1M/
	let i=i+1
done
export DISPLAY=""
/home/cameronc/gitdir/dose_remoll/rad_analysis/rad_dose_1M ${mod}
sleep 5
export DISPLAY="localhost:13.0"
