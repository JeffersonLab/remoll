#!/bin/bash
#
dir=$HOME/gitdir/dose_remoll/build/output
i=1
read -p 'Filename Modifier: ' mod
mkdir ../../output/${mod}_10M
while [ $i -le 10 ];
do
  #destination, add in, add original
	mv $dir/out_${mod}$i ../../output/${mod}_10M/
	let i=i+1
done
