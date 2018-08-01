#!/bin/bash
#
dir=$HOME/gitdir/dose_remoll/build/output
i=2
read -p 'Filename Modifier: ' mod
cp $dir/out_${mod}1/remoll_beam_tracking_1M_NEW.root $dir/${mod}_Result.root
while [ $i -le 10 ];
do
  #destination, add in, add original
	hadd $dir/${mod}_temp_result.root $dir/out_${mod}$i/remoll_beam_tracking_1M_NEW.root $dir/${mod}_Result.root
	let i=i+1
  mv $dir/${mod}_temp_result.root $dir/${mod}_Result.root
done
