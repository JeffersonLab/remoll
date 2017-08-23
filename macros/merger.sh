#!/bin/bash
#
dir=$HOME/gitdir/dose_remoll/build/output
i=2
while [ $i -le 10 ];
do
  #destination, add in, add original
	hadd $dir/result.root $dir/out_AlCan$i/remoll_beam_tracking_1M_NEW.root $dir/Result.root
	let i=i+1
	mv $dir/result.root $dir/Result.root
done
