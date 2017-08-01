#!/bin/bash
#
dir=$HOME/gitdir/dose_remoll/build/output
i=2
while [ $i -le 10 ];
do
	hadd $dir/result.root $dir/out$i/remoll_beam_tracking_1M.root $dir/Result.root
	let i=i+1
	mv $dir/result.root $dir/Result.root
done
