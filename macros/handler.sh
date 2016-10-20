#!/bin/bash
#
dir=$HOME/gitdir/tracking_env_remoll/build/

for i in `seq 1 20`;
do
	name="out$i"
	rm -r $name
	mkdir $name
	cd $name
	cmake ../../
	make
	qsub ./macros/runscript.sh
	cd ..
done
