#!/bin/bash
#
location=$HOME/gitdir/dose_remoll/build/output/
cd $location

for i in `seq 1 10`;
do
	name="out$i"
	mkdir $name
	cd $name
	qsub ../../macros/runscript.sh
	cd ..
done
