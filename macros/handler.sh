#!/bin/bash
#
location=$HOME/gitdir/dose_remoll/build/output/
cd $location

for i in `seq 1 10`;
do
	name="out_AlCan$i"
	mkdir $name
	cd $name
	qsub ../../macros/runscript_AlCan.sh
	cd ..
done
for i in `seq 1 10`;
do
	name="out_movedcol4$i"
	mkdir $name
	cd $name
	qsub ../../macros/runscript_moved_col4.sh
	cd ..
done
for i in `seq 1 10`;
do
	name="out_moreshldsUS$i"
	mkdir $name
	cd $name
	qsub ../../macros/runscript_moreshldsUS.sh
	cd ..
done
for i in `seq 1 10`;
do
	name="out_moreshldsDS$i"
	mkdir $name
	cd $name
	qsub ../../macros/runscript_moreshldsDS.sh
	cd ..
done
