#!/bin/bash

dir=`dirname $0`/../..
mkdir -p $dir/rootfiles/tests
mkdir -p $dir/log/tests

for i in $dir/macros/tests/*.mac ; do
	echo "Running $i..."
	build/remoll $i 2>&1 | tee $dir/log/tests/${i/mac/log}
done
