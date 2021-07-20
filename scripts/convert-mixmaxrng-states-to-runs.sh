#!/bin/bash

dir=`dirname $0`
dir=`realpath $dir`

run=0
while [ $# -gt 0 ] ; do
	$dir/convert-mixmaxrng-state.sh $1 > run${run}evt0.rndm
	let run=run+1
	shift
done
