#!/bin/bash

dir=`dirname $0`/../..
dir=`readlink -f ${dir}`

rootfiles=${dir}/rootfiles/tests
analysis=${dir}/analysis/tests
logfiles=${dir}/logfiles/tests
macros=${dir}/macros/tests
mkdir -p ${logfiles}


mkdir -p ${rootfiles}
for macro in ${macros}/*.mac ; do
	name=`basename ${macro} .mac`
	echo "Running ${name}..."
	build/remoll ${macro} 2>&1 | tee ${logfiles}/${name}.log
done


mkdir -p ${rootfiles}/analysis
for rootfile in ${rootfiles}/*.root ; do
	name=`basename ${rootfile} .root`
	for rootmacro in ${analysis}/*.C ; do
		root -q -b -l "${rootmacro}+(\"${rootfiles}\",\"${name}\")" 2>&1 | tee -a ${logfiles}/${name}.log
	done
done
