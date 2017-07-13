#!/bin/bash

dir=`dirname $0`/../..
dir=`readlink -f ${dir}`

rootfiles=${dir}/rootfiles/tests
analysis=${dir}/analysis/tests
logfiles=${dir}/logfiles/tests
macros=${dir}/macros/tests
mkdir -p ${logfiles}


mkdir -p ${rootfiles}
macro=${macros}/$1.mac
if [ ! -f ${macro} ] ; then
	echo "${macro} not found."
	exit 1
fi

name=`basename ${macro} .mac`
echo "Running ${name}..."
build/remoll ${macro} 2>&1 | tee ${logfiles}/${name}.log


mkdir -p ${rootfiles}/analysis
rootfile=${rootfiles}/$1.root
if [ ! -f ${rootfile} ] ; then
	echo "${rootfile} not found."
	exit 1
fi

name=`basename ${rootfile} .root`
for rootmacro in ${analysis}/*.C ; do
	root -q -b -l "${rootmacro}+(\"${rootfiles}\",\"${name}\")" 2>&1 | tee -a ${logfiles}/${name}.log
done
