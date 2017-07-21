#!/bin/bash

dir=`dirname $0`/../..
dir=`readlink -f ${dir}`

rootfiles=${dir}/rootfiles/valgrind
logfiles=${dir}/logfiles/valgrind
macros=${dir}/macros/valgrind
mkdir -p ${logfiles}

# Figure out ROOT valgrind suppression
suppression=""
if [ -f "/etc/root/valgrind-root.supp" ] ; then
	suppression="--suppressions=/etc/root/valgrind-root.supp"
fi
if [ -f "$ROOTSYS/etc/valgrind-root.supp" ] ; then
	suppression="--suppressions=$ROOTSYS/etc/valgrind-root.supp"
fi
echo "Valgrind suppression options: $suppression"

mkdir -p ${rootfiles}
for macro in ${macros}/*.mac ; do
	name=`basename ${macro} .mac`
	echo "Running ${name}..."
	valgrind $suppression build/remoll ${macro} 2>&1 | tee ${logfiles}/${name}.log
done
