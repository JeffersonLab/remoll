#!/bin/bash

# Exit whenever non-zero exit code occurs
set -euo pipefail

# Determine absolute path of this script
dir=`dirname $0`/../..
dir=`readlink -f ${dir}`

# The test suite can be specified as first argument, default is "commit"
suite="${1:-commit}"

# A specific remoll macro can be specified as second argument, default is "*.mac"
shopt -s nullglob
macroglob="${2:-*.mac}"

# A specific analysis macro can be specified as second argument, default is "*.root"
shopt -s nullglob
analysisglob="${3:-*.C}"

# The branch name is used to avoid clobbering comparative output
branch=`git rev-parse --abbrev-ref HEAD || echo "HEAD"`

# Set test suite input directories
macros=${dir}/macros/tests/${suite}
analysis1=${dir}/analysis/tests
analysis2=${dir}/analysis/tests/${suite}

# Set test suite output directories
rootfiles=${dir}/rootfiles/tests/${suite}/${branch}
logfiles=${dir}/logfiles/tests/${suite}/${branch}
mkdir -p ${logfiles} ${rootfiles} ${rootfiles}/analysis


# Usage information
if [ $# -eq 0 ] ; then
	echo "Usage: `basename $0` [suite = commit] [macroglob = *.mac] [analysisglob = *.root]"
	echo
fi
echo "This script will run a test suite with files located in the following directories:"
echo " remoll macros in ${macros}/${macroglob}"
echo " analysis macros in ${analysis1}/${analysisglob}"
echo " analysis macros in ${analysis2}/${analysisglob}"
echo


# If test suite is "valgrind"
prefix=""
if [ "$suite" == "valgrind" ] ; then
	prefix="valgrind $suppression"

	# Figure out ROOT valgrind suppression
	suppression=""
	if [ -f "/etc/root/valgrind-root.supp" ] ; then
	        suppression="--suppressions=/etc/root/valgrind-root.supp"
	fi
	if [ -f "$ROOTSYS/etc/valgrind-root.supp" ] ; then
	        suppression="--suppressions=$ROOTSYS/etc/valgrind-root.supp"
	fi
	echo "Valgrind suppression options: $suppression"
	echo
fi


# Run test suite macros as requested
for macro in ${macros}/${macroglob} ; do

	name=`basename ${macro} .mac`

	# Announce
	echo "Running remoll macro `basename ${macro} .mac`..."

	# Run remoll macro
	mkdir -p ${logfiles}
	time $prefix remoll ${macro} 2>&1 | tee ${logfiles}/${name}.log

	# Unit tests do not have output
	if [ "$suite" == "unit" ] ; then
		continue
	fi

	# Determine output file
	if [ -f ${name}.root ] ; then
		mkdir -p ${rootfiles}
		rootfile=${rootfiles}/${name}.root
		mv -f ${name}.root ${rootfile}
	else
	        echo "Output file ${name}.root not found."
	        continue
	fi

	# Analyze output file
	mkdir -p ${logfiles}/analysis
	mkdir -p ${rootfiles}/analysis
	echo "Starting analysis..." | tee ${logfiles}/analysis/${name}.log
	for rootmacro in ${analysis1}/${analysisglob} ${analysis2}/${analysisglob} ; do
		echo "Running analysis macro `basename ${rootmacro} .C`..."
		time reroot -q -b -l "${rootmacro}+(\"${rootfiles}\",\"${name}\")" 2>&1 | tee -a ${logfiles}/analysis/${name}.log
	done

done
