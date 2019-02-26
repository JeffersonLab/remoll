#!/bin/bash

# Exit whenever non-zero exit code occurs
set -euo pipefail

# Determine absolute path of this script
dir=`dirname $0`/../..
dir=`readlink -f ${dir}`

# The test suite can be specified as first argument, default is "load"
suite="${1:-load}"

# A specific remoll macro can be specified as second argument, default is "*.mac"
shopt -s nullglob
geomglob="${2:-mollerMother*.gdml}"

# Set test suite input directories
geom=${dir}/geometry

# Set test suite output directories
logfiles=${dir}/logfiles/tests/geometry/${suite}
mkdir -p ${logfiles}

# Usage information
if [ $# -eq 0 ] ; then
  echo "Usage: `basename $0` [suite = load] [geomglob = mollerMother*.gdml]"
  echo
fi
echo "This script will run a geometry test suite."
echo

for geometry in ${geom}/${geomglob} ; do

  name=`basename ${geometry} .gdml`

  # Announce
  echo "Running remoll geometry test ${suite} on `basename ${geometry} .gdml`..."

  # Run remoll macro
  mkdir -p ${logfiles}
  build/remoll -g ${geometry} -m macros/tests/geometry/${suite}.mac 2>&1 | tee ${logfiles}/${name}.log || exit -1
done
