#!/bin/bash

export JLAB_VERSION=2.3
export JLAB_ROOT=/jlab
export REMOLL=/jlab/remoll
for entrypoint in /entrypoint.sh /usr/local/bin/entrypoint.sh ; do
  if [ -f $entrypoint ] ; then
    source $entrypoint ""
  fi
done

nevents=100
macro=macros/osg/run.mac
geometry=geometry/mollerMother.gdml

usage() {
  echo "Usage: $0 [-n <nevents=100>] [-m <macro=macros/osg/run.mac>] [-g <geometry=geometry/mollerMother.gdml>]" 1>&2
}

while getopts "hn:m:g:" opt; do
  case ${opt} in
    n )
      nevents=$OPTARG
      ;;
    m )
      macro=$OPTARG
      ;;
    g )
      geometry=$OPTARG
      ;;
    h )
      usage
      exit 0
      ;;
    : )
      usage
      exit 0
      ;;
    * )
      usage
      exit 1
      ;;
  esac
done
shift $((OPTIND -1))

mymacro=`mktemp /tmp/run_XXXXXX.mac`
sed "s|%NEVENTS%|${nevents}|g" ${macro} > ${mymacro}

build/remoll -g ${geometry} -m ${mymacro}
