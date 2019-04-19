#!/bin/bash

echo "Usage: $0 <nevents> [macro = macros/osg/run.mac] [geometry = geometry/mollerMother.gdml]"

nevents=${1:-100}
shift
macro=${1:-macros/osg/run.mac}
shift
geometry=${2:-geometry/mollerMother.gdml}
shift

mymacro=`mktemp /tmp/run_XXXXXX.mac`
sed "s|%NEVENTS%|${nevents}|g" ${macro} > ${mymacro}

build/remoll -g ${geometry} -m ${mymacro}
