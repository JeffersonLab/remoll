#!/bin/bash

# Exit whenever non-zero exit code occurs
set -euo pipefail

# Determine absolute path of this script
dir=`dirname $0`/../..
dir=`readlink -f ${dir}`

# The test suite can be specified as first argument, default is "commit"
suite="${1:-commit}"

# Pack log files
tar -czvf remolltest.${suite}.log.tar.gz            --transform 's|logfiles/tests/||g'      logfiles/tests/${suite}/*.log
tar -czvf remolltest.${suite}.analysis.log.tar.gz   --transform 's|logfiles/tests/||g'      logfiles/tests/${suite}/analysis/*.log
# Pack analysis products
tar -czvf remolltest.${suite}.root.tar.gz           --transform 's|rootfiles/tests/||g'     rootfiles/tests/${suite}/*.root
tar -czvf remolltest.${suite}.analysis.png.tar.gz   --transform 's|rootfiles/tests/||g'     rootfiles/tests/${suite}/analysis/*.png
tar -czvf remolltest.${suite}.analysis.root.tar.gz  --transform 's|rootfiles/tests/||g'     rootfiles/tests/${suite}/analysis/*.root
