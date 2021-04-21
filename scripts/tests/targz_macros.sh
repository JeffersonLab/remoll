#!/bin/bash

shopt -s nullglob

# Exit whenever non-zero exit code occurs
set -euo pipefail

# Determine absolute path of this script
dir=`dirname $0`/../..
dir=`readlink -f ${dir}`

# The test suite can be specified as first argument, default is "commit"
suite="${1:-commit}"

# The branch name is used to avoid clobbering comparative output
branch=`git rev-parse --abbrev-ref HEAD || echo "HEAD"`

# Pack log files
tar -czvf logfiles/remoll.${suite}.${branch}.log.tar.gz           --transform 's|logfiles/tests/||g'  logfiles/tests/${suite}/${branch}/*.log
tar -czvf logfiles/remoll.${suite}.${branch}.analysis.log.tar.gz  --transform 's|logfiles/tests/||g'  logfiles/tests/${suite}/${branch}/analysis/*.log
# Pack analysis products
tar -czvf rootfiles/remoll.${suite}.${branch}.root.tar.gz          --transform 's|rootfiles/tests/||g' rootfiles/tests/${suite}/${branch}/*.root
tar -czvf rootfiles/remoll.${suite}.${branch}.analysis.pdf.tar.gz  --transform 's|rootfiles/tests/||g' rootfiles/tests/${suite}/${branch}/analysis/*.pdf
tar -czvf rootfiles/remoll.${suite}.${branch}.analysis.root.tar.gz --transform 's|rootfiles/tests/||g' rootfiles/tests/${suite}/${branch}/analysis/*.root
