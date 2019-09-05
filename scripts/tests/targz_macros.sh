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
branch=`git rev-parse --abbrev-ref HEAD`

# Pack log files
tar -czvf remolltest.${suite}.log.tar.gz            --transform 's|logfiles/tests/||g'      logfiles/tests/${suite}/${branch}/*.log
tar -czvf remolltest.${suite}.analysis.log.tar.gz   --transform 's|logfiles/tests/||g'      logfiles/tests/${suite}/${branch}/analysis/*.log
# Pack analysis products
tar -czvf remolltest.${suite}.root.tar.gz           --transform 's|rootfiles/tests/||g'     rootfiles/tests/${suite}/${branch}/*.root
tar -czvf remolltest.${suite}.analysis.pdf.tar.gz   --transform 's|rootfiles/tests/||g'     rootfiles/tests/${suite}/${branch}/analysis/*.pdf
tar -czvf remolltest.${suite}.analysis.root.tar.gz  --transform 's|rootfiles/tests/||g'     rootfiles/tests/${suite}/${branch}/analysis/*.root
