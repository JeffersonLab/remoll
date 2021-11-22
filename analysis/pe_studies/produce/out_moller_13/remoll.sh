#!/bin/bash
#
# You must source this script.
#   - At your bash prompt type: "source <path>/remoll.sh"
#
# This script assumes that the $REMOLL base directory is one
# level up from the location of this script. The binaries are
# installed in $REMOLL/bin and libraries in $REMOLL/lib.
#

(return 0 2>/dev/null) && sourced=1
if [ -z "$sourced" ] ; then
    echo "This script must be sourced: source $0"
    exit
fi

export REMOLL="/home/jonmott/Remoll/remoll"
export PATH="/home/jonmott/Remoll/remoll/bin":${PATH}
export LD_LIBRARY_PATH="/home/jonmott/Remoll/remoll/lib64":${LD_LIBRARY_PATH}
