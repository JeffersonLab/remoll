#!/bin/bash
#
#$ -cwd
#$ -j y
#$ -S /bin/bash
source remoll.sh
./remoll runexample_moller_parallel.mac
    
