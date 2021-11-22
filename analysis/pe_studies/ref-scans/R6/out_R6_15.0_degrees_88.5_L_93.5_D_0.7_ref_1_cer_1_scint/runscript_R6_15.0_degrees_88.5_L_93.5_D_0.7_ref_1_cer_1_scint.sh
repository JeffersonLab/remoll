#!/bin/bash
#
#$ -cwd
#$ -j y
#$ -S /bin/bash
source remoll.sh
./remoll scans_R6_15.0_degrees_88.5_L_93.5_D_0.7_ref_1_cer_1_scint.mac
        
