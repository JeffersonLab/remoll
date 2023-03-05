#!/bin/bash

#SBATCH --account=rrg-jmammei
#SBATCH --job-name=remoll-standalone-edep
#SBATCH --time=00:30:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=5
#SBATCH --mem=10G
#SBATCH --output=/lustre03/project/6049248/commonFiles/log/%A_%a.out
#SBATCH --error=/lustre03/project/6049248/commonFiles/log/%A_%a.err

cp -r /lustre03/project/6049248/commonFiles/remoll $TMPDIR

cd $TMPDIR/remoll

python scripts/standalone_edep_studies/generateStandalone.py -c script/standalone_edep_studies/DSconfig.list

./build/remoll macros/run.mac

mv  DSconfig.root /lustre03/project/6049248/commonFiles/out/DSconfig_${SLURM_JOBID}_${SLURM_ARRAY_TASK_ID}.root
