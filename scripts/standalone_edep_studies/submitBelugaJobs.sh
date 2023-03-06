#!/bin/bash

#SBATCH --account=rrg-jmammei
#SBATCH --job-name=remoll-standalone-edep
#SBATCH --time=00:30:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16GB
#SBATCH --output=/lustre03/project/6049248/commonFiles/log/%A_%a.out
#SBATCH --error=/lustre03/project/6049248/commonFiles/log/%A_%a.err

cp -r /lustre03/project/6049248/commonFiles/remoll $TMPDIR

cd $TMPDIR/remoll

python scripts/standalone_edep_studies/generateStandalone.py -c scripts/standalone_edep_studies/USconfig.list

./build/remoll macros/run.mac

mv  USconfig.root /lustre03/project/6049248/commonFiles/out/USconfig1_${SLURM_JOBID}_${SLURM_ARRAY_TASK_ID}.root
