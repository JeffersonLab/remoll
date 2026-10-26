#!/bin/bash
#SBATCH --partition=production
#SBATCH --account=halla
#SBATCH --job-name=skim_and_add_PE
#SBATCH --cpus-per-task=4
#SBATCH --nodes=1
#SBATCH --mem=16G
#SBATCH --time=12:00:00
#SBATCH --output=/farm_out/dvalmass/skim_and_add_PE.out
#SBATCH --error=/farm_out/dvalmass/skim_and_add_PE.err
    
##source /site/12gev_phys/softenv.sh 2.4

/w/halla-scshelf2102/moller12gev/dvalmass/remoll/build/reroot -l -b -q './skim_and_add_PE.C'