#!/usr/bin/env python

import sys
import os
import subprocess
import math
import time
import argparse
import glob

parser = argparse.ArgumentParser(description="Submit array jobs to GREX.")
parser.add_argument("--home", dest="home", action="store", required= False, default="/home/jmammei/REMOLL/analysis/rate")
parser.add_argument("-s", dest="src_dir", action="store", required=True, help="source folder where simulation directory exists")
parser.add_argument("-j", dest="jsub_dir", action="store", required=True, help="choose directory to write the slurm submission scripts")
parser.add_argument("-t", dest="tmp_dir", action="store", required=True, help="choose directory to write the slurm output logs")
parser.add_argument("-o", dest="out_dir", action="store", required=True, help="choose where to write the output root files")
parser.add_argument("-g", dest="gen", action= "store", required=False, default="moller",  help="choose generator to use. Options are moller, elastic, inelastic, beam, etc.")
parser.add_argument("--time", dest="time", action= "store", required= False, default= "00:15:00", help= "provide the estimated run time. Ex: \"00:25:00\". Usually it is 10 minutes for 1000 moller events.")
parser.add_argument("-r", dest="run_range", action = "store", required=False, default="1", help="provide run range. Example: \"2-5\"")
parser.add_argument("--xOff", dest="xOff", action = "store", required=False, default="0", help="provide xoffset for basic ana Example: \"0\"")
parser.add_argument("--yOff", dest="yOff", action = "store", required=False, default="0", help="provide yoffset for basic ana Example: \"0\"")


args=parser.parse_args()


if not os.path.exists(args.jsub_dir):
        os.system("mkdir -p "+args.jsub_dir)
if not os.path.exists(args.tmp_dir):
        os.system("mkdir -p "+args.tmp_dir)
if not os.path.exists(args.out_dir):
        os.system("mkdir -p "+args.out_dir)

args.src_dir=os.path.realpath(args.src_dir)
args.home=os.path.realpath(args.home)
jsub=os.path.realpath(args.jsub_dir)
out=os.path.realpath(args.out_dir)
tmp=os.path.realpath(args.tmp_dir)

os.chdir(args.src_dir)
list_dir=glob.glob(args.gen+"_*")
print(list_dir)


jsubf=open(jsub+"/"+args.gen+".sh", "w")
jsubf.write("#!/bin/bash\n")
jsubf.write("#SBATCH --account=rrg-jmammei\n")
jsubf.write("#SBATCH --job-name=remollAna\n")
jsubf.write("#SBATCH --time="+args.time+" \n")
jsubf.write("#SBATCH --nodes=1\n")
jsubf.write("#SBATCH --ntasks=1\n")
jsubf.write("#SBATCH --cpus-per-task=1\n")
jsubf.write("#SBATCH --mem=4G\n")
jsubf.write("#SBATCH --output="+tmp+"/"+args.gen+"_%A_%a.out\n")
jsubf.write("#SBATCH --error="+tmp+"/"+args.gen+"_%A_%a.err\n")
jsubf.write("source ~/projects/rrg-jmammei/REMOLL/environment/cedar_env_2.0.sh \n")
for i in range(0, len(list_dir)):
     jsubf.write("FILE["+str(i)+"]=\""+str(list_dir[i])+"\"\n")
jsubf.write("cd $TMPDIR\n")
jsubf.write("cp "+args.home+"/* $TMPDIR\n")
jsubf.write("cp "+args.home+"/../reroot $TMPDIR\n")
jsubf.write("cp "+args.src_dir+"/${FILE[${SLURM_ARRAY_TASK_ID}-1]} $TMPDIR\n")
jsubf.write("echo \"Current working directory is `pwd`\"\n")

if ("ciprian_plots" in args.home):
    if(args.gen =="beam"):
      flag=1
    else:
      flag=0
    jsubf.write("./reroot -q -b cut1plot2.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}\\\", "+str(flag)+")\"\n")
    jsubf.write("mv *ciprian_plots.root "+out+"\n")
elif ("deconvolution" in args.home):
    if(args.gen =="inelastic"):
      flag=1
    else:
      flag=0
    jsubf.write("./reroot -q -b shieldingAna.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}\\\", "+str(flag)+")\"\n")
    jsubf.write("mv *shldAnaV7* "+out+"\n")
elif ("basicAna" in args.home):
    jsubf.write("./reroot -q -b basicAna.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}\\\", "+str(args.xOff)+", "+str(args.yOff)+")\"\n")
    jsubf.write("mv *basicAnaV0* "+out+"\n")
else:    
    jsubf.write("./reroot -q -b analyse.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}\\\",\\\"analysed_${FILE[${SLURM_ARRAY_TASK_ID}-1]}\\\", \\\""+args.gen+"\\\", 600, 1900, 600, 1900, 600, 1900)\"\n")
    jsubf.write("mv analysed_${FILE[${SLURM_ARRAY_TASK_ID}-1]} "+out+"\n")
jsubf.write("echo \"Program remoll finished with exit code $? at: `date`\"\n")
jsubf.close()
	        
                
subprocess.call("sbatch --array="+args.run_range+" "+jsub+"/"+args.gen+".sh",shell=True)
		
		

		
	
	
