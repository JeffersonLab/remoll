#!/usr/bin/env python

import sys
import os
import subprocess
import math
import time
import argparse
import glob

parser = argparse.ArgumentParser(description="Submit array analysis jobs on ifarm. This script can be further improved.")
parser.add_argument("--home", dest="home", action="store", required= False, default="/home/jmammei/REMOLL/analysis/rate")
parser.add_argument("-s", dest="src_dir", action="store", required=True, help="source folder where simulation directory exists")
parser.add_argument("-j", dest="jsub_dir", action="store", required=True, help="choose directory to write the slurm submission scripts")
parser.add_argument("-t", dest="tmp_dir", action="store", required=True, help="choose directory to write the slurm output logs")
parser.add_argument("-o", dest="out_dir", action="store", required=True, help="choose where to write the output root files")
parser.add_argument("-g", dest="gen", action= "store", required=False, default="moller",  help="choose generator to use. Options are moller, elastic, inelastic, beam, etc.")
parser.add_argument("--time", dest="time", action= "store", required= False, default= "00:20:00", help= "provide the estimated run time. Ex: \"00:25:00\". Usually it is 10 minutes for 1000 moller events.")
parser.add_argument("-r", dest="run_range", action = "store", required=False, default="1", help="provide run range. Example: \"2-5\"")
parser.add_argument("-w", dest="work_dir", action="store", required=False, default="/scratch/slurm", help="Enter location where analysis takes place. Choose: /scratch/slurm")


args=parser.parse_args()
args.home=os.path.realpath(args.home)
args.work_dir=os.path.realpath(args.work_dir)
args.src_dir= os.path.realpath(args.src_dir)
jsub=os.path.realpath(args.jsub_dir)
out=os.path.realpath(args.out_dir)
tmp=os.path.realpath(args.tmp_dir)

if not os.path.exists(args.work_dir):
        os.system("mkdir -p "+args.work_dir)
if not os.path.exists(args.jsub_dir):
        os.system("mkdir -p "+args.jsub_dir)
if not os.path.exists(args.tmp_dir):
        os.system("mkdir -p "+args.tmp_dir)
if not os.path.exists(args.out_dir):
        os.system("mkdir -p "+args.out_dir)


os.chdir(args.src_dir)
list_dir=glob.glob(args.gen+"_*")
print(list_dir)

		
jsubf=open(jsub+"/"+args.gen+".sh", "w")
jsubf.write("#!/bin/bash\n")
jsubf.write("#SBATCH --account=halla\n")
jsubf.write("#SBATCH --partition=production\n")
jsubf.write("#SBATCH --job-name=remollAna\n")
jsubf.write("#SBATCH --time="+args.time+" \n")
jsubf.write("#SBATCH --nodes=1\n")
jsubf.write("#SBATCH --ntasks=1\n")
jsubf.write("#SBATCH --cpus-per-task=1\n")
jsubf.write("#SBATCH --mem=10G\n")
jsubf.write("#SBATCH --output="+tmp+"/"+args.gen+"_%A_%a.out\n")
jsubf.write("#SBATCH --error="+tmp+"/"+args.gen+"_%A_%a.err\n")
jsubf.write("#SBATCH --chdir="+args.work_dir+"\n")
jsubf.write("mkdir -p ${SLURM_ARRAY_JOB_ID}\n")
jsubf.write("cd ${SLURM_ARRAY_JOB_ID}\n")
jsubf.write("mkdir ${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}\n")
jsubf.write("cd ${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}\n")

# jsubf.write("source /site/12gev_phys/softenv.csh 2.4 \n")
for i in range(0, len(list_dir)):
     index= str(list_dir[i]).rfind(".root")
     jsubf.write("FILE["+str(i)+"]=\""+str(list_dir[i][0:index])+"\"\n")

jsubf.write("echo \"Current working directory is `pwd`\"\n")
jsubf.write("cp "+args.home+"/* "+args.work_dir+"/${SLURM_ARRAY_JOB_ID}/${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}\n")
jsubf.write("cp "+args.home+"/../reroot "+args.work_dir+"/${SLURM_ARRAY_JOB_ID}/${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}\n")
jsubf.write("cp "+args.src_dir+"/${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root "+args.work_dir+"/${SLURM_ARRAY_JOB_ID}/${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}\n")
jsubf.write("echo \"Current working directory is `pwd`\"\n")
#jsubf.write("./reroot -q -b radAna.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}\\\",1,1,1)\"\n")
#jsubf.write("./reroot -q -b radAna.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}\\\",2,0,1)\"\n")
#jsubf.write("./reroot -q -b radAna.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}\\\",4,0,1)\"\n")

if ("tgtShldAna" in args.home):
  jsubf.write("./reroot -q -b tgtShldAna.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\",1,1000,1,1)\"\n")
  jsubf.write("./reroot -q -b tgtShldAna.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\",2,1000,0,1)\"\n")
  jsubf.write("./reroot -q -b tgtShldAna.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\",4,1000,0,1)\"\n")
  jsubf.write("mv ${FILE[${SLURM_ARRAY_TASK_ID}-1]}_tgtShldAnaV0.root "+out+"\n"
)
elif ("deconvolution" in args.home):
    if(args.gen =="inelastic"):
      flag=1
    else:
      flag=0
    jsubf.write("./reroot -q -b shieldingAna.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\", "+str(flag)+")\"\n")
    jsubf.write("mv ${FILE[${SLURM_ARRAY_TASK_ID}-1]}_shldAnaV7.root "+out+"\n")
elif ("radAna" in args.home):
  jsubf.write("./reroot -q -b radAna.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\",1,1000,1,1)\"\n")
  jsubf.write("mv ${FILE[${SLURM_ARRAY_TASK_ID}-1]}_radAnaV5.root "+out+"\n")
elif ("tgtFlangeAna" in args.home):
  jsubf.write("./reroot -q -b tgtFlangeRadLevel.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\",1000,1,1)\"\n")
  jsubf.write("mv ${FILE[${SLURM_ARRAY_TASK_ID}-1]}_tgtFlangeRadLevelV0.root "+out+"\n")
elif ("usElectronicsAna" in args.home):
  jsubf.write("./reroot -q -b usElectronicsRad.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\",1000,1,1)\"\n")
  jsubf.write("mv ${FILE[${SLURM_ARRAY_TASK_ID}-1]}_usElectronicsRadV0.root "+out+"\n")
elif ("heShldAna" in args.home):
  jsubf.write("./reroot -q -b heShldAna.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\",1000,1,1)\"\n")
  jsubf.write("mv ${FILE[${SLURM_ARRAY_TASK_ID}-1]}_heShldAnaV0.root "+out+"\n")
elif ("sampipe_backsplash" in args.home):
  jsubf.write("./reroot -q -b analyse.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\",\\\"analysed_${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\", \\\""+args.gen+"\\\", \\\"ring1\\\", \\\"RECREATE\\\")\"\n")
  jsubf.write("./reroot -q -b analyse.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\",\\\"analysed_${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\", \\\""+args.gen+"\\\", \\\"ring2\\\", \\\"UPDATE\\\")\"\n")
  jsubf.write("./reroot -q -b analyse.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\",\\\"analysed_${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\", \\\""+args.gen+"\\\", \\\"ring3\\\", \\\"UPDATE\\\")\"\n")
  jsubf.write("./reroot -q -b analyse.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\",\\\"analysed_${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\", \\\""+args.gen+"\\\", \\\"ring4\\\", \\\"UPDATE\\\")\"\n")
  jsubf.write("./reroot -q -b analyse.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\",\\\"analysed_${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\", \\\""+args.gen+"\\\", \\\"ring5\\\", \\\"UPDATE\\\")\"\n")
  jsubf.write("./reroot -q -b analyse.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\",\\\"analysed_${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\", \\\""+args.gen+"\\\", \\\"ring6\\\", \\\"UPDATE\\\")\"\n")
  jsubf.write("./reroot -q -b analyse.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\",\\\"analysed_${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\", \\\""+args.gen+"\\\", \\\"PMT\\\", \\\"UPDATE\\\")\"\n")
  jsubf.write("mv analysed_${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root "+out+"\n")	
else:
  jsubf.write("./reroot -q -b analyse.C\"(\\\"${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\",\\\"analysed_${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root\\\", \\\""+args.gen+"\\\", 900, 1060, 900, 1060, 900, 1060)\"\n")
  jsubf.write("mv analysed_${FILE[${SLURM_ARRAY_TASK_ID}-1]}.root "+out+"\n")


jsubf.write("cd "+args.work_dir+"\n")
jsubf.write("rm -rf ${SLURM_ARRAY_JOB_ID}/${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}\n")

jsubf.write("echo \"Program remoll finished with exit code $? at: `date`\"\n")
jsubf.close()
	        
                
#subprocess.call("sbatch --array="+args.run_range+" "+jsub+"/"+args.gen+".sh",shell=True)
print("sbatch --array="+args.run_range+" "+jsub+"/"+args.gen+".sh")
		
		

		
	
	
