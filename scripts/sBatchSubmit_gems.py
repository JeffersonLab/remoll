#!/apps/bin/python3
from subprocess import call
import sys, os, time, tarfile
import glob

def main():

    sourceDir = "/work/halla/moller12gev/path/to/your/directory"

    activeDetectors = [28,
                        300, 301, 302, 303, 304, 305, 306,
                        307, 308, 309, 310, 311, 312, 313,
                        314, 315, 316, 317, 318, 319, 320,
                        321, 322, 323, 324, 325, 326, 327]


    generators = ["moller", "epelastic"]
    n_rots = 5
    for generator in generators:
        for nrot in range(n_rots):

            config = "gem_geometry_pos_{0}_{1}".format(nrot, generator)
            outDir = "/volatile/halla/moller12gev/jshirk/remolloutput/"+config
            # this changes the gem geometry gdml file automatically
            rotate_gems(nrot, n_rots, sourceDir)

            if not os.path.exists(outDir):
               os.makedirs(outDir)
            nrEv   = 20000
            nrStart= 0
            nrStop = 500 #(nrStop -nrStart)
            submit  = 1 ## submit is 1 to submit job, submit is 0 to create folder without submitting the jobs

            print('Running ' + str(nrEv*(nrStop - nrStart)) + ' events...')

            jobName=config + '_%03dkEv'%(nrEv/1000)

            ###tar exec+geometry
            make_tarfile(sourceDir)
            call(["cp",sourceDir+"/jobs/default.tar.gz",
                  outDir+"/default.tar.gz"])

            for jobNr in range(nrStart,nrStop): # repeat for jobNr jobs
                print("Starting job setup for jobID: " + str(jobNr))

                jobFullName = jobName + '_%04d'%jobNr
                outDirFull=outDir+"/"+jobFullName
                createMacFile(sourceDir, outDirFull, nrEv, jobNr, activeDetectors, generator)

                createSBATCHfile(sourceDir, outDirFull, jobName, jobNr)

                if submit==1:
                    print("submitting", jobName)
                    call(["sbatch",sourceDir+"/jobs/"+jobName+".sh"])

            print("All done for config ",config," for #s between ",nrStart, " and ", nrStop)


def createMacFile(srcDir, outDirFull, nrEv, jobNr, detectorList, generator):

    if not os.path.exists(outDirFull):
        os.makedirs(outDirFull)

    f=open(outDirFull+"/"+"macro.mac",'w')
    f.write("/remoll/setgeofile geometry/mollerMother.gdml\n")
    f.write("/remoll/physlist/register QGSP_BERT_HP\n")
    f.write("/remoll/physlist/parallel/enable\n")
    f.write("/remoll/parallel/setfile geometry/mollerParallel.gdml\n")
    f.write("/run/initialize\n")
    f.write("/remoll/addfield "+srcDir+"/map_directory/V2U.1a.50cm.parallel.txt\n")
    f.write("/remoll/addfield "+srcDir+"/map_directory/DS_TM1-4_CoilA-G_ll_TM2-4_out3mm.txt\n")
    if generator == "epelastic":
        f.write("/remoll/evgen/set elastic\n")
        f.write("/remoll/evgen/emin 80.0 MeV\n")
        f.write("/remoll/evgen/thmin 0.1 deg\n")
        f.write("/remoll/evgen/thmax 2 deg\n")
    elif generator == "moller":
        f.write("/remoll/evgen/set moller\n")
    else:
        print("generator not added")
        sys.exit(0)
    f.write("/remoll/oldras false\n")
    f.write("/remoll/beamene 11 GeV\n")
    f.write("/remoll/beamcurr 65 microampere\n")
    f.write("/remoll/SD/disable_all\n")


    for det in detectorList:
        f.write("/remoll/SD/enable "+str(det)+"\n")
        f.write("/remoll/SD/detect lowenergyneutral "+str(det)+"\n")
        f.write("/remoll/SD/detect secondaries "+str(det)+"\n")
        f.write("/remoll/SD/detect boundaryhits "+str(det)+"\n")

    f.write("/remoll/kryptonite/enable\n")
    f.write("/process/list\n")
    f.write("/remoll/seed "+str(int(time.clock_gettime(0)) + jobNr)+"\n")
    f.write("/remoll/filename o_moller.root\n")
    f.write("/run/beamOn "+str(nrEv)+"\n")
    f.close()
    return 0

def createSBATCHfile(sourceDir,outDirFull,jobName,jobNr):

    if not os.path.exists(sourceDir+"/jobs"):
        os.makedirs(sourceDir+"/jobs")

    f=open(sourceDir+"/jobs/"+jobName+".sh","w")
    f.write("#!/bin/bash\n")
    f.write("#SBATCH --ntasks=1\n")
    f.write("#SBATCH --job-name="+jobName+'_%03d'%jobNr+"\n")
    f.write("#SBATCH --output="+outDirFull+"/log.out\n")
    f.write("#SBATCH  --error="+outDirFull+"/log.err\n")
    f.write("#SBATCH --partition=production\n")
    f.write("#SBATCH --account=halla\n")
    f.write("#SBATCH --mem-per-cpu=1000\n")
    f.write("cd "+outDirFull+"\n")
    f.write("cp ../default.tar.gz ./\n")
    f.write("tar -zxvf default.tar.gz\n")
    f.write("./remoll macro.mac\n")
    f.write("rm -rf default.tar.gz geometry libremoll.so macro.mac remoll\n")
    f.close()
    return 0

def rotate_gems(nrot, total_rots, sourceDir):
    in_world = False

    rot_linenos = []
    lines = []

    with open("{}/geometry/tracking/GEMDetectors.gdml".format(sourceDir), "r") as gems:
        for i, line in enumerate(gems):
            if "<volume name=\"GEM_world_log\">" in line: 
                in_world = True
            if "<rotation" in line and in_world:
                rot_linenos.append(i)
            lines.append(line)        

    rotation = "\t\t\t\t\t<rotation z=\"(-j)*360./7 - 90 + {0}*360./{1}\" unit=\"deg\"/>\n".format(nrot, (total_rots-1)*7)

    with open("{}/geometry/tracking/GEMDetectors.gdml".format(sourceDir), "w") as gem_new:
        for i, line in enumerate(lines):
            if i in rot_linenos:
                gem_new.write(rotation)
            else:
                gem_new.write(line)


def make_tarfile(sourceDir):
    print("making geometry tarball")
    if os.path.isfile(sourceDir+"/jobs/default.tar.gz"):
        os.remove(sourceDir+"/jobs/default.tar.gz")
    tar = tarfile.open(sourceDir+"/jobs/default.tar.gz","w:gz")
    tar.add(sourceDir+"/build/remoll",arcname="remoll")
    tar.add(sourceDir+"/build/libremoll.so",arcname="libremoll.so")

    # just get every geometry file. Makes the geometry tar a little bigger but dont have to edit it every time anymore

    geometry_files = [y for x in os.walk("{}/geometry/".format(sourceDir)) for y in glob.glob(os.path.join(x[0], '*.*ml'))]

    for file in geometry_files:
        tar.add(file, arcname = file[len(sourceDir):])
    tar.close()

if __name__ == '__main__':
    main()
